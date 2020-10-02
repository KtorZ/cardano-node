
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracing.Metrics
  ( KESMetricsData (..)
  , MaxKESEvolutions (..)
  , OperationalCertStartKESPeriod (..)
  , HasKESMetricsData (..)
  , ForgingStats (..)
  , ForgeThreadStats (..)
  , mapForgingCurrentThreadStats
  , mapForgingStatsTxsProcessed
  , mkForgingStats
  ) where

import           Prelude (id)
import           Cardano.Prelude hiding (All, (:.:))

import           Cardano.Crypto.KES.Class (Period)
import           Control.Arrow ((&&&))
import           Control.Concurrent (myThreadId)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (All, (:.:)(..), hcmap, K (..), hcollapse)
import           Ouroboros.Consensus.Block (ForgeStateInfo, SlotNo)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapForgeStateInfo (..))
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (PerEraForgeStateInfo (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node ()
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import qualified Ouroboros.Consensus.Util.OptNP as OptNP
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))


-- | KES-related data to be traced as metrics.
data KESMetricsData
  = NoKESMetricsData
  -- ^ The current protocol does not support KES.
  | TPraosKESMetricsData
      !Period
      -- ^ The current KES period of the hot key, relative to the start KES
      -- period of the operational certificate.
      !MaxKESEvolutions
      -- ^ The configured max KES evolutions.
      !OperationalCertStartKESPeriod
      -- ^ The start KES period of the configured operational certificate.

-- | The maximum number of evolutions that a KES key can undergo before it is
-- considered expired.
newtype MaxKESEvolutions = MaxKESEvolutions Word64

-- | The start KES period of the configured operational certificate.
newtype OperationalCertStartKESPeriod = OperationalCertStartKESPeriod Period

class HasKESMetricsData blk where
  -- Because 'ForgeStateInfo' is a type family, we need a Proxy argument to
  -- disambiguate.
  getKESMetricsData :: Proxy blk -> ForgeStateInfo blk -> KESMetricsData

  -- Default to 'NoKESMetricsData'
  getKESMetricsData _ _ = NoKESMetricsData

instance HasKESMetricsData (ShelleyBlock c) where
  getKESMetricsData _ forgeStateInfo =
      TPraosKESMetricsData currKesPeriod maxKesEvos oCertStartKesPeriod
    where
      HotKey.KESInfo
        { kesStartPeriod = KESPeriod startKesPeriod
        , kesEvolution = currKesPeriod
        , kesEndPeriod = KESPeriod endKesPeriod
        } = forgeStateInfo

      maxKesEvos = MaxKESEvolutions $
          fromIntegral $ endKesPeriod - startKesPeriod

      oCertStartKesPeriod = OperationalCertStartKESPeriod startKesPeriod

instance HasKESMetricsData ByronBlock where

instance All HasKESMetricsData xs => HasKESMetricsData (HardForkBlock xs) where
  getKESMetricsData _ forgeStateInfo =
        combineKESMetrics
      . hcollapse
      . hcmap (Proxy @ HasKESMetricsData) getOne
      . OptNP.toNP
      . getPerEraForgeStateInfo
      $ forgeStateInfo
    where
      getOne :: forall blk. HasKESMetricsData blk
             => (Maybe :.: WrapForgeStateInfo) blk
             -> K KESMetricsData blk
      getOne (Comp mbWrapForgeStateInfo) = K $
          case unwrapForgeStateInfo <$> mbWrapForgeStateInfo of
            Nothing -> NoKESMetricsData
            Just oneForgeStateInfo ->
              getKESMetricsData (Proxy @ blk) oneForgeStateInfo

      -- Multiple eras can have 'KESMetricsData', e.g., Shelley and Shelley +
      -- native assets. We don't know which era we're in, so we have to make a
      -- choice: we choose the last era's 'KESMetricsData'.
      --
      -- TODO this might need revision when we have updated everything for the
      -- first hard fork after Shelley, but before it actually happened. At
      -- that moment, we'll trace the KESMetricsData of the upcoming era,
      -- instead of the KESMetricsData of the current Shelley era.
      combineKESMetrics :: [KESMetricsData] -> KESMetricsData
      combineKESMetrics = lastDef NoKESMetricsData

-- | This structure stores counters of blockchain-related events,
--   per individual forge thread.
--   These counters are driven by traces.
data ForgingStats
  = ForgingStats
  { fsTxsProcessedNum               :: !(IORef Word64)
  , fsState                         :: !(IORef (Map ThreadId ForgeThreadStats))
  }

data ForgeThreadStats = ForgeThreadStats
  { ftsBlocksForgedNum              :: !Word64
  , ftsNodeCannotForgeNum           :: !Word64
  , ftsNodeIsLeaderNum              :: !Word64
  , ftsSlotsMissedNum               :: !Word64
  , ftsLeadershipChecksInitiated    :: !Word64
  , ftsFirstSlot                    :: !(Maybe SlotNo)
  }

mkForgingStats :: IO ForgingStats
mkForgingStats =
  ForgingStats
    <$> newIORef 0
    <*> newIORef mempty

mapForgingStatsTxsProcessed ::
     ForgingStats
  -> (Int -> Int)
  -> IO Int
mapForgingStatsTxsProcessed fs f =
  atomicModifyIORef' (fsTxsProcessedNum fs)
    (fromIntegral &&& id . f . fromIntegral)

mapForgingCurrentThreadStats ::
     ForgingStats
  -> (ForgeThreadStats -> ForgeThreadStats)
  -> IO ForgeThreadStats
mapForgingCurrentThreadStats fs f = do
  tid <- myThreadId
  atomicModifyIORef' (fsState fs) $
    (id &&& (Map.! tid)) -- We're guaranteed to have the tid by the next line,
                         --  so Map.! is morally acceptable.
    . Map.alter (Just . f . fromMaybe initialStats) tid
 where
   initialStats = ForgeThreadStats 0 0 0 0 0 Nothing
