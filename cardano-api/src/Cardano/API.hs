-- | This module provides a library interface for interacting with Cardano as
-- a user of the system.
--
-- It is intended to be the complete API covering everything but without exposing
-- constructors that reveal any lower level types.
--
-- In the interest of simplicity it glosses over some details of the system.
-- Most simple tools should be able to work just using this interface,
-- however you can go deeper and expose the types from the underlying libraries
-- using "Cardano.Api.Byron" or "Cardano.Api.Shelley".
--

module Cardano.API (
    HasTypeProxy(..),
    AsType(..),
    -- * Cryptographic key interface
    -- $keys
    Key,
    VerificationKey,
    SigningKey,
    getVerificationKey,
    verificationKeyHash,
    castVerificationKey,
    castSigningKey,

    -- ** Generating keys
    generateSigningKey,
    deterministicSigningKey,
    deterministicSigningKeySeedSize,

    -- ** Hashes
    -- | In Cardano most keys are identified by their hash, and hashes are
    -- used in many other places.
    Hash,
    castHash,

    -- * Payment addresses
    -- | Constructing and inspecting normal payment addresses
    Address,
    NetworkId(Mainnet,Testnet),
    -- * Byron addresses
    makeByronAddress,
    ByronKey,
    -- * Shelley addresses
    makeShelleyAddress,
    PaymentCredential,
    StakeAddressReference,
    PaymentKey,
    PaymentExtendedKey,

    -- * Stake addresses
    -- | Constructing and inspecting stake addresses
    StakeAddress,
    StakeCredential,
    makeStakeAddress,
    StakeKey,
    StakeExtendedKey,

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody,
    TxId,
    getTxId,
    TxIn(TxIn),
    TxOut(TxOut),
    TxIx(TxIx),
    TTL,
    TxFee,
    Lovelace(Lovelace),
    makeByronTransaction,
    makeShelleyTransaction,
    SlotNo,
    TxExtraContent,
    txExtraContentEmpty,
    Certificate,

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx,
    getTxBody,
    getTxWitnesses,

    -- ** Signing in one go
    signByronTransaction,
    signShelleyTransaction,

    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    Witness,
    makeByronKeyWitness,
    ShelleyWitnessSigningKey,
    makeShelleyKeyWitness,
    makeShelleyBootstrapWitness,
    makeShelleyScriptWitness,

    -- * Fee calculation
    transactionFee,
    estimateTransactionFee,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    TxMetadata(TxMetadata),
    TxMetadataValue,
    makeTransactionMetadata,

    -- * Registering stake address and delegating
    -- | Certificates that are embedded in transactions for registering and
    -- unregistering stake address, and for setting the stake pool delegation
    -- choice for a stake address.
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressDelegationCertificate,

    -- * Registering stake pools
    -- | Certificates that are embedded in transactions for registering and
    -- retiring stake pools. This includes updating the stake pool parameters.
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters,
    StakePoolRelay,
    StakePoolMetadataReference,

    -- ** Stake pool off-chain metadata
    StakePoolMetadata,
    validateAndHashStakePoolMetadata,
    StakePoolMetadataValidationError,

    -- * Scripts
    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.
    -- Shelley supports multi-signatures via scripts.
    Script,

    -- ** Script addresses
    -- | Making addresses from scripts.
    scriptHash,

    -- ** Multi-signature scripts
    -- | Making multi-signature scripts.
    MultiSigScript,
    makeMultiSigScript,

    -- * Serialisation
    -- | Support for serialising data in JSON, CBOR and text files.

    -- ** CBOR
    SerialiseAsCBOR,
    ToCBOR,
    FromCBOR,
    serialiseToCBOR,
    deserialiseFromCBOR,

    -- ** JSON
    ToJSON,
    FromJSON,
    serialiseToJSON,
    deserialiseFromJSON,

    -- ** Bech32
    SerialiseAsBech32,
    serialiseToBech32,
    deserialiseFromBech32,
    deserialiseAnyOfFromBech32,
    Bech32DecodeError,

    -- ** Addresses
    -- | Address serialisation is (sadly) special
    SerialiseAddress,
    serialiseAddress,
    deserialiseAddress,

    -- ** Raw binary
    -- | Some types have a natural raw binary format.
    SerialiseAsRawBytes,
    serialiseToRawBytes,
    deserialiseFromRawBytes,
    serialiseToRawBytesHex,
    deserialiseFromRawBytesHex,

    -- ** Text envelope
    -- | Support for a envelope file format with text headers and a hex-encoded
    -- binary payload.
    HasTextEnvelope,
    TextEnvelope,
    TextEnvelopeType,
    TextEnvelopeDescr,
    TextEnvelopeError,
    serialiseToTextEnvelope,
    deserialiseFromTextEnvelope,
    readFileTextEnvelope,
    writeFileTextEnvelope,
    readTextEnvelopeFromFile,
    readTextEnvelopeOfTypeFromFile,
    -- *** Reading one of several key types
    FromSomeType,
    deserialiseFromTextEnvelopeAnyOf,
    readFileTextEnvelopeAnyOf,

    -- * Errors
    Error,
    throwErrorAsException,
    FileError,

    -- * Node interaction
    -- | Operations that involve talking to a local Cardano node.

    -- ** Queries
    -- ** Submitting transactions

    -- ** Low level protocol interaction with a Cardano node
    connectToLocalNode,
    LocalNodeConnectInfo,
    localNodeSocketPath,
    localNodeNetworkId,
    localNodeConsensusMode,
    NodeConsensusMode,
    LocalNodeClientProtocols,
    localChainSyncClient,
    localTxSubmissionClient,
    localStateQueryClient,
    nullLocalNodeClientProtocols,
--  connectToRemoteNode,

    -- *** Chain sync protocol
    ChainSyncClient,
    runChainSyncClient,

    -- *** Local tx submission
    LocalTxSubmissionClient,
    runLocalTxSubmissionClient,
    submitTxToNodeLocal,

    -- *** Local state query
    LocalStateQueryClient,
    runLocalStateQueryClient,
    queryNodeLocalState,

    -- * Node operation
    -- | Support for the steps needed to operate a node

    -- ** Operational certificates
    OperationalCertificate,
    OperationalCertificateIssueCounter,
    OperationalCertIssueError,
    issueOperationalCertificate,

    -- * Genesis file
    -- | Types and functions needed to inspect or create a genesis file.
    GenesisKey,
    GenesisExtendedKey,
    GenesisDelegateKey,
    GenesisDelegateExtendedKey,
    GenesisUTxOKey,
    genesisUTxOPseudoTxIn,

    -- * Special transactions
    -- | There are various additional things that can be embedded in a
    -- transaction for special operations.
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,

    NetworkMagic,
    makeShelleyUpdateProposal,
  ) where

import           Cardano.Api.Typed
