{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Api.Context where

import           Data.Aeson
import qualified Data.Text            as T
import qualified Data.ByteString      as BS
import           GeniusYield.GYConfig (GYCoreConfig (..))
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V3   (BuiltinByteString)
import           Prelude

-- import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..))

------------------------- :Context & Setup: -------------------------

-- | Configuration context.
data Ctx = Ctx
  { ctxCoreCfg       :: !GYCoreConfig
  , ctxProviders     :: !GYProviders
--  , ctxPartialParams :: !(BuiltinByteString -> OnRampParams)
  }

------------------------- :Unsigned response: -------------------------

data UnsignedTxResponse = UnsignedTxResponse
  { urspTxBodyHex :: !T.Text           -- ^ Unsigned transaction cbor.
  , urspTxFee     :: !(Maybe Integer)  -- ^ Tx fees.
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Construct `UnsignedTxResponse` return type for our endpoint given the transaction body.
unSignedTxWithFee :: GYTxBody -> UnsignedTxResponse
unSignedTxWithFee txBody = UnsignedTxResponse
  { urspTxBodyHex  = T.pack . txToHex $ unsignedTx txBody
  , urspTxFee      = Just $ txBodyFee txBody
  }

------------------------- :own address: -------------------------

-- | Own addresses input.
data OwnAddresses = OwnAddresses { oaUsedAddrs :: ![GYAddress] }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Return own address as text.
data OwnPubKeyBytes = OwnPubKeyBytes { oaPubKeyBytes :: !T.Text }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

-- | Handle to get own address.
handleOwnAddr :: OwnAddresses -> IO OwnPubKeyBytes
handleOwnAddr OwnAddresses{..} = pure . OwnPubKeyBytes . addressToText $ head oaUsedAddrs
