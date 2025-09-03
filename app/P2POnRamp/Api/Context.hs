{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Api.Context where

import           Data.Aeson
import qualified Data.Text            as T
-- import qualified Data.ByteString      as BS
import           GeniusYield.GYConfig (GYCoreConfig (..))
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V1.Value (lovelaceValue)
import           Prelude

import           P2POnRamp.Utils               (hexToBuiltin)
import           ZkFold.Cardano.OffChain.Utils (parseAddress)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..), onRampCompiled)


---------------------------- :Orders DB: ----------------------------

dbFile :: FilePath
dbFile = "orders.json"

-------------------------- :OnRamp Params: --------------------------

data OnRampParams' = OnRampParams'
  { orpFeeAddress      :: String
  , orpFeeValue        :: Integer
  , orpFiatPubKeyBytes :: String
  } deriving (Show, Generic)

instance FromJSON OnRampParams'

-- | Minting policy from OnRamp parameters decoded from JSON
-- onRampPolicy :: OnRampParams' -> Either String (GYBuildScript PlutusV3)
onRampPolicy :: OnRampParams' -> Either String (GYScript PlutusV3)
onRampPolicy orParams' = do
  feeAddr <- parseAddress $ orpFeeAddress orParams'
  feePKBytes <- hexToBuiltin $ orpFiatPubKeyBytes orParams'
  let feeVal = lovelaceValue . Lovelace $ orpFeeValue orParams'
      orParams = OnRampParams feeAddr feeVal feePKBytes
      mintScript = scriptFromPlutus @PlutusV3 $ onRampCompiled orParams
  -- return $ GYMintScript @PlutusV3 mintScript
  return mintScript

------------------------- :Context & Setup: -------------------------

-- | Configuration context.
data Ctx = Ctx
  { ctxCoreCfg      :: !GYCoreConfig
  , ctxProviders    :: !GYProviders
  , ctxOnRampParams :: !OnRampParams'
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
