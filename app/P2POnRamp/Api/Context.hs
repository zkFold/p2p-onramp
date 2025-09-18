{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Api.Context where

import           Control.Exception             (throwIO)
import           Crypto.Error                  (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519         as Ed25519
import           Data.Aeson
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig          (GYCoreConfig (..))
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3
import           Prelude
import           Servant

import           ZkFold.Cardano.Crypto.Utils   (eitherHexToKey)
import           ZkFold.Cardano.OffChain.Utils (parseAddress)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..),
                                                onRampCompiled')

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

data OnRampConfig = OnRampConfig
  { orParams           :: OnRampParams'
  , orFiatSKeyFilePath :: FilePath
  , orClaimGracePeriod :: Integer
  } deriving (Show, Generic)

instance FromJSON OnRampConfig

-- | Decode OnRamp parameters from JSON
decodeOnRampParams :: OnRampParams' -> Either String OnRampParams
decodeOnRampParams orParams' = do
  feeAddr     <- parseAddress $ orpFeeAddress orParams'
  feePKBytes' <- eitherHexToKey $ orpFiatPubKeyBytes orParams'
  feeVKey     <- case Ed25519.publicKey feePKBytes' of
                   CryptoPassed vk -> Right vk
                   CryptoFailed _  -> Left "Malformed public key"

  let feePKBytes = toBuiltin @BS.ByteString $ BA.convert feeVKey
      feeVal     = lovelaceValue . Lovelace $ orpFeeValue orParams'

  return $ OnRampParams feeAddr feeVal feePKBytes

-- | Validator script from OnRamp parameters decoded from JSON
onRampValidator :: OnRampParams' -> Either String (GYScript PlutusV3)
onRampValidator orParams' = do
  orParams <- decodeOnRampParams orParams'
  return $ scriptFromPlutus @PlutusV3 $ onRampCompiled' orParams


------------------------- :Context & Setup: -------------------------

-- | Configuration context.
data Ctx = Ctx
  { ctxCoreCfg          :: !GYCoreConfig
  , ctxProviders        :: !GYProviders
  , ctxOnRampParams     :: !OnRampParams'
  , ctxFiatSKeyFilePath :: !FilePath
  , ctxClaimGracePeriod :: !Integer
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


------------------------- :http helpers: -------------------------

badRequest :: String -> IO a
badRequest msg = throwIO $ err400 { errBody = LBS.fromStrict (BSC.pack msg) }

notFoundErr :: String -> IO a
notFoundErr msg = throwIO $ err404 { errBody = LBS.fromStrict (BSC.pack msg) }

internalErr :: String -> IO a
internalErr msg = throwIO $ err500 { errBody = LBS.fromStrict (BSC.pack msg) }

unauthorizedText :: T.Text -> IO T.Text
unauthorizedText t = throwIO $ err401 { errBody = LBS.fromStrict (TE.encodeUtf8 t) }
