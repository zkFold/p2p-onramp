{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Api.Context where

import           Control.Exception             (throwIO)
import           Data.Aeson
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as BL
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

import           P2POnRamp.OrdersDB            (DB)
import           P2POnRamp.Utils               (hexToBuiltin)
import           ZkFold.Cardano.OffChain.Utils (parseAddress)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..),
                                                onRampCompiled')

---------------------------- :Orders DB: ----------------------------

dbFile :: FilePath
dbFile = "orders.json"

readDB :: FilePath -> IO (Either String DB)
readDB path = do
  bytes <- BL.readFile path
  pure (eitherDecode bytes)

-------------------------- :OnRamp Params: --------------------------

data OnRampParams' = OnRampParams'
  { orpFeeAddress      :: String
  , orpFeeValue        :: Integer
  , orpFiatPubKeyBytes :: String
  } deriving (Show, Generic)

instance FromJSON OnRampParams'

-- | Decode OnRamp parameters from JSON
decodeOnRampParams :: OnRampParams' -> Either String OnRampParams
decodeOnRampParams orParams' = do
  feeAddr    <- parseAddress $ orpFeeAddress orParams'
  feePKBytes <- hexToBuiltin $ orpFiatPubKeyBytes orParams'
  let feeVal = lovelaceValue . Lovelace $ orpFeeValue orParams'
  return $ OnRampParams feeAddr feeVal feePKBytes

-- | Minting policy from OnRamp parameters decoded from JSON
onRampPolicy :: OnRampParams' -> Either String (GYScript PlutusV3)
onRampPolicy orParams' = do
  orParams <- decodeOnRampParams orParams'
  let mintScript = scriptFromPlutus @PlutusV3 $ onRampCompiled' orParams
  return mintScript

------------------------- :Context & Setup: -------------------------

-- | Configuration context.
data Ctx = Ctx
  { ctxCoreCfg      :: !GYCoreConfig
  , ctxProviders    :: !GYProviders
  , ctxOnRampParams :: !OnRampParams'
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
