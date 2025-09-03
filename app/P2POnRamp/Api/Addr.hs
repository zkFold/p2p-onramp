{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}


module P2POnRamp.Api.Addr where

import           Control.Exception             (throwIO)
-- import           Control.Monad.IO.Class        (liftIO)
-- import           Control.Monad                 (when)
-- import           Data.Aeson                    (FromJSON (..), ToJSON (..), object, withObject, (.=), (.:))
-- import qualified Data.Aeson                    as Aeson
-- import qualified Data.ByteArray                as BA
-- import           Data.ByteString               (ByteString)
-- import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
-- import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                     (Text)
-- import qualified Data.Text                     as T
-- import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig          (GYCoreConfig (..))
import           GeniusYield.Types
-- import           GHC.Generics                  (Generic)
-- import           PlutusLedgerApi.V1.Value      (lovelaceValue)
-- import           PlutusLedgerApi.V3            as V3
-- import           PlutusTx                      (makeIsDataIndexed)
-- import           PlutusTx.Prelude              (verifyEd25519Signature)
import           Servant
-- import           System.FilePath               ((</>))

-- CBOR
-- import           Codec.CBOR.Read               (deserialiseFromBytes)
-- import           Codec.CBOR.Term               (Term(..), encodeTerm, decodeTerm)
-- import           Codec.CBOR.Write              (toStrictByteString)

-- Ed25519 (cryptonite)
-- import           Crypto.Error                  (CryptoFailable(..))
-- import qualified Crypto.PubKey.Ed25519         as Ed25519
import           Prelude

import           P2POnRamp.Api.Context         (Ctx (..), onRampPolicy)
-- import           P2POnRamp.OrdersDB            (Order (..), SellerInfo (..), createOrder)
-- [maybe also remove hexToBuiltin']
-- import           P2POnRamp.Utils               (hexToBuiltin')
-- import           ZkFold.Cardano.Crypto.Utils   (eitherHexToKey)
-- import           ZkFold.Cardano.OffChain.Utils (dataToJSON)
-- import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
-- import           ZkFold.Cardano.P2P.Example (paymentInfoHashEx1)
-- import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))


--------------------------------------------------------------------------------
-- Helpers

badRequest :: String -> IO a
badRequest msg = throwIO $ err400 { errBody = LBS.fromStrict (BSC.pack msg) }

-- internalErr :: String -> IO a
-- internalErr msg = throwIO $ err500 { errBody = LBS.fromStrict (BSC.pack msg) }

-- unauthorizedText :: Text -> IO Text
-- unauthorizedText t = throwIO $ err401 { errBody = LBS.fromStrict (TE.encodeUtf8 t) }

-- decodeHexStrict :: Text -> Either String ByteString
-- decodeHexStrict = B16.decode . TE.encodeUtf8

-- decodeHexSized :: Int -> Text -> String -> Either String ByteString
-- decodeHexSized n t what = do
--   bs <- decodeHexStrict t
--   if BS.length bs == n
--     then Right bs
--     else Left $ what <> " must be exactly " <> show n <> " bytes (got " <> show (BS.length bs) <> ")"

-- hex :: ByteString -> Text
-- hex = TE.decodeUtf8 . B16.encode

--------------------------------------------------------------------------------
-- Handlers: seller data

handleGetOnRampAddr :: Ctx -> IO Text
handleGetOnRampAddr ctx = do
  case onRampPolicy $ ctxOnRampParams ctx of
    Left err -> badRequest err
    Right onRampScript -> do
      let nid            = cfgNetworkId $ ctxCoreCfg ctx
          onRampAddress' = addressFromValidator nid onRampScript
          onRampAddress  = addressToText onRampAddress'

      return onRampAddress
