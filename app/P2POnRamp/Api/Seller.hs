{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}


module P2POnRamp.Api.Seller where

import           Control.Exception             (throwIO)
import           Data.Aeson                    (FromJSON (..), ToJSON (..), object, withObject, (.=), (.:))
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteArray                as BA
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GHC.Generics                  (Generic)
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           PlutusTx                      (makeIsDataIndexed)
import           Prelude
import           Servant
import           System.FilePath               ((</>))

-- Ed25519 (cryptonite)
import           Crypto.Error                  (CryptoFailable(..))
import qualified Crypto.PubKey.Ed25519         as Ed25519

import           P2POnRamp.Api.Context         (dbFile)
import           P2POnRamp.OrdersDB            (Order (..), SellerInfo (..), createOrder)
import           ZkFold.Cardano.Crypto.Utils   (eitherHexToKey)
import           ZkFold.Cardano.OffChain.Utils (dataToJSON)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))

expectedMessage :: ByteString
expectedMessage = "Hello messenger."

newtype SPKB = SPKB BuiltinByteString

sellerPKB :: BS.ByteString -> SPKB
sellerPKB sellerPubKey = SPKB $ toBuiltin sellerPubKey

--------------------------------------------------------------------------------
-- Helpers

badRequest :: String -> IO a
badRequest msg = throwIO $ err400 { errBody = LBS.fromStrict (BSC.pack msg) }

internalErr :: String -> IO a
internalErr msg = throwIO $ err500 { errBody = LBS.fromStrict (BSC.pack msg) }

unauthorizedText :: Text -> IO Text
unauthorizedText t = throwIO $ err401 { errBody = LBS.fromStrict (TE.encodeUtf8 t) }

decodeHexStrict :: Text -> Either String ByteString
decodeHexStrict = B16.decode . TE.encodeUtf8

decodeHexSized :: Int -> Text -> String -> Either String ByteString
decodeHexSized n t what = do
  bs <- decodeHexStrict t
  if BS.length bs == n
    then Right bs
    else Left $ what <> " must be exactly " <> show n <> " bytes (got " <> show (BS.length bs) <> ")"

hex :: ByteString -> Text
hex = TE.decodeUtf8 . B16.encode


--------------------------------------------------------------------------------
-- Handlers: message & sign

data VKey = VKey
  { vkType :: Text
  , description :: Text
  , cborHex :: Text
  } deriving Show

instance FromJSON VKey where
  parseJSON = withObject "VKey" $ \o ->
    VKey <$> o .: "type"
         <*> o .: "description"
         <*> o .: "cborHex"

instance ToJSON VKey where
  toJSON (VKey t d c) =
    object [ "type"        .= t
           , "description" .= d
           , "cborHex"     .= c
           ]

data SellerOK = SellerOK
  { message :: Text
  , signatureHex :: Text
  , vkeyJson :: VKey
  , scheme :: Text
  , note :: Text
  } deriving (Show, Generic)

instance FromJSON SellerOK
instance ToJSON SellerOK

handleMessage :: IO Text
handleMessage = pure "hello"

handleSigned :: SellerOK -> IO SellerOK
handleSigned = pure


--------------------------------------------------------------------------------
-- Handlers: seller data

data SellerData = SellerData
  { sdName          :: Text
  , sdAccount       :: Text
  , sdSellAda       :: Integer
  , sdPrice         :: Integer
  , sdSellerPKBytes :: Text
  } deriving (Show, Generic)

instance FromJSON SellerData
instance ToJSON SellerData

data SellerInfoBBS = SellerInfoBBS
  { sellerName'    :: BuiltinByteString
  , sellerAccount' :: BuiltinByteString
  } deriving Show

makeIsDataIndexed ''SellerInfoBBS [('SellerInfoBBS, 0)]

fromSellerInfo :: SellerInfo -> SellerInfoBBS
fromSellerInfo (SellerInfo nm acc) = SellerInfoBBS nm' acc'
  where nm'  = toBuiltin $ TE.encodeUtf8 nm
        acc' = toBuiltin $ TE.encodeUtf8 acc

data NewOrder = NewOrder { newID :: Int, newDatum :: Aeson.Value }
  deriving (Show, Generic)

instance FromJSON NewOrder
instance ToJSON NewOrder

handleSellerData :: FilePath -> SellerData -> IO NewOrder
handleSellerData path seller = do
  let sellerInfo  = SellerInfo (sdName seller) (sdAccount seller)
      sellerVKeyE = do
        pkBytes <- eitherHexToKey . T.unpack $ sdSellerPKBytes seller
        case Ed25519.publicKey pkBytes of
          CryptoPassed vk -> Right vk
          CryptoFailed _  -> Left "Malformed public key"

  case sellerVKeyE of
    Left err -> badRequest err
    Right sellerVKey -> do
      let paymentInfoHash   = dataToBlake $ fromSellerInfo sellerInfo
          sellPriceUsd      = sdPrice seller
          valueSold         = lovelaceValue . Lovelace $ sdSellAda seller
          sellerPubKeyBytes = toBuiltin @BS.ByteString . BA.convert $ sellerVKey
      
      let sellerDatum = OnRampDatum paymentInfoHash sellPriceUsd valueSold sellerPubKeyBytes Nothing Nothing

      newOrder <- createOrder (path </> dbFile) sellerInfo

      return $ NewOrder (orderID newOrder) (dataToJSON sellerDatum)
