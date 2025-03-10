module ZkFold.Cardano.Crypto.Utils where

import qualified Data.Aeson             as Aeson
import           Data.Aeson             (FromJSON, eitherDecode, withObject, (.:))
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import           Crypto.PubKey.Ed25519
import           Crypto.Error           (CryptoFailable(..))
import           Prelude                (IO, Either (..), FilePath, String, pure, ($), (<$>), (-), (>=))


data KeyFile = KeyFile { cborHex :: String }

instance FromJSON KeyFile where
    parseJSON = withObject "KeyFile" $ \v -> KeyFile <$> v .: "cborHex"

-- | Extract the last 32 bytes of a key
extractKey :: FilePath -> IO (Either String B8.ByteString)
extractKey filePath = do
  fileContent <- BL.readFile filePath

  pure $ do
    KeyFile hexStr <- eitherDecode fileContent
    rawBytes       <- B16.decode $ B8.pack hexStr

    if B8.length rawBytes >= 32
      then Right $ B8.drop (B8.length rawBytes - 32) rawBytes
      else Left "Unable to retrieve rawbytes"

-- | Extract key checking it is a valid secret key
extractSecretKey :: FilePath -> IO (Either String SecretKey)
extractSecretKey filePath = do
  skBytesE <- extractKey filePath

  pure $ do
    skBytes <- skBytesE
    case secretKey skBytes of
      CryptoPassed key -> Right key
      CryptoFailed _   -> Left "Invalid skey"
