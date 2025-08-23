module ZkFold.Cardano.Crypto.Utils where

import           Crypto.Error           (CryptoFailable (..))
import           Crypto.PubKey.Ed25519
import           Data.Aeson             (FromJSON, eitherDecode, withObject,
                                         (.:))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import           Prelude                (Either (..), FilePath, IO, String,
                                         pure, ($), (-), (<$>), (>=), (>>=))


data KeyFile = KeyFile { cborHex :: String }

instance FromJSON KeyFile where
    parseJSON = withObject "KeyFile" $ \v -> KeyFile <$> v .: "cborHex"

-- | Obtain the last 32 bytes of a key as string
stringToKey :: String -> Either String B8.ByteString
stringToKey hexStr = do
  rawBytes <- B16.decode $ B8.pack hexStr
  if B8.length rawBytes >= 32
    then Right $ B8.drop (B8.length rawBytes - 32) rawBytes
    else Left "Unable to retrieve rawbytes"

-- | Extract the last 32 bytes of a key in file
extractKey :: FilePath -> IO (Either String B8.ByteString)
extractKey filePath = do
  fileContent <- BL.readFile filePath

  pure $ do
    KeyFile hexStr <- eitherDecode fileContent
    stringToKey hexStr

-- | Obtain secret key from bytes
bytesToSecretKey :: B8.ByteString -> Either String SecretKey
bytesToSecretKey skBytes = case secretKey skBytes of
  CryptoPassed key -> Right key
  CryptoFailed _   -> Left "Invalid skey"

-- | Extract secret key in file
extractSecretKey :: FilePath -> IO (Either String SecretKey)
extractSecretKey filePath = do
  skBytesE <- extractKey filePath
  pure $ skBytesE >>= bytesToSecretKey
