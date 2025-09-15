{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Utils where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as C8
import           Data.Fixed                 (Pico)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Time.Clock.POSIX      as Clock (POSIXTime)
import           Data.Time.Clock            (nominalDiffTimeToSeconds)
import           PlutusLedgerApi.V3         as V3
import           Prelude


-- | Decode hex-encoded String into BuiltinByteString
hexToBuiltin :: String -> Either String BuiltinByteString
hexToBuiltin s = toBuiltin <$> B16.decode (C8.pack s)

decodeHexStrict :: T.Text -> Either String BS.ByteString
decodeHexStrict = B16.decode . TE.encodeUtf8

-- | Decode hex-encoded Text into BuiltinByteString
hexToBuiltin' :: T.Text -> Either String BuiltinByteString
hexToBuiltin' t = toBuiltin <$> decodeHexStrict t

-- | Convert ByteString as hex-encoded Text
toHexText :: BS.ByteString -> T.Text
toHexText = TE.decodeUtf8 . B16.encode

decodeHexSized :: Int -> T.Text -> String -> Either String BS.ByteString
decodeHexSized n t what = do
  bs <- decodeHexStrict t
  if BS.length bs == n
    then Right bs
    else Left $ what <> " must be exactly " <> show n <> " bytes (got " <> show (BS.length bs) <> ")"

-- | Convert POSIX time to miliseconds (integer).
posixToMillis :: Clock.POSIXTime -> Integer
posixToMillis t =
  floor (1000 * nominalDiffTimeToSeconds t :: Pico)
