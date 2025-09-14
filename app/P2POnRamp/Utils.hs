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

-- | Decode hex-encoded Text into BuiltinByteString
hexToBuiltin' :: T.Text -> Either String BuiltinByteString
hexToBuiltin' t = toBuiltin <$> B16.decode (TE.encodeUtf8 t)

-- | Convert ByteString as hex-encoded Text
toHexText :: BS.ByteString -> T.Text
toHexText = TE.decodeUtf8 . B16.encode

-- | Convert POSIX time to miliseconds (integer).
posixToMillis :: Clock.POSIXTime -> Integer
posixToMillis t =
  floor (1000 * nominalDiffTimeToSeconds t :: Pico)
