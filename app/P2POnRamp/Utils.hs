{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Utils where

-- import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C8
import qualified Data.ByteString.Base16  as B16
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
-- import           PlutusTx.Builtins       (toBuiltin)
import           PlutusLedgerApi.V3            as V3
import           Prelude


-- | Decode hex-encoded String into BuiltinByteString
hexToBuiltin :: String -> Either String BuiltinByteString
hexToBuiltin s = toBuiltin <$> B16.decode (C8.pack s)

-- | Decode hex-encoded Text into BuiltinByteString
hexToBuiltin' :: T.Text -> Either String BuiltinByteString
hexToBuiltin' t = toBuiltin <$> B16.decode (TE.encodeUtf8 t)
