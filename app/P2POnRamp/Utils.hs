{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Utils where

import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Char8         as BS
import           PlutusLedgerApi.V3            as V3
import           Prelude


-- | Decode hex-encoded String into BuiltinByteString
hexToBuiltin :: String -> Either String BuiltinByteString
hexToBuiltin s = toBuiltin <$> B16.decode (BS.pack s)
