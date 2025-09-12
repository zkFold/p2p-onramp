{-# LANGUAGE DeriveAnyClass #-}

module P2POnRamp.Utils where

-- import qualified Data.ByteString         as BS
import           Data.Aeson              (eitherDecode)
import           Cardano.Api             (getScriptData)
import           Cardano.Api.Shelley     (toPlutusData, scriptDataFromJsonDetailedSchema)
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Char8   as C8
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
-- import           PlutusTx.Builtins       (toBuiltin)
import           PlutusLedgerApi.V3            as V3
import           Prelude

import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))


-- | Decode hex-encoded String into BuiltinByteString
hexToBuiltin :: String -> Either String BuiltinByteString
hexToBuiltin s = toBuiltin <$> B16.decode (C8.pack s)

-- | Decode hex-encoded Text into BuiltinByteString
hexToBuiltin' :: T.Text -> Either String BuiltinByteString
hexToBuiltin' t = toBuiltin <$> B16.decode (TE.encodeUtf8 t)

-- | Temporary hack: read 'OnRampDatum'. ToDo: eliminate this hack by
-- incorporating buyerPKHash & timelock into database
readOnRampDatum :: FilePath -> IO (Either String OnRampDatum)
readOnRampDatum path = do
  bs <- LBS.readFile path
  pure $ do
    j <- eitherDecode bs
    either (Left . show)
           (Right . unsafeFromData . toPlutusData . getScriptData)
           (scriptDataFromJsonDetailedSchema j)
