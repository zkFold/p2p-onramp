{-# LANGUAGE DeriveAnyClass  #-}

module ZkFold.Cardano.Parse.Utils where

import           Data.Aeson                 (eitherDecode, (.:?))
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types           (parseEither)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe                 (maybe)
import           PlutusLedgerApi.V3         as V3
import           Prelude                    (Either (..), Maybe (..), String,
                                             ($), (.), (++), (>>=))

import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))
import           ZkFold.Cardano.OffChain.Utils (parseInlineDatum)


parseOnRampDatumJSON :: Aeson.Value -> Either String OnRampDatum
parseOnRampDatumJSON (Aeson.Object v) = do
  inlineDatum <- case parseEither (.:? "inlineDatum") v of
                   Right (Just inlineDatumObject) -> parseInlineDatum inlineDatumObject
                   Right Nothing                  -> Left "Missing inline datum"
                   Left err                       -> Left $ "Failed to parse inlineDatum: " ++ err

  case inlineDatum of
    Just (OutputDatum dat) -> maybe (Left "Missing datum") Right (fromBuiltinData . getDatum $ dat)
    _                      -> Left "Missing output datum"
parseOnRampDatumJSON _ = Left "Failed to parse datum"

-- | Parse 'OnRampDatum' from a string representation of UTxO
parseOnRampDatum :: String -> Either String OnRampDatum
parseOnRampDatum utxoStr = eitherDecode (BL8.pack utxoStr) >>= parseOnRampDatumJSON
