{-# LANGUAGE DeriveAnyClass #-}

module ZkFold.Cardano.Parse.Utils where

import           Cardano.Api                   as Api
import           Data.Aeson                    (eitherDecode, parseJSON)
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Coerce
import           GHC.Generics                  (Generic)
import           GHC.IsList                    (toList)
import           PlutusLedgerApi.V1.Value
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           Text.Parsec                   (parse)

import           ZkFold.Cardano.OffChain.Utils (integerParser, parseInlineDatum)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))


data OnRampUtxo = OnRampUtxo { address         :: String
                             , inlineDatum     :: OnRampDatumJSON
                             , inlineDatumhash :: String
                             , value           :: ValueJSON
                             } deriving stock (Generic, Show)
                               deriving anyclass FromJSON

----- PARSE 'OnRampDatum' -----

newtype OnRampDatumJSON = OnRampDatumJSON OnRampDatum
  deriving stock Show

parseOnRampDatumJSON :: Aeson.Value -> Either String OnRampDatumJSON
parseOnRampDatumJSON v = do
  inlineDatum <- parseInlineDatum v

  case inlineDatum of
    Just (OutputDatum dat) -> maybe (Left "Missing datum") Right
                                (OnRampDatumJSON <$> (fromBuiltinData . getDatum $ dat))
    _                      -> Left "Missing output datum"

instance FromJSON OnRampDatumJSON where
  parseJSON = either fail pure . parseOnRampDatumJSON

-- | Parsing 'OnRampDatum' from a string representation of 'OnRampUtxo'
parseOnRampDatum :: String -> Either String OnRampDatum
parseOnRampDatum utxoStr = coerce . inlineDatum <$> eitherDecode (BL8.pack utxoStr)


----- PARSE 'Value' -----

toPlutusValue :: Api.Value -> V3.Value
toPlutusValue val =
    mconcat . map convertAsset $ toList val
  where
    convertAsset (AdaAssetId, Quantity q) =
        singleton adaSymbol adaToken q
    convertAsset (AssetId policyId assetName, Quantity q) =
        singleton
            (CurrencySymbol . toBuiltin . serialiseToRawBytes $ policyId)
            (TokenName . toBuiltin . serialiseToRawBytes $ assetName)
            q

newtype ValueJSON = ValueJSON V3.Value
  deriving stock Show

instance FromJSON ValueJSON where
  parseJSON v = ValueJSON . toPlutusValue <$> parseJSON @Api.Value v

-- | Parsing 'Value' from a string representation of 'OnRampUtxo'
parseValue :: String -> Either String V3.Value
parseValue utxoStr = coerce . value <$> eitherDecode (BL8.pack utxoStr)


----- PARSE 'Integer' -----

parseInteger :: String -> Either String Integer
parseInteger = either (Left . show) Right . parse integerParser ""
