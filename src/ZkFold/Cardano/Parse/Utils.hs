{-# LANGUAGE DeriveAnyClass  #-}

module ZkFold.Cardano.Parse.Utils where

import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Coerce
import           Data.Maybe                 (maybe)
import           GHC.Generics               (Generic)
import           PlutusLedgerApi.V3         as V3
import           Prelude                    (Either (..), Integer, Maybe (..), String, either, fail, pure, show,
                                             ($), (<$>), (.))
import           Text.Parsec                (parse)

import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))
import           ZkFold.Cardano.OffChain.Utils (integerParser, parseInlineDatum)

----- PARSE 'OnRampDatum' -----

newtype OnRampDatumJSON = OnRampDatumJSON OnRampDatum

parseOnRampDatumJSON :: Aeson.Value -> Either String OnRampDatumJSON
parseOnRampDatumJSON v = do
  inlineDatum <- parseInlineDatum v

  case inlineDatum of
    Just (OutputDatum dat) -> maybe (Left "Missing datum") Right
                                (OnRampDatumJSON <$> (fromBuiltinData . getDatum $ dat))
    _                      -> Left "Missing output datum"

instance FromJSON OnRampDatumJSON where
  parseJSON v = either fail pure (parseOnRampDatumJSON v)

data OnRampUtxo = OnRampUtxo { address         :: String
                             , inlineDatum     :: OnRampDatumJSON
                             , inlineDatumhash :: String
                             , value           :: Aeson.Value
                             } deriving stock Generic
                               deriving anyclass FromJSON

-- | Parsing 'OnRampDatum' from a string representation of 'OnRampUtxo'
parseOnRampDatum :: String -> Either String OnRampDatum
parseOnRampDatum utxoStr = coerce . inlineDatum <$> eitherDecode (BL8.pack utxoStr)


----- PARSE 'Integer' -----

parseInteger :: String -> Either String Integer
parseInteger = either (Left . show) Right . parse integerParser ""
