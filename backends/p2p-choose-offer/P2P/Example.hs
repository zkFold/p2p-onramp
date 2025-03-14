module P2P.Example where

import Data.Ratio                 ((%))
import PlutusLedgerApi.V1.Value   (Lovelace (..), lovelaceValueOf)
import Prelude

import ZkFold.Cardano.UPLC.OnRamp (OnRampDatum (..))


-- | Choice value assigned to each sell offer.  (Higher choice value makes the offer more
-- attractive).
choiceValue :: OnRampDatum -> Rational
choiceValue OnRampDatum { sellPriceUsd, valueSold } = value % price
  where
    value = getLovelace . lovelaceValueOf $ valueSold
    price = sellPriceUsd

