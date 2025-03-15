module ZkFold.Cardano.P2P.Example where

import qualified Data.ByteString.Char8      as B8
import           Data.Ratio                 ((%))
import           PlutusLedgerApi.V1.Value   (Lovelace (..), lovelaceValueOf)
import           PlutusLedgerApi.V3
import           PlutusTx.Prelude
import           Prelude                    (String)
import qualified Prelude                    as Haskell

import           ZkFold.Cardano.UPLC.OnRamp (OnRampDatum (..))


------------------ :Payment Info: ------------------

-- | Payment info hash example.
-- In this example, the "fiat payment information" is just the hash of the seller's name.
paymentInfoHashEx1 :: String -> BuiltinByteString
paymentInfoHashEx1 = blake2b_256 . toBuiltin . B8.pack



-------------- :Choosing Sell Offer: ---------------

-- | Choice value assigned to each sell offer.  (Higher choice value makes the offer more
-- attractive).
choiceValue :: OnRampDatum -> Haskell.Rational
choiceValue OnRampDatum { sellPriceUsd, valueSold } = value % price
  where
    value = getLovelace . lovelaceValueOf $ valueSold
    price = sellPriceUsd
