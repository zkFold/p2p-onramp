module P2P.Example where

import qualified Data.ByteString.Char8  as B8
import           PlutusLedgerApi.V3
import           Prelude            (String)
import           PlutusTx.Prelude


-- | Payment info hash example.
-- In this example, the "fiat payment information" is just the hash of the seller's name.
paymentInfoHashEx1 :: String -> BuiltinByteString
paymentInfoHashEx1 = blake2b_256 . toBuiltin . B8.pack

