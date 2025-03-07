module P2P.Example where

import           PlutusLedgerApi.V3
import           PlutusTx.Builtins  (ByteOrder (..))
import           PlutusTx.Prelude

-- Payment info hash example
paymentInfoHashEx1 :: BuiltinByteString
paymentInfoHashEx1 = blake2b_256 $ integerToByteString BigEndian 0 43
