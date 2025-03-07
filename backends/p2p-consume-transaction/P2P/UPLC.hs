{-# LANGUAGE TemplateHaskell #-}

module P2P.UPLC where

import           GHC.Generics                (Generic)
import           PlutusLedgerApi.V3          (POSIXTime)
import           PlutusTx                    (makeIsDataIndexed)
import           Prelude                     (Show)


data ValidityRedeemer = ValidityRedeemer { vrTimeMark :: POSIXTime }
  deriving stock (Generic, Show)

makeIsDataIndexed ''ValidityRedeemer [('ValidityRedeemer,0)]
