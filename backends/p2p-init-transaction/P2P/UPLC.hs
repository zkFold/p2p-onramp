{-# LANGUAGE TemplateHaskell #-}

module P2P.UPLC where

import           GHC.Generics                (Generic)
import           PlutusLedgerApi.V1.Interval (before)
import           PlutusLedgerApi.V3          (BuiltinData, POSIXTime, POSIXTimeRange, Redeemer (..),ScriptContext (..), TxInfo (..),
                                              UnsafeFromData (..))
import           PlutusTx                    (CompiledCode, compile, liftCodeDef, makeIsDataIndexed, unsafeApplyCode)
import           PlutusTx.Prelude            (Bool (..), BuiltinUnit, Integer, check, ($), (.))
import           Prelude                     (Show)


data ValidityRedeemer = ValidityRedeemer { vrTimeMark :: POSIXTime }
  deriving stock (Generic, Show)

makeIsDataIndexed ''ValidityRedeemer [('ValidityRedeemer,0)]

{-# INLINABLE timed #-}
timed :: ValidityRedeemer -> ScriptContext -> Bool
timed red ctx = isCurrent
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validityRange :: POSIXTimeRange
    validityRange = txInfoValidRange info

    isCurrent :: Bool
    isCurrent = before (vrTimeMark red) validityRange

{-# INLINABLE untypedTimed #-}
untypedTimed :: Integer -> BuiltinData -> BuiltinUnit
untypedTimed _tag ctx' =
  let
    ctx = unsafeFromBuiltinData ctx'
    red = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ timed red ctx

timedCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
timedCompiled tag =
    $$(compile [|| untypedTimed ||])
    `unsafeApplyCode` liftCodeDef tag
