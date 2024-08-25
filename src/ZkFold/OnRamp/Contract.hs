{-# LANGUAGE UndecidableInstances #-}

module ZkFold.OnRamp.Contract where

import           GHC.Natural                               (Natural)
import           Prelude                           hiding (Bool, Maybe
                                                    , any, divMod, elem, maybe, length, splitAt, truncate
                                                    , (&&), (*), (+), (||), (==))

import           ZkFold.Base.Algebra.Basic.Class   (fromConstant)
import           ZkFold.Symbolic.Data.Bool         (Bool, BoolType (..))
import           ZkFold.Symbolic.Data.ByteString   (ByteString)
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.FieldElement (FieldElement (..))
import           ZkFold.Symbolic.Data.Maybe        (Maybe)
import           ZkFold.Symbolic.Data.UInt         (UInt)
import           ZkFold.Symbolic.Data.UTCTime      (UTCTime)
import           ZkFold.Symbolic.Class             (Symbolic)
import           ZkFold.Symbolic.Cardano.Types     (Transaction, Input)

type R = Auto

data OnRampDatum c = OnRampDatum
    { paymentInfoHash  :: ByteString 256 c
    , sellPriceUsd     :: UInt 64 R c
    , sellAmount       :: UInt 64 R c
    , sellerPubKeyHash :: FieldElement c
    , buyerPubKeyHash  :: Maybe c (FieldElement c)
    , timelock         :: Maybe c (UTCTime c)
    }

moneyKitPKH :: Symbolic c => FieldElement c
moneyKitPKH = fromConstant @Natural 0

type Tokens  = 2
type Datum c = Maybe c (OnRampDatum c)
type TxIn c  = Input Tokens (Datum c) c
type Tx c    = Transaction 2 1 3 Tokens 0 (Datum c) c

-- TODO: add signature to the second argument
onRampContract :: Symbolic c => Tx c -> OnRampDatum c -> Bool c
onRampContract _ _ =
    let 
    -- Find the input with `OnRampDatum`
    -- Find the output with `OnRampDatum`

    -- Check that there is at most one input and one output with `OnRampDatum`
    noDuplicateContractUTXOs = undefined

    -- Check that `buyerPubKeyHash` and `timelock` are `Nothing` for the input and are `Just` for the output (Intent to buy)
    -- Check that the updated datum (+ "Intent to buy" tag) is signed by MoneyKit (Intent to buy)
    intentToBuy = undefined

    -- Check that the datum (+ "Cancel the sell" tag) is signed by the seller (Cancel the sell)
    -- Check that the validity interval of the transaction is past the timelock (Cancel the sell)
    -- Check that the correct amount is returned to the seller (Cancel the sell)
    cancelSell = undefined

    -- Check that the datum (+ "Claim" tag) is signed by MoneyKit (Claim)
    -- Check that the correct amount is sent to the buyer (Claim)
    claim = undefined

    in noDuplicateContractUTXOs && (intentToBuy || cancelSell || claim)
