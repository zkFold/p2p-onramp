{-# LANGUAGE UndecidableInstances #-}

module ZkFold.P2P.Contract where

import           Control.Monad.State.Lazy         (evalState, state)
import           Data.Maybe                       (fromJust)
import           GHC.Natural                      (Natural)
import           Prelude                          hiding (Bool, Eq ((==)), (&&), (*), (+), elem, length, splitAt, any)

import qualified Prelude  as Haskell              (Eq ((==)))

import           ZkFold.Base.Algebra.Basic.Class  (fromConstant)
import           ZkFold.Symbolic.Cardano.Types
import           ZkFold.Symbolic.Compiler         (Arithmetizable)
import           ZkFold.Symbolic.Data.Bool        (Bool, any)
import           ZkFold.Symbolic.Data.ByteString  (ByteString)
import           ZkFold.Symbolic.Data.Conditional (Conditional(..))
import           ZkFold.Symbolic.Data.Eq          (Eq(..))
import           ZkFold.Symbolic.Data.UInt        (UInt)
import           ZkFold.Symbolic.Types            (Symbolic)

-- Should include part of PAN, account number holder, probably with PCI DSS masking
-- Can be finished when arithmetizable ByteStrings be ready
newtype FiatAccount a = FiatAccount a
    deriving Haskell.Eq

deriving instance
    Arithmetizable i a => Arithmetizable i (FiatAccount a)

newtype ISO427 a = ISO427 (a, (a, a))
    deriving Haskell.Eq

deriving instance
    Arithmetizable i a
    => Arithmetizable i (ISO427 a)

newtype Offer a = Offer
    (FiatAccount a, (UInt 64 a, ISO427 a))
    deriving Haskell.Eq

deriving instance
    ( Arithmetizable i (UInt 64 a)
    , Arithmetizable i a
    ) => Arithmetizable i (Offer a)

newtype FiatTransfer a = FiatTransfer
    (FiatAccount a, Offer a)
    deriving Haskell.Eq

deriving instance
    ( Arithmetizable a (FiatAccount a)
    , Arithmetizable a (Offer a)
    ) => Arithmetizable a (FiatTransfer a)

newtype MatchedOffer a = MatchedOffer
    ((Address a, FiatAccount a), Offer a)
    deriving Haskell.Eq

deriving instance
    ( Arithmetizable a (UInt 64 a)
    , Arithmetizable a (ByteString 4 a)
    , Arithmetizable a (ByteString 224 a)
    , Arithmetizable a a
    ) => Arithmetizable a (MatchedOffer a)

hashMatchedOffer :: MatchedOffer a -> ByteString 256 a
hashMatchedOffer = undefined

p2pMatchedOrderContract :: forall inputs rinputs outputs tokens a .
    ( Symbolic a
    , Eq (Bool a) (Output () tokens a)
    , Eq (Bool a) (ByteString 256 a)
    , Conditional (Bool a) (Maybe (Output () tokens a))
    )
    => Transaction inputs rinputs  outputs tokens () a -> MatchedOffer a -> Bool a
p2pMatchedOrderContract tx mo@(MatchedOffer ((addr, sender), Offer (recipient, (val, cur)))) =
    let h                  = hashMatchedOffer mo
        f = (\o acc -> bool @(Bool a) acc (Just o) (txoDatumHash o == h))
        -- TODO: Simplify this using symbolic `find`.
        v = (\(Output (_, (v, _))) -> v) $ fromJust $ foldr f Nothing $
            fmap (\(Input (_, o)) -> o) $ txInputs tx
        -- TODO: Instead of `zero`, it should be the hash of `()`.
        txo                = Output (addr, (v, fromConstant 0 :: ByteString 256 a)) :: Output () tokens a
    in any (\o -> txo == o) $ txOutputs tx
