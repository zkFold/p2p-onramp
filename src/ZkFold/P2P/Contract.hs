{-# LANGUAGE UndecidableInstances #-}

module ZkFold.P2P.Contract where

import           Control.Monad.State.Lazy         (evalState, state)
import           Data.Maybe                       (fromJust)
import           GHC.Natural                      (Natural)
import           Prelude                          hiding (Bool, Eq ((==)), (&&), (*), (+), elem, length, splitAt, any)

import qualified Prelude  as Haskell              (Eq ((==)))

import           ZkFold.Base.Algebra.Basic.Class  (fromConstant)
import           ZkFold.Symbolic.Cardano.Types
import           ZkFold.Symbolic.Compiler         (SymbolicData, SomeArithmetizable)
import           ZkFold.Symbolic.Data.Bool        (Bool(..), BoolType (..), any)
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
    SymbolicData i a => SymbolicData i (FiatAccount a)

newtype ISO427 a = ISO427 (a, (a, a))
    deriving Haskell.Eq

deriving instance
    SymbolicData i a
    => SymbolicData i (ISO427 a)

newtype Offer a = Offer
    (FiatAccount a, (UInt 64 a, ISO427 a))
    deriving Haskell.Eq

deriving instance
    ( SymbolicData i (UInt 64 a)
    , SymbolicData i a
    ) => SymbolicData i (Offer a)

newtype FiatTransfer a = FiatTransfer
    (FiatAccount a, Offer a)
    deriving Haskell.Eq

deriving instance
    ( SymbolicData i (FiatAccount a)
    , SymbolicData i (Offer a)
    ) => SymbolicData i (FiatTransfer a)

newtype MatchedOffer a = MatchedOffer
    (Address a, FiatTransfer a, ByteString 256 a)
    deriving Haskell.Eq

deriving instance
    ( SymbolicData i (UInt 64 a)
    , SymbolicData i (ByteString 4 a)
    , SymbolicData i (ByteString 224 a)
    , SymbolicData i (ByteString 256 a)
    , SymbolicData i (Address a)
    , SymbolicData i a
    ) => SymbolicData i (MatchedOffer a)

hashMatchedOffer :: MatchedOffer a -> ByteString 256 a
hashMatchedOffer = undefined

verifyFiatTransferSignature :: ByteString 256 a -> FiatTransfer a -> ByteString 256 a -> Bool a
verifyFiatTransferSignature = undefined

p2pMatchedOrderContract :: forall inputs rinputs outputs tokens a .
    ( Symbolic a
    , Eq (Bool a) (Output () tokens a)
    , Eq (Bool a) (ByteString 256 a)
    , Conditional (Bool a) (Maybe (Output () tokens a))
    )
    => ByteString 256 a -> Transaction inputs rinputs  outputs tokens () a -> MatchedOffer a -> Bool a
p2pMatchedOrderContract vk tx mo@(MatchedOffer (addr, transfer, signature)) =
    let h                  = hashMatchedOffer mo
        f = (\o acc -> bool @(Bool a) acc (Just o) (txoDatumHash o == h))
        -- TODO: Simplify this using symbolic `find`.
        v = (\(Output (_, (v, _))) -> v) $ fromJust $ foldr f Nothing $
            fmap (\(Input (_, o)) -> o) $ txInputs tx
        -- TODO: Instead of `zero`, it should be the hash of `()`.
        txo                = Output (addr, (v, fromConstant 0 :: ByteString 256 a)) :: Output () tokens a
    in any (\o -> txo == o) (txOutputs tx) && verifyFiatTransferSignature vk transfer signature
