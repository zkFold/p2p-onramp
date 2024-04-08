{-# LANGUAGE UndecidableInstances #-}
module ZkFold.P2P.Payment where

import           Prelude                  hiding (Bool, Eq ((==)), (*), (+), elem, length, splitAt)
import           Control.Monad.State.Lazy        (evalState, state)
import           Data.Foldable                   (find)

import qualified Prelude  as Haskell              (Eq ((==)))

import           ZkFold.Prelude
import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector
import           ZkFold.Symbolic.Cardano.Types
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.UInt

-- Should include part of PAN, account number holder, probably with PCI DSS masking
-- Can be finished when arithmetizable ByteStrings be ready
newtype FiatAccount a = FiatAccount a
    deriving Haskell.Eq

-- deriving instance
--    (Ring a, Finite a, MultiplicativeGroup a, Eq (Bool a) a) => Eq (Bool a) (FiatAccount a)

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

newtype MatchedOffer a = MatchedOffer
    (FiatAccount a, (UInt 64 a, (ISO427 a, Address a)))
    deriving Haskell.Eq

deriving instance
    ( Arithmetizable i (UInt 64 a)
    , Arithmetizable i a
    ) => Arithmetizable i (MatchedOffer a)

zkSmartContract ::
    Eq (Bool a) (Offer a) =>
    Transaction rinputs input outputs tokens Offer a -> Offer a -> Bool a
zkSmartContract tx offer = elem offer $ txiDatum <$> txInputs tx
