{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.P2P.Contract where

import           GHC.Natural                               (Natural)
import           Data.Monoid                               (First (..))
import           Data.Function                             ((&))
import           Data.Functor                              ((<&>))
import           Prelude                                   hiding (Bool,
                                                            Eq ((==)), any,
                                                            divMod, elem,
                                                            length, splitAt,
                                                            truncate, (&&), (*),
                                                            (+), (||))

import qualified Prelude                                   as Haskell

import           ZkFold.Base.Algebra.Basic.Class           (AdditiveSemigroup (..),
                                                            BinaryExpansion (..),
                                                            FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.Class
import           ZkFold.Base.Algebra.EllipticCurve.Ed25519
import           ZkFold.Symbolic.Algorithms.Hash.SHA2      (SHA2, sha2)
import           ZkFold.Symbolic.Cardano.Types             (Address (..),
                                                            Output (..),
                                                            Transaction (..),
                                                            Value (..),
                                                            txInputs, txOutputs, txiOutput,
                                                            txoDatumHash)
import           ZkFold.Symbolic.Compiler                  (SymbolicData)
import           ZkFold.Symbolic.Data.Bool                 (Bool (..),
                                                            BoolType (..))
import           ZkFold.Symbolic.Data.ByteString           (ByteString (..),
                                                            ShiftBits (..),
                                                            Truncate (..))
import           ZkFold.Symbolic.Data.Combinators          (Extend (..),
                                                            Iso (..))
import           ZkFold.Symbolic.Data.Conditional          (Conditional (..))
import           ZkFold.Symbolic.Data.Ed25519              ()
import           ZkFold.Symbolic.Data.Eq                   (Eq (..))
import           ZkFold.Symbolic.Data.UInt                 (UInt (..))
import           ZkFold.Symbolic.Types                     (Symbolic)

-- Should include part of PAN, account number holder, probably with PCI DSS masking
-- Can be finished when arithmetizable ByteStrings be ready
newtype FiatAccount a = FiatAccount a
    deriving Haskell.Eq

deriving instance
    SymbolicData i a => SymbolicData i (FiatAccount a)

-- According to ISO4217 RFC
newtype CurrencyCode a = CurrencyCode (a, (a, a))
    deriving Haskell.Eq

deriving instance
    SymbolicData i a
    => SymbolicData i (CurrencyCode a)

newtype Offer a = Offer
    (FiatAccount a, (UInt 64 a, CurrencyCode a))
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
    (Address a, FiatTransfer a, (Point (Ed25519 a), UInt 256 a))

deriving instance
    ( EllipticCurve (Ed25519 a)
    , Haskell.Eq a
    , Haskell.Eq (BaseField (Ed25519 a))
    , Haskell.Eq (Address a)
    ) => Haskell.Eq (MatchedOffer a)

deriving instance
    ( SymbolicData i (UInt 64 a)
    , SymbolicData i (UInt 256 a)
    , SymbolicData i (Address a)
    , SymbolicData i (Point (Ed25519 a))
    , SymbolicData i a
    ) => SymbolicData i (MatchedOffer a)

hashMatchedOffer :: MatchedOffer a -> ByteString 256 a
hashMatchedOffer = undefined

-- | TODO: A temporary solution while we don't have a proper serialisation for the types above.
--
serialiseTransfer :: forall a. BinaryExpansion a => FiatTransfer a -> ByteString 1524 a
serialiseTransfer (FiatTransfer (FiatAccount r0, Offer (FiatAccount r1, (UInt rs r2, CurrencyCode (r3, (r4, r5)))))) =
    ByteString $ concatMap binaryExpansion $ (r0 : r1 : rs) <> [r2, r3, r4, r5]


-- | An EdDSA signature on a message M by public key A is the pair (R, S), encoded in 2b bits,
-- of a curve point R ∈  E( Fq ) and an integer 0 < S < ℓ
verifyFiatTransferSignature
    :: forall a
    .  Symbolic a
    => Eq (Bool a) (Point (Ed25519 a))
    => Iso (UInt 256 a) (ByteString 256 a)
    => Extend (ByteString 1524 a) (ByteString 2036 a)
    => Extend (ByteString 256 a) (ByteString 2036 a)
    => BoolType (ByteString 2036 a)
    => ShiftBits (ByteString 2036 a)
    => Truncate (ByteString 512 a) (ByteString 256 a)
    => SHA2 "SHA512" a 2036
    => EllipticCurve (Ed25519 a)
    => ScalarField (Ed25519 a) ~ UInt 256 a
    => BaseField (Ed25519 a) ~ UInt 256 a
    => Point (Ed25519 a)
    -> FiatTransfer a
    -> (Point (Ed25519 a), UInt 256 a)
    -> Bool a
verifyFiatTransferSignature pubkey message (r, s) = (mul s b) == (r + mul hInt pubkey)
    where
        rBS :: ByteString 256 a
        rBS = from $ _y r

        pkeyBS :: ByteString 256 a
        pkeyBS = from $ _y pubkey


        -- TODO: 1524 == 6 * 254 is the number of bits stored in 6 field elements required to describe FiatTransfer.
        -- We need to make this calculation automatic, but adding a type family @TypeSize a x@ to @Arithmetizable a@ requires too many changes in zkfold-base
        -- and breaks automatic deriving of @instance Arithmetizable a@
        --
        messageBits :: ByteString 1524 a
        messageBits = serialiseTransfer message

        fullMsg :: ByteString 2036 a
        fullMsg = (extend rBS `shiftBitsL` 1780) || (extend pkeyBS `shiftBitsL` 1524) || extend messageBits

        h :: ByteString 512 a
        h = sha2 @"SHA512" fullMsg

        hInt :: UInt 256 a
        hInt = from (truncate h :: ByteString 256 a)

        b :: Point (Ed25519 a)
        b = gen

verifyFiatTransferSignature'
    :: forall a
    .  Symbolic a
    => Eq (Bool a) (Point (Ed25519 a))
    => Iso (UInt 256 a) (ByteString 256 a)
    => Extend (ByteString 1524 a) (ByteString 2036 a)
    => Extend (ByteString 256 a) (ByteString 2036 a)
    => BoolType (ByteString 2036 a)
    => ShiftBits (ByteString 2036 a)
    => Truncate (ByteString 512 a) (ByteString 256 a)
    => SHA2 "SHA512" a 2036
    => EllipticCurve (Ed25519 a)
    => ScalarField (Ed25519 a) ~ UInt 256 a
    => BaseField (Ed25519 a) ~ UInt 256 a
    => Conditional (Bool a) (Maybe (Point (Ed25519 a)))
    => Point (Ed25519 a)
    -> FiatTransfer a
    -> (Point (Ed25519 a), UInt 256 a)
    -> Maybe (Point (Ed25519 a))
verifyFiatTransferSignature' p m (r, s) =
    verifyFiatTransferSignature p m (r, s)
        & bool @(Bool a) Nothing (Just p)

transactionHasMatchedOffer :: forall a inputs rinputs outputs .
    Haskell.Eq a =>
    Eq (Bool a) (Output 0 () a) =>
    FromConstant Natural a =>
    Conditional (Bool a) (Maybe (Output 0 () a)) =>
    Transaction inputs rinputs outputs 0 () a -> MatchedOffer a -> Maybe (Output 0 () a)
transactionHasMatchedOffer tx mo@(MatchedOffer (addr, _, _)) =
    do find (txInputs tx <&> txiOutput) $ (== hashMatchedOffer mo) . txoDatumHash
   >>= find (txOutputs tx) . (==) . Output . (addr, ). (, fromConstant 0) . txoTokens

-- TODO: move it to `zkfold-base`
txoTokens :: Output tokens datum a -> Value tokens a
txoTokens (Output (_, (v, _))) = v

-- TODO: move it to `zkfold-base`
find :: forall a t tt .
    Foldable tt =>
    Conditional (Bool a) (Maybe (t a)) =>
    tt (t a) -> (t a -> Bool a) -> Maybe (t a)
find xs p = getFirst $ foldMap (\x -> First (p x & bool @(Bool a) (Just x) Nothing)) xs

p2pMatchedOrderContract
    :: forall inputs rinputs outputs a
    .  Symbolic a
    => Haskell.Eq a
    => Eq (Bool a) (Point (Ed25519 a))
    => Eq (Bool a) (Output 0 () a)
    => Iso (UInt 256 a) (ByteString 256 a)
    => Extend (ByteString 1524 a) (ByteString 2036 a)
    => Extend (ByteString 256 a) (ByteString 2036 a)
    => BoolType (ByteString 2036 a)
    => ShiftBits (ByteString 2036 a)
    => Truncate (ByteString 512 a) (ByteString 256 a)
    => SHA2 "SHA512" a 2036
    => EllipticCurve (Ed25519 a)
    => Conditional (Bool a) (Maybe (Output 0 () a))
    => Conditional (Bool a) (Maybe (Point (Ed25519 a)))
    => ScalarField (Ed25519 a) ~ UInt 256 a
    => BaseField (Ed25519 a) ~ UInt 256 a
    => Point (Ed25519 a)
    -> Transaction inputs rinputs outputs 0 () a
    -> MatchedOffer a
    -> Bool a
p2pMatchedOrderContract vk tx mo@(MatchedOffer (_, trnsfr, sgntr)) =
    maybe false (const true) $ (,)
        <$> verifyFiatTransferSignature' vk trnsfr sgntr
        <*> transactionHasMatchedOffer tx mo
