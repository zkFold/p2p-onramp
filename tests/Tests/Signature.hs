{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Signature (specSignature) where

import           GHC.TypeNats                                (Natural)
import           Prelude                                     (IO, String,
                                                              return, ($),
                                                              (<$>))
import           Test.Hspec                                  (Spec, describe,
                                                              hspec, it)
import           Test.QuickCheck                             (Arbitrary (..),
                                                              Gen, Testable,
                                                              property, (===))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.P2P.Contract                         (FiatAccount (..),
                                                              FiatTransfer (..),
                                                              ISO427 (..),
                                                              Offer (..),
                                                              serialiseTransfer,
                                                              verifyFiatTransferSignature)
import           ZkFold.Prelude                              (chooseNatural)
import           ZkFold.Symbolic.Algorithms.Hash.SHA2        (SHA2, SHA2N, sha2,
                                                              sha2Natural)
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString             (ByteString,
                                                              Extend (..),
                                                              ShiftBits (..),
                                                              Truncate (..))
import           ZkFold.Symbolic.Data.Combinators            (Iso (..))
import           ZkFold.Symbolic.Data.UInt                   (UInt)

deriving instance (Arbitrary a) => Arbitrary (FiatAccount a)
deriving instance (FromConstant Natural a, Finite a, AdditiveMonoid a, Arbitrary a) => Arbitrary (Offer a)
deriving instance (Arbitrary a) => Arbitrary (ISO427 a)
deriving instance (FromConstant Natural a, Finite a, AdditiveMonoid a, Arbitrary a) => Arbitrary (FiatTransfer a)
instance (AdditiveMonoid a, Finite a, FromConstant Natural a, Arbitrary a) => Arbitrary (UInt 64 a) where
    arbitrary = fromConstant <$> toss (2 ^ (64 :: Natural))

toss :: Natural -> Gen Natural
toss x = chooseNatural (0, x)

it' :: Testable prop => String -> prop -> Spec
it' desc prop = it desc (property prop)

sign
    :: forall a
    .  SHA2 "SHA512" a 2036
    => SHA2 "SHA512" a 1780
    => SHA2N "SHA512" a
    => AdditiveSemigroup (UInt 256 a)
    => MultiplicativeSemigroup (UInt 256 a)
    => ShiftBits (ByteString 512 a)
    => Truncate (ByteString 512 a) (ByteString 256 a)
    => Field a
    => Extend (ByteString 1524 a) (ByteString 2036 a)
    => BoolType (ByteString 2036 a)
    => BoolType (ByteString 1780 a)
    => ShiftBits (ByteString 2036 a)
    => ShiftBits (ByteString 1780 a)
    => Extend (ByteString 1524 a) (ByteString 1780 a)
    => Extend (ByteString 256 a) (ByteString 1780 a)
    => Extend (ByteString 256 a) (ByteString 2036 a)
    => Iso (ByteString 256 a) (UInt 256 a)
    => Natural
    -> FiatTransfer a
    -> (ByteString 256 a, UInt 256 a, UInt 256 a)
sign k f = (publicKey, from rCapital, rInt + hInt * privateKey)
    where
        messageBits :: ByteString 1524 a
        messageBits = serialiseTransfer f

        -- b is the base point
        b :: UInt 256 a
        b = fromConstant (15112221349535400772501151409588531511454012693041857206046113283949847762202 :: Natural)


        -- An EdDSA private key is a b-bit string k which should be chosen uniformly at random.
        -- The corresponding public key is A = s B, where s = H 0 , … , b − 1 (k) is the least significant b bits of H(k)
        -- interpreted as an integer in little-endian.
        --
        -- s512 is H(k)
        s512 :: ByteString 512 a
        s512 = sha2Natural @"SHA512" 256 k

        -- H 0 , … , b − 1 ( k ) are the least significant b bits of H(k)
        privateKeyBS :: ByteString 256 a
        privateKeyBS = truncate $ s512 `rotateBitsR` 256

        privateKey :: UInt 256 a
        privateKey = from privateKeyBS

        publicKey :: ByteString 256 a
        publicKey = from $ privateKey * b



        r' :: ByteString 1780 a
        r' = (extend messageBits) || (extend privateKeyBS `shiftBitsL` 1524)

        rBS :: ByteString 512 a
        rBS = sha2 @"SHA512" r'

        r :: ByteString 256 a
        r = truncate rBS

        -- This is the lowercase r, a secret integer r = hash(hash(privKey) + msg)
        rInt :: UInt 256 a
        rInt = from r

        rCapital :: ByteString 256 a
        rCapital = from $ rInt * b



        fullMsg :: ByteString 2036 a
        fullMsg = (extend rCapital `shiftBitsL` 1780) || (extend publicKey `shiftBitsL` 1524) || extend messageBits

        h :: ByteString 512 a
        h = sha2 @"SHA512" fullMsg

        hInt :: UInt 256 a
        hInt = from $ (truncate h :: ByteString 256 a)

type F = Zp BLS12_381_Scalar

specSignature :: IO ()
specSignature = hspec $ describe "Signature verificarion" $ do
    it' "Verifies a message with a valid signature" $ do
        seed <- toss (2 ^ (256 :: Natural))
        message <- arbitrary
        let (pubKey, r, s) = sign seed message
        return $ verifyFiatTransferSignature @F pubKey message (r, s) === true
    it' "Does not verify a message with an invalid signature (message was changed)" $ do
        seed <- toss (2 ^ (256 :: Natural))
        message <- arbitrary
        let (pubKey, r, s) = sign seed message
        newMessage <- arbitrary
        return $ verifyFiatTransferSignature @F pubKey newMessage (r, s) === false
    it' "Does not verify a message with an invalid signature (public key was changed)" $ do
        seed <- toss (2 ^ (256 :: Natural))
        message <- arbitrary
        let (pubKey, r, s) = sign seed message
        mask <- fromConstant <$> toss (2 ^ (256 :: Natural))
        return $ verifyFiatTransferSignature @F (pubKey `xor` mask) message (r, s) === false
    it' "Does not verify a message with an invalid signature (r was changed)" $ do
        seed <- toss (2 ^ (256 :: Natural))
        message <- arbitrary
        let (pubKey, r, s) = sign seed message
        mask <- fromConstant <$> toss (2 ^ (256 :: Natural))
        return $ verifyFiatTransferSignature @F pubKey message (r + mask, s) === false
    it' "Does not verify a message with an invalid signature (s was changed)" $ do
        seed <- toss (2 ^ (256 :: Natural))
        message <- arbitrary
        let (pubKey, r, s) = sign seed message
        mask <- fromConstant <$> toss (2 ^ (256 :: Natural))
        return $ verifyFiatTransferSignature @F pubKey message (r, s + mask) === false


