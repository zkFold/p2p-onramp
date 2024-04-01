{-# LANGUAGE UndecidableInstances #-}
module ZkFold.P2P.Payment where

import           Prelude                        hiding (Bool, Eq ((==)), (*), (+), length, splitAt)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Data.Vector
import           ZkFold.Symbolic.Cardano.Types.Tx
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.UInt

data CardanoPaymentCredentials a
    = ShellyPaymentCredentials (Vector 56 (UInt 8 a))
  -- TODO: | DaedalusPaymentCredentials (...)

-- TODO
instance Arithmetizable a x => Arithmetizable a (CardanoPaymentCredentials x) where

-- Should include part of PAN, account number holder, probably with PCI DSS masking
data FiatAccount a = FiatAccount a

instance (Ring a, Finite a, MultiplicativeGroup a) => Eq (Bool a) (FiatAccount a)

-- TODO
instance Arithmetizable a x => Arithmetizable a (FiatAccount x) where

data ISO427 a = ISO427 a a a

instance Arithmetizable a x => Arithmetizable a (ISO427 x) where
    arithmetize (ISO427 o1 o2 o3) =
        (\o1 o2 o3 -> o1 <> o2 <> o3)
            <$> arithmetize o1
            <*> arithmetize o2
            <*> arithmetize o3

data Offer a = Offer
    { oFiatAccount :: FiatAccount a
    , oFiatAmount :: UInt 64 a
    , oFiatCurrency :: ISO427 a
    , oCardanoAddress :: CardanoPaymentCredentials a
    , oCardanoAmount :: UInt 64 a
    }

instance (Arithmetizable a (UInt 64 x), Arithmetizable a x) => Arithmetizable a (Offer x) where
    arithmetize (Offer fAccount fAmount fCurrency cAddress cAmount) =
        (\fac fam fc cad cam -> fac <> fam <> fc <> cad <> cam)
            <$> arithmetize fAccount
            <*> arithmetize fAmount
            <*> arithmetize fCurrency
            <*> arithmetize cAddress
            <*> arithmetize cAmount

zkSmartContract ::
    Eq (Bool a) (Offer a) =>
    Transaction input outputs Offer a -> Offer a -> Bool a
zkSmartContract tx offer = txDatum tx == offer
