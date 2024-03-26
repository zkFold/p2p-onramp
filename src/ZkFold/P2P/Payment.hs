module ZkFold.P2P.Payment where

import ZkFold.Base.Data.Vector
import ZkFold.Symbolic.Data.Char
import ZkFold.Symbolic.Data.UInt

data CardanoPaymentCredentials a
  = ShellyPaymentCredentials (Vector 56 (UInt 8 a))
  -- TODO: | DaedalusPaymentCredentials (...)

data PaymentData a = PaymentData
    { pdSender :: CardanoPaymentCredentials a
    , pdReceiever :: CardanoPaymentCredentials a
    , pdAmount :: UInt 64 a
    , pdCurrency :: Vector 8 (ASCIIChar a)
    }

-- TODO: instance Arithmetic a => Arithmetizable a (PaymentData (ArithmeticCircuit a)) where
