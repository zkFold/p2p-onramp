{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.OnRamp where

import           GHC.Generics                             (Generic)
import           PlutusLedgerApi.V1.Interval              (before, after)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts              (findOwnInput)
import           PlutusTx                                 (makeIsDataIndexed)
import           PlutusTx.Prelude                         hiding (toList, (*), (+))
import           Prelude                                  (Show)

import           ZkFold.Cardano.OnChain.Utils             (dataToBlake)

data OnRampParams = OnRampParams
  { feeAddress          :: Address
  , feeValue            :: Value
  , fiatPubKeyHashBytes :: BuiltinByteString
  }
  deriving stock (Generic, Show)

makeIsDataIndexed ''OnRampParams [('OnRampParams,0)]

data OnRampDatum = OnRampDatum
    { paymentInfoHash  :: BuiltinByteString
    , sellPriceUsd     :: Integer
    , valueSold        :: Value
    , sellerPubKeyHash :: PubKeyHash
    , buyerPubKeyHash  :: Maybe PubKeyHash
    , timelock         :: Maybe POSIXTime
    }
    deriving stock (Generic, Show)

makeIsDataIndexed ''OnRampDatum [('OnRampDatum,0)]

data OnRampRedeemer
  = Update BuiltinByteString
  -- ^ Update the bid with the buyer's public key hash and the timelock.
  | Claim BuiltinByteString
  -- ^ Buyer claims the value.
  | Cancel
  -- ^ Seller cancels the value selling.
  deriving stock (Generic, Show)

makeIsDataIndexed ''OnRampRedeemer [('Cancel,0),('Update,1),('Claim,2)]

-- | Plutus script for a trustless P2P on-ramp.
{-# INLINABLE onRamp #-}
onRamp :: OnRampParams -> OnRampRedeemer -> ScriptContext -> Bool
onRamp _ (Update sig) ctx =
  let
    -- Get the current on-ramp output
    (addr, val, dat) = case findOwnInput ctx of
      Just (TxInInfo _ (TxOut a v (OutputDatum (Datum d)) Nothing)) -> (a, v, unsafeFromBuiltinData @OnRampDatum d)
      _ -> traceError "onRamp: missing input"

    -- Get the next on-ramp output
    (addr', val', dat') = case head $ txInfoOutputs $ scriptContextTxInfo ctx of
      TxOut a v (OutputDatum (Datum d)) _ -> (a, v, unsafeFromBuiltinData @OnRampDatum d)
      _ -> traceError "onRamp: missing output"
  in
    -- Check the current on-ramp output
    isNothing (buyerPubKeyHash dat)
    

    -- Check the next on-ramp output
    && addr' == addr
    && val' == val
    && paymentInfoHash dat' == paymentInfoHash dat
    && sellPriceUsd dat' == sellPriceUsd dat
    && sellerPubKeyHash dat' == sellerPubKeyHash dat
    && isJust (buyerPubKeyHash dat')
    -- The timelock must be in the future
    && maybe False (\t -> t `after` txInfoValidRange (scriptContextTxInfo ctx)) (timelock dat')

    -- Check the seller's signature
    && verifyEd25519Signature (getPubKeyHash $ sellerPubKeyHash dat) (dataToBlake dat') sig
onRamp _ Cancel ctx =
  let
    -- Get the current on-ramp output
    (val, dat) = case findOwnInput ctx of
      Just (TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) Nothing)) -> (v, unsafeFromBuiltinData @OnRampDatum d)
      _ -> traceError "onRamp: missing input"

    -- Get the payment output
    (addr', val') = case head $ txInfoOutputs $ scriptContextTxInfo ctx of
      TxOut a v NoOutputDatum Nothing -> (a, v)
      _ -> traceError "onRamp: missing output"
  in
    -- The timelock is either not set or must be in the past
    maybe True (\t -> t `before` txInfoValidRange (scriptContextTxInfo ctx)) (timelock dat)

    -- The value must be returned to the seller
    && addr' == Address (PubKeyCredential $ sellerPubKeyHash dat) Nothing
    && val' == val
onRamp OnRampParams{..} (Claim sig) ctx =
  let
    -- Get the current on-ramp output
    (val, dat) = case findOwnInput ctx of
      Just (TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) Nothing)) -> (v, unsafeFromBuiltinData @OnRampDatum d)
      _ -> traceError "onRamp: missing input"

    -- Get the payment output
    (addr', val') = case head $ txInfoOutputs $ scriptContextTxInfo ctx of
      TxOut a v NoOutputDatum Nothing -> (a, v)
      _ -> traceError "onRamp: missing output"

    -- Get the fee output
    (addr'', val'') = case head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx of
      TxOut a v NoOutputDatum Nothing -> (a, v)
      _ -> traceError "onRamp: missing output"
  in
    -- The value must be sent to the buyer
    maybe False (\a -> addr' == Address (PubKeyCredential a) Nothing) (buyerPubKeyHash dat)
    && val' == val

    -- The fiat payment processor must sign the hash of the fiat payment info
    -- TODO: this can be replaced with a ZK proof of the fiat payment
    && verifyEd25519Signature fiatPubKeyHashBytes (dataToBlake $ paymentInfoHash dat) sig

    -- The fee must be sent to the fee address
    && addr'' == feeAddress
    && val'' == feeValue

{-# INLINABLE untypedOnRamp #-}
untypedOnRamp :: OnRampParams -> BuiltinData -> BuiltinUnit
untypedOnRamp computation ctx' =
  let
    ctx      = unsafeFromBuiltinData ctx'
    redeemer = unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx
  in
    check $ onRamp computation redeemer ctx
