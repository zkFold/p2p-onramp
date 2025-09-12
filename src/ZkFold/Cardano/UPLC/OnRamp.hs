{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.UPLC.OnRamp where

import           GHC.Generics                 (Generic)
import           PlutusLedgerApi.V1.Interval  (after, before)
import           PlutusLedgerApi.V3
import           PlutusLedgerApi.V3.Contexts  (findOwnInput, txSignedBy)
import           PlutusTx                     (CompiledCode, compile,
                                               liftCodeDef, makeIsDataIndexed,
                                               makeLift, unsafeApplyCode)
import           PlutusTx.Prelude             hiding (toList, (*), (+))
import           Prelude                      (Show)

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)

data OnRampParams = OnRampParams
  { feeAddress      :: Address
  , feeValue        :: Value
  , fiatPubKeyBytes :: BuiltinByteString
  }
  deriving stock (Generic, Show)

makeLift ''OnRampParams
makeIsDataIndexed ''OnRampParams [('OnRampParams,0)]

data OnRampDatum = OnRampDatum
    { paymentInfoHash   :: BuiltinByteString
    , sellPriceUsd      :: Integer
    , valueSold         :: Value
    , sellerPubKeyBytes :: BuiltinByteString
    , buyerPubKeyHash   :: Maybe PubKeyHash
    , timelock          :: Maybe POSIXTime
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
onRamp _ (Update _signed) ctx =
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
    && sellerPubKeyBytes dat' == sellerPubKeyBytes dat
    && isJust (buyerPubKeyHash dat')
    -- The timelock must be in the future
    && maybe False (\t -> t `after` txInfoValidRange (scriptContextTxInfo ctx)) (timelock dat')

    -- Check the seller's signature
    -- && verifyEd25519Signature (sellerPubKeyBytes dat) (dataToBlake dat') signed
onRamp _ Cancel ctx =
  let
    -- Get the current on-ramp output
    (val, dat) = case findOwnInput ctx of
      Just (TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) Nothing)) -> (v, unsafeFromBuiltinData @OnRampDatum d)
      _ -> traceError "onRamp: missing input"

    -- Get the payment output
    val' = case head $ txInfoOutputs $ scriptContextTxInfo ctx of
      TxOut _ v NoOutputDatum Nothing -> v
      _                               -> traceError "onRamp: missing output"

    -- Derive seller's pub key hash
    sellerPubKeyHash = blake2b_224 $ sellerPubKeyBytes dat
  in
    -- The timelock is either not set or must be in the past
    maybe True (\t -> t `before` txInfoValidRange (scriptContextTxInfo ctx)) (timelock dat)

    -- Seller must sign the Tx and reclaim all the locked value
    && txSignedBy (scriptContextTxInfo ctx) (PubKeyHash sellerPubKeyHash)
    && val' == val
onRamp OnRampParams{..} (Claim sig) ctx  =
  let
    -- Get the current on-ramp output
    (val, dat) = case findOwnInput ctx of
      Just (TxInInfo _ (TxOut _ v (OutputDatum (Datum d)) Nothing)) -> (v, unsafeFromBuiltinData @OnRampDatum d)
      _ -> traceError "onRamp: missing input"

    -- Get the payment output
    val' = case head $ txInfoOutputs $ scriptContextTxInfo ctx of
      TxOut _ v NoOutputDatum Nothing -> v
      _                               -> traceError "onRamp: missing output"

    -- Get the fee output
    (addr'', val'') = case head $ tail $ txInfoOutputs $ scriptContextTxInfo ctx of
      TxOut a v NoOutputDatum Nothing -> (a, v)
      _                               -> traceError "onRamp: missing output"
  in
    -- Buyer must sign the Tx and claim all the locked value
    maybe False (txSignedBy $ scriptContextTxInfo ctx) (buyerPubKeyHash dat)
    && val' == val

    -- The fiat payment processor must sign the hash of the fiat payment info
    -- TODO: this can be replaced with a ZK proof of the fiat payment
    && verifyEd25519Signature fiatPubKeyBytes (dataToBlake $ paymentInfoHash dat) sig

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

{-# INLINABLE onRampCompiled #-}
onRampCompiled :: OnRampParams -> CompiledCode (BuiltinData -> BuiltinUnit)
onRampCompiled computation =
  $$(compile [|| untypedOnRamp ||])
  `unsafeApplyCode` liftCodeDef computation
