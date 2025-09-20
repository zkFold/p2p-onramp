{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Crypto.PubKey.Ed25519        (SecretKey, generateSecretKey,
                                               sign, toPublic)
import qualified Data.ByteArray               as BA
import qualified Data.ByteString              as BS
import qualified PlutusLedgerApi.V1.Value     as V1
import           PlutusLedgerApi.V3
import           PlutusTx.AssocMap            (empty)
import           PlutusTx.Prelude             hiding (pure, (<$>))
import           Prelude                      (IO, Int, pure, (<$>))
import           Test.Tasty                   (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck        as QC

import           ZkFold.Cardano.OnChain.Utils (dataToBlake)
import           ZkFold.Cardano.UPLC.OnRamp

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

-- | Construct a trivial ADA value
lovelace :: Integer -> Value
lovelace = V1.lovelaceValue . V1.Lovelace

-- | Dummy script address used by the input and (for Update) the next-state output
scriptAddr :: Address
scriptAddr = Address (ScriptCredential (ScriptHash "deadbeefdeadbeefdeadbeefdeadbeef")) Nothing

-- | PubKey address from a PubKeyHash
pkhAddr :: PubKeyHash -> Address
pkhAddr pkh = Address (PubKeyCredential pkh) Nothing

-- | Build a TxOut carrying an inline datum
outWithDatum :: Address -> Value -> OnRampDatum -> TxOut
outWithDatum a v dat =
  TxOut a v (OutputDatum (Datum (toBuiltinData dat))) Nothing

-- | Build a plain payment TxOut
payOut :: Address -> Value -> TxOut
payOut a v = TxOut a v NoOutputDatum Nothing

-- | Build a ScriptContext with:
--   - exactly one input (from OnRamp script), identified by 'ownRef'
--   - the provided 'outputs'
--   - the provided validity range
mkCtx :: TxOutRef -> TxOut -> [TxOut] -> [PubKeyHash] -> POSIXTimeRange -> ScriptContext
mkCtx ownRef ownResolved outputs signatories validRange =
  let
    input = TxInInfo ownRef ownResolved
    txInfo = TxInfo
      { txInfoInputs = [input]
      , txInfoReferenceInputs = []
      , txInfoOutputs = outputs
      , txInfoFee = V1.Lovelace 500000
      , txInfoMint = mempty
      , txInfoTxCerts = []
      , txInfoWdrl = empty
      , txInfoValidRange = validRange
      , txInfoSignatories = signatories
      , txInfoRedeemers = empty
      , txInfoData = empty
      , txInfoId = "abcd"
      , txInfoVotes = empty
      , txInfoProposalProcedures = []
      , txInfoCurrentTreasuryAmount = Nothing
      , txInfoTreasuryDonation = Nothing
      }
    unitRedeemer = Redeemer $ toBuiltinData ()
    purpose = SpendingScript ownRef Nothing
  in
    ScriptContext txInfo unitRedeemer purpose

-- | A made-up input reference
ownRef0 :: TxOutRef
ownRef0 = TxOutRef "feedbeefcafecafe" 0

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genPosix :: Gen POSIXTime
genPosix = POSIXTime <$> chooseInteger (0, 10_000_000)

genRangeBefore :: POSIXTime -> Gen POSIXTimeRange
genRangeBefore t = oneof
  [ pure $ to (t - 1)                                              -- (-inf, t-1]
  , do dt <- POSIXTime <$> chooseInteger (2, 1000)
       pure $ Interval (lowerBound (t - dt)) (upperBound (t - 1))  -- [t-dt, t-1]
  ]

genValue :: Gen Value
genValue = lovelace <$> chooseInteger (1_000_000, 50_000_000)

genBS :: Int -> Gen BuiltinByteString
genBS n = do
  bytes <- vectorOf n $ chooseInteger (0, 255)
  pure $ foldr consByteString emptyByteString bytes

genPKH :: Gen PubKeyHash
genPKH = genBS 28 >>= pure . PubKeyHash

genParams :: Gen OnRampParams
genParams = do
  feeAddress <- pure $ Address (ScriptCredential (ScriptHash "fee0000000000000000000000000000")) Nothing
  feeValue   <- lovelace <$> chooseInteger (10_000, 100_000)
  fiatPubKeyBytes <- genBS 32
  pure OnRampParams{feeAddress, feeValue, fiatPubKeyBytes}

genDatum :: Gen OnRampDatum
genDatum = do
  paymentInfoHash   <- genBS 32
  sellPriceUsd      <- chooseInteger (1, 100_000)
  valueSold         <- genValue
  sellerPubKeyBytes <- genBS 32
  buyerPubKeyHash   <- frequency [ (3, pure Nothing)
                                 , (1, Just <$> genPKH)
                                 ]
  timelock          <- frequency [ (2, pure Nothing)
                                 , (1, Just . POSIXTime <$> chooseInteger (0, 10_000_000))
                                 ]
  pure OnRampDatum{paymentInfoHash, sellPriceUsd, valueSold, sellerPubKeyBytes, buyerPubKeyHash, timelock}

--------------------------------------------------------------------------------
-- Set fields on datums for scenarios
--------------------------------------------------------------------------------

setBuyer :: PubKeyHash -> OnRampDatum -> OnRampDatum
setBuyer pkh d = d { buyerPubKeyHash = Just pkh }

setNoBuyer :: OnRampDatum -> OnRampDatum
setNoBuyer d = d { buyerPubKeyHash = Nothing }

setTimelock :: Maybe POSIXTime -> OnRampDatum -> OnRampDatum
setTimelock mt d = d { timelock = mt }

--------------------------------------------------------------------------------
-- Signing data
--------------------------------------------------------------------------------

fromBA :: BA.ByteArrayAccess a => a -> BuiltinByteString
fromBA = toBuiltin @BS.ByteString . BA.convert

signedData :: ToData a => SecretKey -> a -> BuiltinByteString
signedData skey dat = fromBA sigBytes
  where
    vkey = toPublic skey
    sigBytes = sign skey vkey . fromBuiltin $ dataToBlake dat

--------------------------------------------------------------------------------
-- UPDATE properties
--------------------------------------------------------------------------------

-- Happy path: buyer & time-lock added
prop_Update_accepts_on_buyer_addition :: Property
prop_Update_accepts_on_buyer_addition =
  forAll genParams                 $ \p ->
  forAll (setNoBuyer <$> genDatum) $ \d0 ->
  forAll genValue                  $ \v ->
  forAll genPosix                  $ \tNow ->
  forAll (genRangeBefore tNow)     $ \vr ->
  forAll genPKH                    $ \buyer ->
  ioProperty $ do
    sellerSkey <- generateSecretKey
    let sellerVkeyBS = fromBA $ toPublic sellerSkey
        d0'          = d0 { sellerPubKeyBytes = sellerVkeyBS }
        d1           = setTimelock (Just tNow) $ setBuyer buyer d0'
        inOut        = outWithDatum scriptAddr v d0'
        out1_good    = outWithDatum scriptAddr v d1
        ctx          = mkCtx ownRef0 inOut [out1_good] [buyer] vr
        sellerSig    = signedData sellerSkey d1
    pure $
      counterexample "Update should succeed on correct buyer & timelock addition" $
        onRamp p (Update sellerSig) ctx === True

-- If current datum already has a buyer, Update must fail.
prop_Update_rejects_when_buyer_already_set :: Property
prop_Update_rejects_when_buyer_already_set =
  forAll genParams                 $ \p ->
  forAll (setNoBuyer <$> genDatum) $ \d0 ->
  forAll genValue                  $ \v ->
  forAll genPosix                  $ \tNow ->
  forAll (genRangeBefore tNow)     $ \vr ->
  forAll genPKH                    $ \buyer ->
  ioProperty $ do
    sellerSkey <- generateSecretKey
    let sellerVkeyBS = fromBA $ toPublic sellerSkey
        d0'          = d0 { sellerPubKeyBytes = sellerVkeyBS }
        d0''         = setBuyer buyer d0'  -- current datum already has buyer
        d1           = setTimelock (Just tNow) d0''
        inOut        = outWithDatum scriptAddr v d0''
        out1_good    = outWithDatum scriptAddr v d1
        ctx          = mkCtx ownRef0 inOut [out1_good] [buyer] vr
        sellerSig    = signedData sellerSkey d1
    pure $
      counterexample "Update should fail if current datum already has a buyer" $
        onRamp p (Update sellerSig) ctx === False

-- Any change to immutable fields between dat and dat' must fail.
prop_Update_rejects_on_immutable_mutation :: Property
prop_Update_rejects_on_immutable_mutation =
  forAll genParams                 $ \p ->
  forAll (setNoBuyer <$> genDatum) $ \d0 ->
  forAll genValue                  $ \v ->
  forAll genPosix                  $ \tNow ->
  forAll (genRangeBefore tNow)     $ \vr ->
  forAll genPKH                    $ \buyer ->
  ioProperty $ do
    sellerSkey <- generateSecretKey
    let sellerVkeyBS = fromBA $ toPublic sellerSkey
        d0'          = d0 { sellerPubKeyBytes = sellerVkeyBS }
        d1           = setTimelock (Just tNow) $ setBuyer buyer d0'
        -- ToDo: can fuzz other fields similarly
        d1'          = d1 { paymentInfoHash = "MUTATED\NULHASH\NUL" }
        inOut        = outWithDatum scriptAddr v d0'
        out1_bad     = outWithDatum scriptAddr v d1'
        ctxBad       = mkCtx ownRef0 inOut [out1_bad] [buyer] vr
        sellerSig    = signedData sellerSkey d1'
    pure $
      counterexample "Update should fail if paymentInfoHash changes" $
        onRamp p (Update sellerSig) ctxBad === False

-- If timelock for the *next* datum isn't strictly in the future, Update must fail.
prop_Update_rejects_when_timelock_not_future :: Property
prop_Update_rejects_when_timelock_not_future =
  forAll genParams                 $ \p ->
  forAll (setNoBuyer <$> genDatum) $ \d0 ->
  forAll genValue                  $ \v ->
  forAll genPosix                  $ \tNow ->
  forAll genPKH                    $ \buyer ->
  ioProperty $ do
    sellerSkey <- generateSecretKey
    let sellerVkeyBS = fromBA $ toPublic sellerSkey
        d0'          = d0 { sellerPubKeyBytes = sellerVkeyBS }
        d1_bad       = setTimelock (Just tNow) $ setBuyer buyer d0'
        inOut        = outWithDatum scriptAddr v d0'
        out1_good    = outWithDatum scriptAddr v d1_bad
        -- Valid range that *starts* at tNow: 'after' will be False when equal boundary
        vr           = from tNow
        ctx          = mkCtx ownRef0 inOut [out1_good] [buyer] vr
        sellerSig    = signedData sellerSkey d1_bad
    pure $
      counterexample "Update should fail if timelock is not strictly after validRange" $
        onRamp p (Update sellerSig) ctx === False

--------------------------------------------------------------------------------
-- CANCEL properties
--------------------------------------------------------------------------------

-- Happy path: no timelock => must refund exact value to seller's PKH address.
prop_Cancel_refunds_exact_value_when_no_timelock :: Property
prop_Cancel_refunds_exact_value_when_no_timelock =
  forAll genDatum $ \d0_raw ->
  forAll genValue $ \v ->
  forAll genPosix $ \tNow ->
  let
    d0        = setTimelock Nothing d0_raw
    sellerPkh = PubKeyHash (blake2b_224 (sellerPubKeyBytes d0))
    inOut     = outWithDatum scriptAddr v d0
    refund    = payOut (pkhAddr sellerPkh) v
    vr        = from tNow
    ctx       = mkCtx ownRef0 inOut [refund] [sellerPkh] vr
    p         = OnRampParams { feeAddress = scriptAddr, feeValue = mempty, fiatPubKeyBytes = "fiatpk" }
  in
    counterexample "Cancel should succeed and refund exact value to seller when no timelock" $
      onRamp p Cancel ctx === True

-- If timelock exists and is not expired, Cancel must fail.
prop_Cancel_rejects_before_timelock_expires :: Property
prop_Cancel_rejects_before_timelock_expires =
  forAll genDatum $ \d0_raw ->
  forAll genValue $ \v ->
  forAll genPosix $ \tNow ->
  let
    d0        = setTimelock (Just (tNow + 10_000)) d0_raw  -- in the future
    sellerPkh = PubKeyHash (blake2b_224 (sellerPubKeyBytes d0))
    inOut     = outWithDatum scriptAddr v d0
    refund    = payOut (pkhAddr sellerPkh) v
    -- Valid range that does NOT begin after timelock
    vr        = from tNow
    ctx       = mkCtx ownRef0 inOut [refund] [sellerPkh] vr
    p         = OnRampParams { feeAddress = scriptAddr, feeValue = mempty, fiatPubKeyBytes = "fiatpk" }
  in
    counterexample "Cancel should fail before timelock expires" $
      onRamp p Cancel ctx === False

-- Any deviation in recipient or value should fail.
prop_Cancel_rejects_wrong_refund :: Property
prop_Cancel_rejects_wrong_refund =
  forAll genDatum $ \d0_raw ->
  forAll genValue $ \v ->
  forAll genPosix $ \tNow ->
  forAll genPKH   $ \wrongPkh ->
  let
    d0        = setTimelock Nothing d0_raw
    -- Wrong recipient or value (we pick recipient here)
    inOut     = outWithDatum scriptAddr v d0
    badRefund = payOut (pkhAddr wrongPkh) v
    vr        = from tNow
    ctx       = mkCtx ownRef0 inOut [badRefund] [wrongPkh] vr
    p         = OnRampParams { feeAddress = scriptAddr, feeValue = mempty, fiatPubKeyBytes = "fiatpk" }
  in
    counterexample "Cancel should fail if refund is not to seller" $
      onRamp p Cancel ctx === False

--------------------------------------------------------------------------------
-- CLAIM properties
--------------------------------------------------------------------------------

-- Happy claim
prop_Claim_accepts_normal_claim :: Property
prop_Claim_accepts_normal_claim =
  forAll genParams                 $ \p ->
  forAll (setNoBuyer <$> genDatum) $ \d0 ->
  forAll genValue                  $ \v ->
  forAll genPosix                  $ \tPast ->
  forAll genPKH                    $ \buyer ->
  ioProperty $ do
    fiatSkey <- generateSecretKey
    let fiatVkeyBS = fromBA $ toPublic fiatSkey
        p'         = p { fiatPubKeyBytes = fiatVkeyBS }
        d0'        = setBuyer buyer d0
        inOut      = outWithDatum scriptAddr v d0'
        outBuyer   = payOut (pkhAddr buyer) v
        outFee     = payOut (feeAddress p') (feeValue p')
        ctx        = mkCtx ownRef0 inOut [outBuyer, outFee] [buyer] (from tPast)
        fiatSig    = signedData fiatSkey $ paymentInfoHash d0'
    pure $
      counterexample "Normal claim should succeed" $
        onRamp p' (Claim fiatSig) ctx === True

-- Claim must fail if no buyer is set
prop_Claim_rejects_without_buyer :: Property
prop_Claim_rejects_without_buyer =
  forAll genParams                 $ \p ->
  forAll (setNoBuyer <$> genDatum) $ \d0 ->
  forAll genValue                  $ \v ->
  forAll genPosix                  $ \tPast ->
  ioProperty $ do
    fiatSkey <- generateSecretKey
    let fiatVkeyBS = fromBA $ toPublic fiatSkey
        p'         = p { fiatPubKeyBytes = fiatVkeyBS }
        inOut      = outWithDatum scriptAddr v d0
        -- Outputs shaped as if correct, but buyer is Nothing so it must fail
        dummyBuyer = PubKeyHash "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        outBuyer   = payOut (pkhAddr dummyBuyer) v
        outFee     = payOut (feeAddress p') (feeValue p')
        ctx        = mkCtx ownRef0 inOut [outBuyer, outFee] [dummyBuyer] (from tPast)
        fiatSig    = signedData fiatSkey $ paymentInfoHash d0
    pure $
      counterexample "Claim should fail if buyerPubKeyHash is Nothing" $
        onRamp p' (Claim fiatSig) ctx === False

-- Wrong fee address or value must fail
prop_Claim_rejects_wrong_fee :: Property
prop_Claim_rejects_wrong_fee =
  forAll genParams                 $ \p ->
  forAll (setNoBuyer <$> genDatum) $ \d0 ->
  forAll genValue                  $ \v ->
  forAll genPosix                  $ \tPast ->
  forAll genPKH                    $ \buyer ->
  ioProperty $ do
    fiatSkey <- generateSecretKey
    let fiatVkeyBS = fromBA $ toPublic fiatSkey
        p'         = p { fiatPubKeyBytes = fiatVkeyBS }
        -- Use a different fee address
        pBad       = p' { feeAddress = Address (ScriptCredential (ScriptHash "notfee000000000000000000000000")) Nothing }
        d0'        = setBuyer buyer d0
        inOut      = outWithDatum scriptAddr v d0'
        outBuyer   = payOut (pkhAddr buyer) v
        outFee     = payOut (feeAddress p') (feeValue p')
        ctx        = mkCtx ownRef0 inOut [outBuyer, outFee] [buyer] (from tPast)
        fiatSig    = signedData fiatSkey $ paymentInfoHash d0'
    pure $
      counterexample "Claim should fail if fee address is wrong" $
        onRamp pBad (Claim fiatSig) ctx === False

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "OnRamp validator (property tests without external signing)"
  [ testGroup "UPDATE"
    [ QC.testProperty "adds buyer & timelock normally"
        prop_Update_accepts_on_buyer_addition
    , QC.testProperty "rejects if current datum already has a buyer"
        prop_Update_rejects_when_buyer_already_set
    , QC.testProperty "rejects if immutable field changes between dat and dat'"
        prop_Update_rejects_on_immutable_mutation
    , QC.testProperty "rejects if timelock (next datum) is not strictly in the future"
        prop_Update_rejects_when_timelock_not_future
    ]
  , testGroup "CANCEL"
    [ QC.testProperty "refunds exact value to seller when no timelock"
        prop_Cancel_refunds_exact_value_when_no_timelock
    , QC.testProperty "rejects before timelock expires"
        prop_Cancel_rejects_before_timelock_expires
    , QC.testProperty "rejects if refund is not to seller"
        prop_Cancel_rejects_wrong_refund
    ]
  , testGroup "CLAIM"
    [ QC.testProperty "executes normal claim"
        prop_Claim_accepts_normal_claim
    , QC.testProperty "rejects when no buyer is set"
        prop_Claim_rejects_without_buyer
    , QC.testProperty "rejects when fee address is wrong"
        prop_Claim_rejects_wrong_fee
    ]
  ]
