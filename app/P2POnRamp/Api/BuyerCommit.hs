{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module P2POnRamp.Api.BuyerCommit where

import           Control.Monad              (void)
import           Data.Aeson
import           Data.Default
import           Data.List                  (find)
import           Data.Maybe                 (fromJust)
import           Data.String                (fromString)
import qualified Data.Text                  as T
import           GeniusYield.GYConfig       (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V3         as V3
import           Prelude
import           System.FilePath            ((</>))

import           P2POnRamp.Api.Context      (Ctx (..), badRequest, dbFile,
                                             internalErr, notFoundErr,
                                             onRampValidator)
import           P2POnRamp.Api.Tx           (AddSubmitParams (..),
                                             SubmitTxResult (..),
                                             UnsignedTxResponse,
                                             txBodySubmitTxResult,
                                             unSignedTxWithFee)
import           P2POnRamp.OrdersDB         (Order (..), readOrdersDB,
                                             setBuyPostTxIfNull)
import           P2POnRamp.Utils            (hexToBuiltin)
import           ZkFold.Cardano.UPLC.OnRamp (OnRampDatum (..),
                                             OnRampRedeemer (..))


-- | Buyer's public key and selected sell-order.
data BuyCommit = BuyCommit
  { bcBuyerAddrs :: ![GYAddress]
  , bcChangeAddr :: !GYAddress
  , bcOrderID    :: !Int
  } deriving (Show, Generic)

instance FromJSON BuyCommit
instance ToJSON BuyCommit

-- | Update 'OnRampDatum' for given UTxO, buyer's pub-key hash and time
-- increment in seconds.
updateDatum :: GYUTxO -> GYPubKeyHash -> Integer -> IO OnRampDatum
updateDatum utxo pkh tInc = do
  let morDat = do
        let dat = outDatumToPlutus $ utxoOutDatum utxo
        orDat' <- case dat of
          OutputDatum d -> Just $ getDatum d
          _             -> Nothing
        orDat  <- fromBuiltinData @OnRampDatum orDat'
        pure orDat

  case morDat of
    Nothing -> badRequest "Unexpected: datum missing"
    Just d  -> do
      t0 <- getCurrentGYTime
      let t1  = addSeconds t0 $ fromInteger tInc
          t1' = timeToPlutus t1
          d'  = d { buyerPubKeyHash = Just $ pubKeyHashToPlutus pkh
                  , timelock = Just t1'
                  }
      return d'

testSig :: BuiltinByteString
testSig = case hexToBuiltin "71530aa2cdd8ae2df23cc586301e9c57e7107025b1f85d6a8bc75d127f0c584eb15f17a6b1c4a729d92abe82e3ef19c1ea21f965557557db59d9add6b443c301" of
  Left _  -> error $ "Unexpected: not an ex string"
  Right b -> b

handleBuildBuyTx :: Ctx -> FilePath -> BuyCommit -> IO UnsignedTxResponse
handleBuildBuyTx Ctx{..} path BuyCommit{..} = do
  let nid           = cfgNetworkId ctxCoreCfg
      providers     = ctxProviders
      buyerAddress  = head bcBuyerAddrs
      onRampScriptE = onRampValidator ctxOnRampParams

  onRampScript <- case onRampScriptE of
    Left err     -> internalErr err
    Right script -> pure script

  let onRampAddr = addressFromValidator nid onRampScript

  orders <- readOrdersDB (path </> dbFile)

  selectedTxId <- case find (\o -> orderID o == bcOrderID) orders of
                    Nothing -> notFoundErr "Sell order not found"
                    Just o  -> case sellPostTx o of
                                 Nothing   -> notFoundErr "Sell order not yet onchain"
                                 Just txid -> pure txid

  let selectedOref = fromString $ T.unpack selectedTxId ++ "#0"

  mutxo <- runGYTxQueryMonadIO nid
                               providers $
                               utxoAtTxOutRef selectedOref
  selectedUtxo <- case mutxo of
                    Nothing -> notFoundErr "Missing UTxO for selected order"
                    Just u  -> pure u

  buyerPKH <- addressToPubKeyHashIO buyerAddress

  onRampDatumUpdated <- updateDatum selectedUtxo buyerPKH ctxClaimGracePeriod

  let inlineDatumNew = Just ( datumFromPlutusData $ toBuiltinData onRampDatumUpdated
                            , GYTxOutUseInlineDatum @PlutusV3
                            )

  let redeemer = redeemerFromPlutusData . toBuiltinData $ Update testSig

  let onRampPlutusScript = GYBuildPlutusScriptInlined @PlutusV3 onRampScript
      onRampWit = GYTxInWitnessScript onRampPlutusScript Nothing redeemer

  let skeleton' = mustHaveInput (GYTxIn selectedOref onRampWit)
               <> mustHaveOutput (GYTxOut onRampAddr (utxoValue selectedUtxo) inlineDatumNew Nothing)
               <> mustBeSignedBy buyerPKH

  txBody <- runGYTxBuilderMonadIO nid
                                  providers
                                  bcBuyerAddrs
                                  bcChangeAddr
                                  Nothing $ do
    cslot <- slotOfCurrentBlock

    let upperSlot = fromJust $ advanceSlot cslot 300  -- allow five minutes to complete Tx submission
        skeleton  = skeleton' <> isInvalidAfter upperSlot

    buildTxBody skeleton

  return $ unSignedTxWithFee txBody

-- | Add key witness to the unsigned tx, submit tx and store txid.
handleSubmitBuyTx :: Ctx -> FilePath -> AddSubmitParams -> IO SubmitTxResult
handleSubmitBuyTx Ctx{..} path AddSubmitParams{..} = do
  let txBody = getTxBody aspTxUnsigned
  txid <- gySubmitTx ctxProviders $ makeSignedTransaction aspTxWit txBody
  void $ gyAwaitTxConfirmed ctxProviders def txid
  let submitResult = txBodySubmitTxResult txBody
  void . setBuyPostTxIfNull (path </> dbFile) aspOrderID . T.pack $ show txid
  return submitResult
