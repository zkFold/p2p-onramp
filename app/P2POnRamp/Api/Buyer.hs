{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module P2POnRamp.Api.Buyer where

-- import           Cardano.Api                            (AssetName (..),
--                                                          parseAddressAny)
-- import           Control.Exception                      (throwIO)
import           Control.Monad                 (void)
import           Data.Aeson
import qualified Data.ByteString               as BS
-- import qualified Data.ByteString.Char8                  as BS8
-- import qualified Data.ByteString.Lazy                   as BL
-- import           Data.Coerce                            (coerce)
-- import qualified Data.Map.Strict                        as Map
import           Data.Default
import           Data.Maybe                    (fromJust)
import           Data.List                     (find)
-- import           Data.Maybe                             (fromJust)
import           Data.String                            (fromString)
import qualified Data.ByteString.Base16        as B16
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig                   (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V3            as V3
import           Prelude
import           System.FilePath                        ((</>))
-- import           Test.QuickCheck.Arbitrary              (Arbitrary (..))
-- import           Test.QuickCheck.Gen                    (generate)
-- import           Text.Parsec                            (parse)

import           P2POnRamp.Api.Context                  (Ctx (..), badRequest, dbFile, notFoundErr, onRampPolicy, readDB)
import           P2POnRamp.Api.Tx              (SubmitTxResult (..), UnsignedTxResponse, txBodySubmitTxResult, unSignedTxWithFee)
import           P2POnRamp.OrdersDB            (DB (..), Order (..), setBuyPostTxIfNull)
import           P2POnRamp.Utils               (hexToBuiltin)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..), OnRampRedeemer (..))

-- import           ZkFold.Cardano.OffChain.Utils          (byteStringAsHex)
-- import qualified ZkFold.Cardano.OnChain.BLS12_381.F     as F
import           ZkFold.Cardano.OnChain.Utils           (dataToBlake)


-- | Buyer's public key and selected sell-order.
data BuyCommit = BuyCommit
  { bcBuyerAddrs :: ![GYAddress]
  , bcChangeAddr :: !GYAddress
  , bcOrderID    :: !Int
  } deriving (Show, Generic)

instance FromJSON BuyCommit
instance ToJSON BuyCommit

-- | ToDo: decide on a mechanism to set time increment.
timeInc :: Integer
timeInc = 600  -- 10 minutes

-- ToDo: implement seller signing

testSig :: BuiltinByteString  -- DEBUG
testSig = case hexToBuiltin "71530aa2cdd8ae2df23cc586301e9c57e7107025b1f85d6a8bc75d127f0c584eb15f17a6b1c4a729d92abe82e3ef19c1ea21f965557557db59d9add6b443c301" of
  Left _  -> error $ "Unexpected: not an ex string"
  Right b -> b

-- | Updated 'OnRampDatum' for given UTxO, buyer's pub-key hash and time
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

handleBuyerCommit :: Ctx -> FilePath -> BuyCommit -> IO BuyCommit
handleBuyerCommit Ctx{..} path bc@BuyCommit{..} = do
  dbE <- readDB (path </> dbFile)
  case dbE of
    Left err -> badRequest err
    Right db -> do
      let nid           = cfgNetworkId ctxCoreCfg
          providers     = ctxProviders
          buyerAddress  = head bcBuyerAddrs

      selectedTxId <- case find (\o -> orderID o == bcOrderID) $ orders db of
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
      -- putStrLn $ "\nBuyer's pub key hash: " ++ (show buyerPKH) ++ "\n"  -- DEBUG

      orDatUpdated <- updateDatum selectedUtxo buyerPKH timeInc

      let orDatUpdatedHash = fromBuiltin $ dataToBlake orDatUpdated
      BS.writeFile (path </> "buyerCommit.bs") orDatUpdatedHash

      return bc

data BuyCommitHash = BuyCommitHash { hex :: T.Text }
  deriving (Show, Generic)

instance ToJSON BuyCommitHash

handleBuyCommitHash :: FilePath -> IO BuyCommitHash
handleBuyCommitHash path = do
  bs <- BS.readFile (path </> "buyerCommit.bin")
  return . BuyCommitHash . TE.decodeUtf8 $ B16.encode bs

handleBuildBuyTx :: Ctx -> FilePath -> BuyCommit -> IO UnsignedTxResponse
handleBuildBuyTx Ctx{..} path BuyCommit{..} = do
  dbE <- readDB (path </> dbFile)
  case dbE of
    Left err -> badRequest err
    Right db -> do
      let nid           = cfgNetworkId ctxCoreCfg
          providers     = ctxProviders
          buyerAddress  = head bcBuyerAddrs
          onRampScriptE = onRampPolicy ctxOnRampParams

      onRampScript <- case onRampScriptE of
        Left err     -> badRequest err  -- ToDo: change to 'internal error'
        Right script -> pure script

      let onRampAddr = addressFromValidator nid onRampScript

      selectedTxId <- case find (\o -> orderID o == bcOrderID) $ orders db of
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

      onRampDatumUpdated <- updateDatum selectedUtxo buyerPKH timeInc

      let orDatUpdatedHash = fromBuiltin $ dataToBlake onRampDatumUpdated  -- DEBUG: testing
      BS.writeFile (path </> "buyerCommit.bin") orDatUpdatedHash           -- DEBUG: testing

      let inlineDatumNew = Just ( datumFromPlutusData $ toBuiltinData onRampDatumUpdated
                                , GYTxOutUseInlineDatum @PlutusV3
                                )

      let redeemer = redeemerFromPlutusData . toBuiltinData $ Update testSig  -- DEBUG
      -- let redeemer = redeemerFromPlutusData . toBuiltinData $ Test  -- DEBUG

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
        cslot     <- slotOfCurrentBlock

        let upperSlot = fromJust $ advanceSlot cslot 300  -- allow five minutes to complete Tx submission
            skeleton  = skeleton' <> isInvalidAfter upperSlot

        buildTxBody skeleton

      return $ unSignedTxWithFee txBody

-- | Submit parameters to add for witness and order Id.  Assumption: frontend
-- honestly sends the correct order ID.
data AddBuySubmitParams = AddBuySubmitParams
  { abspTxUnsigned :: !GYTx
  , abspTxWit      :: !GYTxWitness
  , abspOrderID    :: !Int
  } deriving Generic

instance FromJSON AddBuySubmitParams

-- | Add key witness to the unsigned tx, submit tx and store txid.
handleSubmitBuyTx :: Ctx -> FilePath -> AddBuySubmitParams -> IO SubmitTxResult
handleSubmitBuyTx Ctx{..} path AddBuySubmitParams{..} = do
  let txBody = getTxBody abspTxUnsigned
  txid <- gySubmitTx ctxProviders $ makeSignedTransaction abspTxWit txBody
  void $ gyAwaitTxConfirmed ctxProviders def txid
  let submitResult = txBodySubmitTxResult txBody
  void . setBuyPostTxIfNull (path </> dbFile) abspOrderID . T.pack . show $ submitTxId submitResult
  return submitResult

--  return $ SubmitTxResult 123456 (fromString "deadbeef461a637bbdfc69d85d8ca22ff6fbfa27afafd5a2b151fb6853ceb5cf")  --DEBUG
