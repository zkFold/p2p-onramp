{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module P2POnRamp.Api.BuyerClaim where

import           Control.Monad                 (void)
import           Crypto.PubKey.Ed25519
import           Data.Aeson
import qualified Data.ByteArray                as BA
-- import qualified Data.ByteString               as BS
import           Data.Default
import           Data.Maybe                    (fromJust, isJust, isNothing)
import           Data.List                     (find)
import           Data.String                   (fromString)
-- import qualified Data.ByteString.Base16        as B16
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig                   (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V3            as V3
-- import           PlutusTx                      (makeIsDataIndexed)
import           PlutusTx                      (makeIsDataIndexed)
import           Prelude
import           System.FilePath               ((</>))

import           P2POnRamp.Api.Context         (Ctx (..), dbFile, internalErr, notFoundErr, decodeOnRampParams, readDB)
import           P2POnRamp.Api.Tx              (AddSubmitParams (..), SubmitTxResult (..), UnsignedTxResponse, txBodySubmitTxResult, unSignedTxWithFee)
import           P2POnRamp.OrdersDB            (DB (..), Order (..), SellerInfo (..), setCompletedIfNull, setFiatSignatureIfNull)
import qualified P2POnRamp.OrdersDB            as DB (CompletedType (Claim))
import           P2POnRamp.Utils               (hexToBuiltin', toHexText)
import           ZkFold.Cardano.Crypto.Utils   (extractSecretKey)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..), OnRampRedeemer (..), onRampCompiled)


-- | Order whose fiat deposit is to be verified.
data FiatVerify = FiatVerify { fvOrderID :: !Int }
  deriving (Show, Generic)

instance FromJSON FiatVerify

-- | Signature verifying fiat deposit.
data FiatVerified = FiatVerified { fvSig :: !T.Text }
  deriving (Show, Generic)

instance ToJSON FiatVerified

data SellerInfoBBS = SellerInfoBBS
  { sellerName'    :: BuiltinByteString
  , sellerAccount' :: BuiltinByteString
  } deriving Show

makeIsDataIndexed ''SellerInfoBBS [('SellerInfoBBS, 0)]

fromSellerInfo :: SellerInfo -> SellerInfoBBS
fromSellerInfo (SellerInfo nm acc) = SellerInfoBBS nm' acc'
  where nm'  = toBuiltin $ TE.encodeUtf8 nm
        acc' = toBuiltin $ TE.encodeUtf8 acc

fiatSkeyPath :: FilePath  -- ToDo: to be given as parameter
fiatSkeyPath = "keys" </> "alice.skey"

handleFiatSign :: FilePath -> FiatVerify -> IO FiatVerified
handleFiatSign path FiatVerify{..} = do
  dbE <- readDB (path </> dbFile)
  case dbE of
    Left err -> internalErr err
    Right db -> do
      let req :: Order -> Bool
          req o = orderID o == fvOrderID && isJust (buyPostTx o) && isNothing (completedData o)
      sellerInfo <- case find req $ orders db of
                      Nothing -> notFoundErr "Sell order not found"
                      Just o  -> pure $ sellerInfo o

      skFiatE <- extractSecretKey (path </> fiatSkeyPath)
      case skFiatE of
        Left err     -> internalErr err
        Right skFiat -> do
          let vkFiat   = toPublic skFiat
              infoHash = dataToBlake $ fromSellerInfo sellerInfo
              sig      = sign skFiat vkFiat . fromBuiltin . dataToBlake $ infoHash
              sigHex   = toHexText $ BA.convert sig

          void $ setFiatSignatureIfNull path fvOrderID sigHex
          -- BS.writeFile (path </> "fiatSignature.bin") $ BA.convert sig
          -- return . FiatVerified . TE.decodeUtf8 . B16.encode $ BA.convert sig
          return $ FiatVerified sigHex

data ClaimCrypto = ClaimCrypto
  { ccBuyerAddrs :: ![GYAddress]
  , ccChangeAddr :: !GYAddress
  , ccOrderID    :: !Int
  } deriving (Show, Generic)

instance FromJSON ClaimCrypto

handleBuildClaimTx :: Ctx -> FilePath -> ClaimCrypto -> IO UnsignedTxResponse
handleBuildClaimTx Ctx{..} path ClaimCrypto{..} = do
  dbE <- readDB (path </> dbFile)
  case dbE of
    Left err -> internalErr err
    Right db -> do
      let nid           = cfgNetworkId ctxCoreCfg
          providers     = ctxProviders
          buyerAddress  = head ccBuyerAddrs

      let ctxParamsE = do
            onRampParams <- decodeOnRampParams ctxOnRampParams
            feeAddress'  <- either (Left . show) Right . addressFromPlutus nid $
                            feeAddress onRampParams
            feeValue'    <- either (Left . show) Right . valueFromPlutus $
                            feeValue onRampParams
            return (onRampParams, feeAddress', feeValue')

      case ctxParamsE of
        Left err -> internalErr err
        Right (onRampParams, feeAddress', feeValue') -> do
          let req :: Order -> Bool
              req o = orderID o == ccOrderID && isJust (buyPostTx o)
                      && isJust (fiatSignature o) && isNothing (completedData o)
          (selectedTxId, fiatSig) <- case find req $ orders db of
            Nothing -> notFoundErr "Buy order not found."
            Just o  -> do
              let txid = fromJust $ buyPostTx o

              sig <- case hexToBuiltin' . fromJust $ fiatSignature o of
                       Left err -> internalErr $ "Corrupted signature: " ++ err
                       Right s  -> pure s
              return (txid, sig)

          let selectedOref = fromString $ T.unpack selectedTxId ++ "#0"

          mutxo <- runGYTxQueryMonadIO nid
                                       providers $
                                       utxoAtTxOutRef selectedOref
          selectedUtxo <- case mutxo of
                            Nothing -> notFoundErr "Missing UTxO for selected order"
                            Just u  -> pure u

          buyerPKH <- addressToPubKeyHashIO buyerAddress

          let redeemer = redeemerFromPlutusData . toBuiltinData $ Claim fiatSig

          let onRampScript       = scriptFromPlutus @PlutusV3 $ onRampCompiled onRampParams
              onRampPlutusScript = GYBuildPlutusScriptInlined @PlutusV3 onRampScript
              onRampWit          = GYTxInWitnessScript onRampPlutusScript Nothing redeemer

          let skeleton = mustHaveInput (GYTxIn selectedOref onRampWit)
                      <> mustHaveOutput (GYTxOut buyerAddress (utxoValue selectedUtxo) Nothing Nothing)
                      <> mustHaveOutput (GYTxOut feeAddress' feeValue' Nothing Nothing)
                      <> mustBeSignedBy buyerPKH

          txBody <- runGYTxBuilderMonadIO nid
                                          providers
                                          ccBuyerAddrs
                                          ccChangeAddr
                                          Nothing $
                                          buildTxBody skeleton

          return $ unSignedTxWithFee txBody

handleSubmitClaimTx :: Ctx -> FilePath -> AddSubmitParams -> IO SubmitTxResult
handleSubmitClaimTx Ctx{..} path AddSubmitParams{..} = do
  let txBody = getTxBody aspTxUnsigned
  txid <- gySubmitTx ctxProviders $ makeSignedTransaction aspTxWit txBody
  void $ gyAwaitTxConfirmed ctxProviders def txid
  let submitResult = txBodySubmitTxResult txBody
  void . setCompletedIfNull (path </> dbFile) aspOrderID DB.Claim . T.pack $ show txid
  return submitResult
