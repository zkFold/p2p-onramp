{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}


module P2POnRamp.Api.Seller where

import           Control.Monad                 (void)
import           Control.Exception             (throwIO)
import           Data.Aeson                    (FromJSON (..), ToJSON (..), object, withObject, (.=), (.:))
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteArray                as BA
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as LBS
import           Data.Default
import           Data.List                     (find)
import           Data.Maybe                    (isJust, isNothing)
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GeniusYield.GYConfig          (GYCoreConfig (..))
import           GeniusYield.Types
import           GeniusYield.TxBuilder
import           GHC.Generics                  (Generic)
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           PlutusTx                      (makeIsDataIndexed)
import           Prelude
import           Servant
import           System.FilePath               ((</>))
import           Crypto.Error                  (CryptoFailable(..))
import qualified Crypto.PubKey.Ed25519         as Ed25519

import           P2POnRamp.Api.Context         (Ctx (..), dbFile, onRampPolicy, readDB)
import           P2POnRamp.Api.Tx              (UnsignedTxResponse (..), AddSubmitParams (..), SubmitTxResult (..), txBodySubmitTxResult, unSignedTxWithFee)
import           P2POnRamp.OrdersDB            (Order (..), SellerInfo (..), IniInfo (..),
                                                createOrder, setSellPostTxIfNull, DB (..), setCompletedIfNull)
import qualified P2POnRamp.OrdersDB            as DB (CompletedType (Cancel))
import           P2POnRamp.Utils               (hexToBuiltin', posixToMillis, toHexText)
import           ZkFold.Cardano.Crypto.Utils   (eitherHexToKey)
import           ZkFold.Cardano.OffChain.Utils (dataToJSON)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..), OnRampRedeemer (..))

expectedMessage :: ByteString
expectedMessage = "Hello messenger."

newtype SPKB = SPKB BuiltinByteString

sellerPKB :: BS.ByteString -> SPKB
sellerPKB sellerPubKey = SPKB $ toBuiltin sellerPubKey

--------------------------------------------------------------------------------
-- Helpers

badRequest :: String -> IO a
badRequest msg = throwIO $ err400 { errBody = LBS.fromStrict (BSC.pack msg) }

notFoundErr :: String -> IO a
notFoundErr msg = throwIO $ err404 { errBody = LBS.fromStrict (BSC.pack msg) }

internalErr :: String -> IO a
internalErr msg = throwIO $ err500 { errBody = LBS.fromStrict (BSC.pack msg) }

unauthorizedText :: Text -> IO Text
unauthorizedText t = throwIO $ err401 { errBody = LBS.fromStrict (TE.encodeUtf8 t) }

decodeHexStrict :: Text -> Either String ByteString
decodeHexStrict = B16.decode . TE.encodeUtf8

decodeHexSized :: Int -> Text -> String -> Either String ByteString
decodeHexSized n t what = do
  bs <- decodeHexStrict t
  if BS.length bs == n
    then Right bs
    else Left $ what <> " must be exactly " <> show n <> " bytes (got " <> show (BS.length bs) <> ")"

hex :: ByteString -> Text
hex = TE.decodeUtf8 . B16.encode


--------------------------------------------------------------------------------
-- Handlers: message & sign

data VKey = VKey
  { vkType :: Text
  , description :: Text
  , cborHex :: Text
  } deriving Show

instance FromJSON VKey where
  parseJSON = withObject "VKey" $ \o ->
    VKey <$> o .: "type"
         <*> o .: "description"
         <*> o .: "cborHex"

instance ToJSON VKey where
  toJSON (VKey t d c) =
    object [ "type"        .= t
           , "description" .= d
           , "cborHex"     .= c
           ]

data SellerOK = SellerOK
  { message :: Text
  , signatureHex :: Text
  , vkeyJson :: VKey
  , scheme :: Text
  , note :: Text
  } deriving (Show, Generic)

instance FromJSON SellerOK
instance ToJSON SellerOK

handleMessage :: IO Text
handleMessage = pure "hello"

handleSigned :: SellerOK -> IO SellerOK
handleSigned = pure


--------------------------------------------------------------------------------
-- Handler: seller data

data SellerData = SellerData
  { sdName          :: Text
  , sdAccount       :: Text
  , sdSellAda       :: Integer
  , sdPrice         :: Integer
  , sdSellerPKBytes :: Text
  } deriving (Show, Generic)

instance FromJSON SellerData
instance ToJSON SellerData

data SellerInfoBBS = SellerInfoBBS
  { sellerName'    :: BuiltinByteString
  , sellerAccount' :: BuiltinByteString
  } deriving Show

makeIsDataIndexed ''SellerInfoBBS [('SellerInfoBBS, 0)]

data NewOrder = NewOrder { newID :: Int, newDatum :: Aeson.Value }
  deriving (Show, Generic)

instance FromJSON NewOrder
instance ToJSON NewOrder

fromSellerInfo :: SellerInfo -> SellerInfoBBS
fromSellerInfo (SellerInfo nm acc) = SellerInfoBBS nm' acc'
  where nm'  = toBuiltin $ TE.encodeUtf8 nm
        acc' = toBuiltin $ TE.encodeUtf8 acc

mkInitialDatum :: SellerInfo -> IniInfo -> IO OnRampDatum
mkInitialDatum seller IniInfo{..} = do
  let paymentInfoHash    = dataToBlake $ fromSellerInfo seller
      sellPriceUsd       = iiPriceUsd
      valueSold          = lovelaceValue . Lovelace $ iiSellAda * 1000000
      sellerPubKeyBytesE = hexToBuiltin' iiSellerPKBytes

  case sellerPubKeyBytesE of
    Left err   -> badRequest err
    Right spkb -> pure $
      OnRampDatum paymentInfoHash sellPriceUsd valueSold spkb Nothing Nothing

handleSellerData :: FilePath -> SellerData -> IO NewOrder
handleSellerData path SellerData{..} = do
  let sellerVKeyE = do
        pkBytes <- eitherHexToKey $ T.unpack sdSellerPKBytes
        case Ed25519.publicKey pkBytes of
          CryptoPassed vk -> Right vk
          CryptoFailed _  -> Left "Malformed public key"

  case sellerVKeyE of
    Left err -> badRequest err
    Right sellerVKey -> do
      let sellerInfo = SellerInfo sdName sdAccount
          iniInfo    = IniInfo sdSellAda sdPrice . toHexText $ BA.convert sellerVKey

      sellerDatum <- mkInitialDatum sellerInfo iniInfo
      newOrder    <- createOrder (path </> dbFile) sellerInfo iniInfo

      return $ NewOrder (orderID newOrder) (dataToJSON sellerDatum)


--------------------------------------------------------------------------------
-- Handlers: seller's Tx

data SellerTx = SellerTx
  { stSellerAddrs :: ![GYAddress]
  , stChangeAddr  :: !GYAddress
  , stOrderID     :: !Int
  } deriving (Show, Generic)

instance FromJSON SellerTx

handleBuildSellTx :: Ctx -> FilePath -> SellerTx -> IO UnsignedTxResponse
handleBuildSellTx Ctx{..} path SellerTx{..} = do
  dbE <- readDB (path </> dbFile)
  case dbE of
    Left err -> internalErr err
    Right db -> do
      let nid           = cfgNetworkId ctxCoreCfg
          providers     = ctxProviders
          sellerAddress = head stSellerAddrs
          onRampScriptE = onRampPolicy ctxOnRampParams

      onRampScript <- case onRampScriptE of
        Left err     -> internalErr err
        Right script -> pure script

      let onRampAddr = addressFromValidator nid onRampScript

      let req :: Order -> Bool
          req o = orderID o == stOrderID && isNothing (sellPostTx o)
      (sell, ini) <- case find req $ orders db of
        Nothing -> notFoundErr "Order not found"
        Just o  -> pure (sellerInfo o, iniInfo o)

      onRampDatum <- mkInitialDatum sell ini

      sellValue <- case valueFromPlutus $ valueSold onRampDatum of
        Left err -> internalErr $ "Unable to retrieve value sold: " ++ (show err)
        Right v  -> pure v

      let inlineDatum = Just ( datumFromPlutusData $ toBuiltinData onRampDatum
                             , GYTxOutUseInlineDatum @PlutusV3
                             )

      sellerPKH   <- addressToPubKeyHashIO sellerAddress

      let skeleton = mustHaveOutput (GYTxOut onRampAddr sellValue inlineDatum Nothing)
                  <> mustBeSignedBy sellerPKH

      txBody <- runGYTxBuilderMonadIO nid
                                      providers
                                      stSellerAddrs
                                      stChangeAddr
                                      Nothing $
                                      buildTxBody skeleton

      return $ unSignedTxWithFee txBody

-- | Add key witness to the unsigned tx, submit tx and store txid.
handleSubmitSellTx :: Ctx -> FilePath -> AddSubmitParams -> IO SubmitTxResult
handleSubmitSellTx Ctx{..} path AddSubmitParams{..} = do
  let txBody = getTxBody aspTxUnsigned
  txid <- gySubmitTx ctxProviders $ makeSignedTransaction aspTxWit txBody
  void $ gyAwaitTxConfirmed ctxProviders def txid
  let submitResult = txBodySubmitTxResult txBody
  void . setSellPostTxIfNull (path </> dbFile) aspOrderID . T.pack $ show txid
  return submitResult


--------------------------------------------------------------------------------
-- Handlers: get orders DB (ToDo: move somewhere else)

data OrderPair = OrderPair
  { opLovelace :: Integer
  , opPriceUsd :: Integer
  } deriving (Show, Eq, Generic)

instance FromJSON OrderPair
instance ToJSON OrderPair

data SellOrder = SellOrder
  { soOrderID  :: Int
  , soHasBuyer :: Bool
  , soInfo     :: SellerInfo
  , soPair     :: Maybe OrderPair
  } deriving (Show, Eq, Generic)

instance FromJSON SellOrder
instance ToJSON SellOrder

filterOrdersBySell :: [Order] -> [(GYTxOutRef, SellOrder)]
filterOrdersBySell os =
  [ (fromString $ T.unpack txid ++ "#0", SellOrder orderID (isJust buyPostTx) sellerInfo Nothing)
  | Order{ orderID
         , sellerInfo
         , sellPostTx    = Just txid
         , buyPostTx
         , completedData = Nothing
         } <- os
  ]

sellOrders :: Ctx -> (GYTxOutRef, SellOrder) -> IO SellOrder
sellOrders Ctx{..} (oref, so) = do
  let nid       = cfgNetworkId ctxCoreCfg
      providers = ctxProviders

  mutxo <- runGYTxQueryMonadIO nid
                               providers $
                               utxoAtTxOutRef oref

  let mop = do
        utxo <- mutxo
        let dat = outDatumToPlutus $ utxoOutDatum utxo

        orDat' <- case dat of
          OutputDatum d -> Just $ getDatum d
          _             -> Nothing
        orDat  <- fromBuiltinData @OnRampDatum orDat'

        let sellLovelace = valueAssetClass (utxoValue utxo) GYLovelace
            priceUsd     = sellPriceUsd orDat

        pure $ OrderPair sellLovelace priceUsd

  return $ so { soPair = mop }

handleSellOrders :: Ctx -> FilePath -> IO [SellOrder]
handleSellOrders ctx path = do
  dbE <- readDB (path </> dbFile)
  case dbE of
    Left err -> badRequest err
    Right db -> mapM (sellOrders ctx) (filterOrdersBySell $ orders db)
      

--------------------------------------------------------------------------------
-- Handler: cancel order

data CancelOrder = CancelOrder
  { coSellerAddrs :: ![GYAddress]
  , coChangeAddr  :: !GYAddress
  , coOrderID     :: !Int
  } deriving (Show, Generic)

instance FromJSON CancelOrder

data CancelOrderResponse = COFail Natural  -- Too early to cancel (milliseconds remaining)
                         | COSucc UnsignedTxResponse
  deriving (Show, Generic)

instance ToJSON CancelOrderResponse

handleBuildCancelTx :: Ctx -> FilePath -> CancelOrder -> IO CancelOrderResponse
handleBuildCancelTx Ctx{..} path CancelOrder{..} = do
  dbE <- readDB (path </> dbFile)
  case dbE of
    Left err -> internalErr err
    Right db -> do
      let nid           = cfgNetworkId ctxCoreCfg
          providers     = ctxProviders
          sellerAddress = head coSellerAddrs
          onRampScriptE = onRampPolicy ctxOnRampParams

      onRampScript <- case onRampScriptE of
        Left err     -> internalErr err
        Right script -> pure script

      let req :: Order -> Bool
          req o = orderID o == coOrderID && isNothing (completedData o)
      selectedTxId <- case find req $ orders db of
        Nothing -> notFoundErr "Order not found"
        Just o  -> case buyPostTx o of
          Just txid -> pure txid
          Nothing   -> case sellPostTx o of
            Nothing   -> notFoundErr "Sell order not yet onchain"
            Just txid -> pure txid

      let selectedOref = fromString $ T.unpack selectedTxId ++ "#0"

      mutxo <- runGYTxQueryMonadIO nid
                                   providers $
                                   utxoAtTxOutRef selectedOref
      selectedUtxo <- case mutxo of
                        Nothing -> notFoundErr "Missing UTxO for selected order"
                        Just u  -> pure u

      let mt1 = do
            let dat = outDatumToPlutus $ utxoOutDatum selectedUtxo
            orDat'       <- case dat of
              OutputDatum d -> Just $ getDatum d
              _             -> Nothing
            orDat        <- fromBuiltinData @OnRampDatum orDat'
            POSIXTime t1 <- timelock orDat
            return $ t1

      t0' <- getCurrentGYTime
      let timePadding = 30000  -- 30 seconds
          t0          = (posixToMillis $ timeToPOSIX t0') - timePadding
      case mt1 of
        Just t1 | t0 <= t1 -> return . COFail . fromInteger $ t1 - t0

        _ -> do
          sellerPKH <- addressToPubKeyHashIO sellerAddress

          let redeemer = redeemerFromPlutusData . toBuiltinData $ Cancel

          let onRampPlutusScript = GYBuildPlutusScriptInlined @PlutusV3 onRampScript
              onRampWit          = GYTxInWitnessScript onRampPlutusScript Nothing redeemer

          let skeleton' = mustHaveInput (GYTxIn selectedOref onRampWit)
                       <> mustHaveOutput (GYTxOut sellerAddress (utxoValue selectedUtxo) Nothing Nothing)
                       <> mustBeSignedBy sellerPKH

          txBody <- runGYTxBuilderMonadIO nid
                                          providers
                                          coSellerAddrs
                                          coChangeAddr
                                          Nothing $ do
            cslot <- slotOfCurrentBlock
            let skeleton = skeleton' <> isInvalidBefore cslot
            buildTxBody skeleton

          return . COSucc $ unSignedTxWithFee txBody

handleSubmitCancelTx :: Ctx -> FilePath -> AddSubmitParams -> IO SubmitTxResult
handleSubmitCancelTx Ctx{..} path AddSubmitParams{..} = do
  let txBody = getTxBody aspTxUnsigned
  txid <- gySubmitTx ctxProviders $ makeSignedTransaction aspTxWit txBody
  void $ gyAwaitTxConfirmed ctxProviders def txid
  let submitResult = txBodySubmitTxResult txBody
  void . setCompletedIfNull (path </> dbFile) aspOrderID DB.Cancel . T.pack $ show txid
  return submitResult
