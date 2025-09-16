{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module P2POnRamp.Api.SellOrders where

import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Maybe                 (isJust)
import           Data.String                (fromString)
import qualified Data.Text                  as T
import           GeniusYield.GYConfig       (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics               (Generic)
import           PlutusLedgerApi.V3         as V3
import           Prelude
import           System.FilePath            ((</>))

import           P2POnRamp.Api.Context      (Ctx (..), dbFile)
import           P2POnRamp.OrdersDB         (Order (..), SellerInfo (..),
                                             readOrdersDB)
import           ZkFold.Cardano.UPLC.OnRamp (OnRampDatum (..))


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
  orders <- readOrdersDB (path </> dbFile)
  mapM (sellOrders ctx) $ filterOrdersBySell orders
