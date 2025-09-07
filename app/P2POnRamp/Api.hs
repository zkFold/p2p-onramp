{-# LANGUAGE TypeOperators #-}

module P2POnRamp.Api where

import           Control.Exception           (try)
import           Control.Monad.Trans.Except  (ExceptT (..))
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Middleware.Cors
import           Prelude
import           Servant
import           Data.Text                    (Text)

import           P2POnRamp.Api.Context        (Ctx (..), OwnPubKeyBytes,
                                               OwnAddresses, handleOwnAddr)
import           P2POnRamp.Api.Addr           (handleGetOnRampAddr)
import           P2POnRamp.Api.Buyer          (AddBuySubmitParams (..), BuyCommit, BuyCommitHash, handleBuyCommitHash, handleBuildBuyTx, handleSubmitBuyTx)
import           P2POnRamp.Api.Seller         (SellerOK, handleMessage, handleSigned, SellerData, handleSellerData, NewOrder, SellerTx, handleSellerTx, SellOrder (..), handleSellOrders)
import           P2POnRamp.Api.Tx             (SubmitTxResult, UnsignedTxResponse)


-- | Type for our Servant API.
type API = "message" :> Get '[PlainText] Text
      :<|> "signed" :> ReqBody '[JSON] SellerOK
                    :> Post '[JSON] SellerOK
      :<|> "seller-data" :> ReqBody '[JSON] SellerData
                         :> Post '[JSON] NewOrder
      :<|> "onramp-addr" :> Get '[PlainText] Text
      :<|> "post-seller-txid" :> ReqBody '[JSON] SellerTx
                              :> Post '[JSON] Bool
      :<|> "sell-orders" :> Get '[JSON] [SellOrder]
      :<|> "buy-commit" :> Get '[JSON] BuyCommitHash
      :<|> "buy-build" :> ReqBody '[JSON] BuyCommit
                       :> Post '[JSON] UnsignedTxResponse
      :<|> "buy-submit" :> ReqBody '[JSON] AddBuySubmitParams
                        :> Post '[JSON] SubmitTxResult
      :<|> "own-addr" :> ReqBody '[JSON] OwnAddresses
                      :> Post    '[JSON] OwnPubKeyBytes

-- | Server Handler
server :: Ctx -> FilePath -> ServerT API IO
server ctx path = handleMessage :<|> handleSigned
        :<|> handleSellerData path
        :<|> handleGetOnRampAddr ctx
        :<|> handleSellerTx path
        :<|> handleSellOrders ctx path
        :<|> handleBuyCommitHash path
        :<|> handleBuildBuyTx ctx path
        :<|> handleSubmitBuyTx ctx path
        :<|> handleOwnAddr

appApi :: Proxy API
appApi = Proxy

app :: Ctx -> FilePath -> Application
app ctx path = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $
  serve appApi $ hoistServer appApi (Handler . ExceptT . try) $
  server ctx path
