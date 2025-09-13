{-# LANGUAGE TypeOperators #-}

module P2POnRamp.Api where

import           Control.Exception           (try)
import           Control.Monad.Trans.Except  (ExceptT (..))
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Middleware.Cors
import           Prelude
import           Servant

import           P2POnRamp.Api.Context        (Ctx (..))
import           P2POnRamp.Api.BuyerCommit    (BuyCommit, handleBuildBuyTx, handleSubmitBuyTx)
import           P2POnRamp.Api.BuyerClaim     (FiatVerify, FiatVerified, ClaimCrypto, handleFiatSign, handleBuildClaimTx, handleSubmitClaimTx)
import           P2POnRamp.Api.Seller         (CancelOrder, CancelOrderResponse, NewOrder, SellerData, SellOrder, SellerTx,
                                               handleBuildCancelTx, handleBuildSellTx, handleSellerData, handleSellOrders, handleSubmitCancelTx, handleSubmitSellTx)
import           P2POnRamp.Api.Tx             (AddSubmitParams, SubmitTxResult, UnsignedTxResponse)


-- | Type for our Servant API.
type API = "seller-data" :> ReqBody '[JSON] SellerData
                         :> Post '[JSON] NewOrder
      :<|> "sell-build" :> ReqBody '[JSON] SellerTx
                        :> Post '[JSON] UnsignedTxResponse
      :<|> "sell-submit" :> ReqBody '[JSON] AddSubmitParams
                         :> Post '[JSON] SubmitTxResult
      :<|> "cancel-build" :> ReqBody '[JSON] CancelOrder
                          :> Post '[JSON] CancelOrderResponse
      :<|> "cancel-submit" :> ReqBody '[JSON] AddSubmitParams
                           :> Post '[JSON] SubmitTxResult
      :<|> "sell-orders" :> Get '[JSON] [SellOrder]
      :<|> "buy-build" :> ReqBody '[JSON] BuyCommit
                       :> Post '[JSON] UnsignedTxResponse
      :<|> "buy-submit" :> ReqBody '[JSON] AddSubmitParams
                        :> Post '[JSON] SubmitTxResult
      :<|> "fiat-verify" :> ReqBody '[JSON] FiatVerify
                         :> Post '[JSON] FiatVerified
      :<|> "claim-build" :> ReqBody '[JSON] ClaimCrypto
                         :> Post '[JSON] UnsignedTxResponse
      :<|> "claim-submit" :> ReqBody '[JSON] AddSubmitParams
                          :> Post '[JSON] SubmitTxResult

-- | Server Handler
server :: Ctx -> FilePath -> ServerT API IO
server ctx path = handleSellerData path
             :<|> handleBuildSellTx ctx path
             :<|> handleSubmitSellTx ctx path
             :<|> handleBuildCancelTx ctx path
             :<|> handleSubmitCancelTx ctx path
             :<|> handleSellOrders ctx path
             :<|> handleBuildBuyTx ctx path
             :<|> handleSubmitBuyTx ctx path
             :<|> handleFiatSign path
             :<|> handleBuildClaimTx ctx path
             :<|> handleSubmitClaimTx ctx path

appApi :: Proxy API
appApi = Proxy

app :: Ctx -> FilePath -> Application
app ctx path = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $
  serve appApi $ hoistServer appApi (Handler . ExceptT . try) $
  server ctx path
