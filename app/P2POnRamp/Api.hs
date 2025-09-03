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
                                               OwnAddresses, UnsignedTxResponse,
                                               handleOwnAddr)
import           P2POnRamp.Api.Addr           (handleGetOnRampAddr)
import           P2POnRamp.Api.Seller         (SellerOK, handleMessage, handleSigned, SellerData, handleSellerData, NewOrder)

-- | Type for our Servant API.
type API = "message" :> Get '[PlainText] Text
      :<|> "signed" :> ReqBody '[JSON] SellerOK
                    :> Post '[JSON] SellerOK
      :<|> "seller-data" :> ReqBody '[JSON] SellerData
                         :> Post '[JSON] NewOrder
      :<|> "onramp-addr" :> Get '[PlainText] Text
      :<|> "own-addr" :> ReqBody '[JSON] OwnAddresses
                      :> Post    '[JSON] OwnPubKeyBytes

-- | Server Handler
server :: Ctx -> FilePath -> ServerT API IO
server ctx path = handleMessage :<|> handleSigned
        :<|> handleSellerData path
        :<|> handleGetOnRampAddr ctx
        :<|> handleOwnAddr

appApi :: Proxy API
appApi = Proxy

app :: Ctx -> FilePath -> Application
app ctx path = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $
  serve appApi $ hoistServer appApi (Handler . ExceptT . try) $
  server ctx path
