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
import           P2POnRamp.Api.Seller         (SignRequest, verifyH)

-- | Type for our Servant API.
type API = "verify" :> ReqBody '[JSON] SignRequest
                    :> Post    '[PlainText] Text
      :<|> "own-addr" :> ReqBody '[JSON] OwnAddresses
                      :> Post    '[JSON] OwnPubKeyBytes

-- | Server Handler
server :: Ctx -> FilePath -> ServerT API IO
server _ _ = verifyH
        :<|> handleOwnAddr

appApi :: Proxy API
appApi = Proxy

app :: Ctx -> FilePath -> Application
app ctx path = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $
  serve appApi $ hoistServer appApi (Handler . ExceptT . try) $
  server ctx path
