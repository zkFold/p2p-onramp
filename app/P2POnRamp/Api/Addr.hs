{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}


module P2POnRamp.Api.Addr where

import           Data.Text             (Text)
import           GeniusYield.GYConfig  (GYCoreConfig (..))
import           GeniusYield.Types
import           Prelude

import           P2POnRamp.Api.Context (Ctx (..), badRequest, onRampPolicy)


--------------------------------------------------------------------------------
-- Handler: get OnRamp address

handleGetOnRampAddr :: Ctx -> IO Text
handleGetOnRampAddr ctx = do
  case onRampPolicy $ ctxOnRampParams ctx of
    Left err -> badRequest err
    Right onRampScript -> do
      let nid            = cfgNetworkId $ ctxCoreCfg ctx
          onRampAddress' = addressFromValidator nid onRampScript
          onRampAddress  = addressToText onRampAddress'

      return onRampAddress
