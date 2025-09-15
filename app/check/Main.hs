{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.Maybe                 (fromJust)
import           GeniusYield.GYConfig       (GYCoreConfig (..), coreConfigIO,
                                             withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           PlutusLedgerApi.V3         as V3
import           Prelude
import           System.Directory           (createDirectoryIfMissing)
import           System.Environment         (getArgs)
import           System.FilePath            ((</>))

import           ZkFold.Cardano.UPLC.OnRamp (OnRampDatum (..))


data Ctx = Ctx
  { ctxCoreCfg   :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

-- | Getting path for our core configuration.
parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    coreCfg: _       -> return coreCfg
    _invalidArgument -> fail "Error: wrong arguments, needed a path to the CoreConfig JSON configuration file\n"

-- | Check datum at UTxO
handleCheck :: Ctx -> FilePath -> IO ()
handleCheck Ctx{..} _path = do
  let nid       = cfgNetworkId ctxCoreCfg
      providers = ctxProviders

  mutxo <- runGYTxQueryMonadIO nid
                               providers $
                               utxoAtTxOutRef ("655bdcfee42c213e7c163711699fcd3827546c6d70ef2546393b6e7114551669#0" :: GYTxOutRef)

  let theDatum' = utxoOutDatum $ fromJust mutxo
      theDatum  = outDatumToPlutus theDatum'

  let orDat = case theDatum of
        OutputDatum orDat' -> getDatum orDat'
        _                  -> error "not OutputDatum"

  let mORDat = fromBuiltinData @OnRampDatum orDat
  print mORDat

main :: IO ()
main = do
  let path       = "."
      assetsPath = path </> "assets"
  createDirectoryIfMissing True assetsPath

  putStrLn "parsing Config ..."
  coreCfgPath <- parseArgs
  coreCfg     <- coreConfigIO coreCfgPath

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "check" $ \providers -> do
    let ctx  = Ctx coreCfg providers
    handleCheck ctx path
