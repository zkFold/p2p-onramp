module Main where

import           Data.Aeson               (eitherDecodeFileStrict)
import           GeniusYield.GYConfig     (coreConfigIO, withCfgProviders)
import           Network.Wai.Handler.Warp
import           Prelude
import           System.Directory         (createDirectoryIfMissing)
import           System.Environment       (getArgs)
import           System.FilePath          ((</>))

import           P2POnRamp.Api            (app)
import           P2POnRamp.Api.Context    (Ctx (..), dbFile)
import           P2POnRamp.OrdersDB


-- | Getting path for our core configuration.
parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    coreCfg: _       -> return coreCfg
    _invalidArgument -> fail "Error: wrong arguments, needed a path to the CoreConfig JSON configuration file\n"

main :: IO ()
main = do
  let path       = "."
      assetsPath = path </> "assets"
  createDirectoryIfMissing True assetsPath

  let dbPath = assetsPath </> dbFile
  initDB dbPath

  putStrLn "parsing Config ..."
  coreCfgPath <- parseArgs
  coreCfg     <- coreConfigIO coreCfgPath

  putStrLn "parsing OnRamp parameters..."
  orParamsE <- eitherDecodeFileStrict (path </> "onramp-params.json")
  let orParams' = case orParamsE of
        Left  err    -> error $ "Error decoding JSON: " ++ err
        Right params -> params

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server" $ \providers -> do
    let port = 8080
        ctx  = Ctx coreCfg providers orParams'
    putStrLn $ "Serving on http://localhost:" ++ show port
    run port $ app ctx assetsPath
