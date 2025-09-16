module Main where

import           Data.Aeson               (eitherDecodeFileStrict)
import           GeniusYield.GYConfig     (coreConfigIO, withCfgProviders)
import           Network.Wai.Handler.Warp
import           Prelude
import           System.Directory         (createDirectoryIfMissing)
import           System.Environment       (getArgs)
import           System.FilePath          ((</>))

import           P2POnRamp.Api            (app)
import           P2POnRamp.Api.Context    (Ctx (..), OnRampConfig (..), dbFile)
import           P2POnRamp.OrdersDB


-- | Getting path for our core configuration.
parseArgs :: IO (FilePath, FilePath)
parseArgs = do
  args <- getArgs
  case args of
    coreCfg : onrampCfg : _ -> return (coreCfg, onrampCfg)
    _invalidArgument        -> fail "Error: wrong arguments, needed 1) path to the CoreConfig JSON configuration file and 2) path to the OnRamp JSON configuration file.\n"

main :: IO ()
main = do
  let path       = "."
      assetsPath = path </> "assets"
  createDirectoryIfMissing True assetsPath

  let dbPath = assetsPath </> dbFile
  initDB dbPath

  (coreCfgPath, onrampCfgPath) <- parseArgs

  putStrLn "parsing CoreConfig ..."
  coreCfg <- coreConfigIO coreCfgPath

  putStrLn "parsing OnRamp config..."
  orConfigE <- eitherDecodeFileStrict onrampCfgPath
  let orConfig = case orConfigE of
        Left  err -> error $ "Error decoding JSON: " ++ err
        Right cfg -> cfg

  putStrLn "Loading Proviers ..."
  withCfgProviders coreCfg "api-server" $ \providers -> do
    let port = 8080
        ctx  = Ctx { ctxCoreCfg          = coreCfg
                   , ctxProviders        = providers
                   , ctxOnRampParams     = orParams orConfig
                   , ctxFiatSKeyFilePath = orFiatSKeyFilePath orConfig
                   , ctxClaimGracePeriod = orClaimGracePeriod orConfig
                   }

    putStrLn $ "Serving on http://localhost:" ++ show port
    run port $ app ctx assetsPath
