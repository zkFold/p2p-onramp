module Main where

import           GeniusYield.GYConfig     (coreConfigIO, withCfgProviders)
import           Network.Wai.Handler.Warp
import           Prelude
import           System.Directory         (createDirectoryIfMissing)
import           System.Environment       (getArgs)
import           System.FilePath          ((</>))

import           P2POnRamp.Api            (app)
import           P2POnRamp.Api.Context    (Ctx (..))


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

  putStrLn "parsing Config ..."
  coreCfgPath <- parseArgs
  coreCfg     <- coreConfigIO coreCfgPath

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server" $ \providers -> do
    let port = 8080
        ctx  = Ctx coreCfg providers
    putStrLn $ "Serving on http://localhost:" ++ show port
    run port $ app ctx assetsPath


{-
import           PlutusLedgerApi.V1.Value (lovelaceValue)
import           PlutusLedgerApi.V3       (Address, BuiltinByteString, Lovelace (..), Value)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..))
import           ZkFold.Cardano.OffChain.Utils (parseAddress)
import           ZkFold.Cardano.Parse.Utils

-- | Getting path for our core configuration.
parseArgs :: IO (Either String (FilePath, Address, Value))
parseArgs = do
  argsRaw <- getArgs

  case argsRaw of
    (coreCfg : feeAddrStr : feeValStr : _) -> pure $ do
        feeAddr <- parseAddress feeAddrStr
        feeVal  <- lovelaceValue . Lovelace <$> parseInteger feeValStr
        pure (coreCfg, feeAddr, feeVal)
      
    _invalidArgument -> fail "Error: wrong arguments, needed\n  1) a path to the CoreConfig JSON configuration file\n  2) a fee address\n  3) a fee value (lovelaces)\n"

main :: IO ()
main = do
  let path       = "."
      assetsPath = path </> "assets"
  createDirectoryIfMissing True assetsPath

  putStrLn "parsing configuration..."
  argsE <- parseArgs
  case argsE of
    Right (coreCfgPath, feeAddress, feeValue) -> do
      coreCfg <- coreConfigIO coreCfgPath

      putStrLn $ show feeAddress
      putStrLn $ show feeValue

      putStrLn "Loading Providers ..."
      withCfgProviders coreCfg "api-server" $ \providers -> do
        let port = 8080
            ctx  = Ctx coreCfg providers -- $ OnRampParams feeAddress feeValue
        putStrLn $ "Serving on http://localhost:" ++ show port
        run port $ app ctx assetsPath

    Left err -> fail err
-}
