module Main where

import qualified Data.ByteString               as BS
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           PlutusTx.Prelude              hiding (error)
import           Prelude                       (IO, String, error, putStr, show)
import           System.Directory              (createDirectoryIfMissing,
                                                getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.Crypto.Utils   (extractKey)
import           ZkFold.Cardano.OffChain.Utils (dataToCBOR)
import           ZkFold.Cardano.P2P.Example    (paymentInfoHashEx1)
import           ZkFold.Cardano.Parse.Utils    (parseInteger)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))


sellerOnRampDatum :: String -> BS.ByteString -> Integer -> Integer  -> OnRampDatum
sellerOnRampDatum sellerName sellerPubKey sellPrice lovelaceSold =
  OnRampDatum { paymentInfoHash   = paymentInfoHashEx1 sellerName
              , sellPriceUsd      = sellPrice
              , valueSold         = lovelaceValue . Lovelace $ lovelaceSold
              , sellerPubKeyBytes = toBuiltin sellerPubKey
              , buyerPubKeyHash   = Nothing
              , timelock          = Nothing
              }

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "p2p"      -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  let assetsPath = path </> "assets"
      keysPath   = path </> "e2e-test" </> "p2p" </> "keys"

  createDirectoryIfMissing True $ path </> "assets"

  argsRaw <- getArgs

  case argsRaw of
    (sellerName : sellPriceStr : lovelaceSoldStr : _) -> do
      vkSellerE <- extractKey (keysPath </> (sellerName ++ ".vkey"))  -- Get seller's public key

      let argsE = do
            vkSeller     <- vkSellerE
            sellPrice    <- parseInteger sellPriceStr
            lovelaceSold <- parseInteger lovelaceSoldStr

            return (vkSeller, sellPrice, lovelaceSold)

      case argsE of
        Right (vkSeller, sellPrice, lovelaceSold) -> do
          BS.writeFile (assetsPath </> (sellerName ++ "SellDatum.cbor")) $ dataToCBOR $
            sellerOnRampDatum sellerName vkSeller sellPrice lovelaceSold

          putStr $ "Wrote " ++ sellerName ++ "SellDatum.cbor\n\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: expected three command-line arguments.\n"
