module Main where

import qualified Data.ByteString               as BS
import           P2P.Example                   (paymentInfoHashEx1)
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           Prelude                       (Bool (..), Either (..), IO,
                                                Integer, Maybe (..), error,
                                                return, show, ($), (++), (.))
import           System.Directory              (createDirectoryIfMissing,
                                                getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.OffChain.Utils (dataToCBOR, parseAddress)
import           ZkFold.Cardano.Parse.Utils    (parseInteger)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..))


sellerOnRampDatum :: Integer -> Integer -> V3.Address -> Integer -> OnRampDatum
sellerOnRampDatum sellPrice lovelaceSold sellerAddr deadline =
  let sellerPkh = case addressCredential sellerAddr of
        PubKeyCredential pkh -> pkh
        _                    -> error $ "Expected 'PubKeyCredential'"

  in OnRampDatum { paymentInfoHash  = paymentInfoHashEx1
                 , sellPriceUsd     = sellPrice
                 , valueSold        = lovelaceValue . Lovelace $ lovelaceSold
                 , sellerPubKeyHash = sellerPkh
                 , buyerPubKeyHash  = Nothing
                 , timelock         = Just $ POSIXTime deadline
                 }

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "p2p"      -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  let assetsPath = path </> "assets"

  createDirectoryIfMissing True $ path </> "assets"

  argsRaw <- getArgs

  case argsRaw of
    (sellerName : sellPriceStr : lovelaceSoldStr : sellerAddrStr : deadlineStr : _) -> do
      let argsE = do
            sellPrice    <- parseInteger sellPriceStr
            lovelaceSold <- parseInteger lovelaceSoldStr
            sellerAddr   <- parseAddress sellerAddrStr
            deadline     <- parseInteger deadlineStr

            return (sellPrice, lovelaceSold, sellerAddr, deadline)

      case argsE of
        Right params4 -> do
          BS.writeFile (assetsPath </> (sellerName ++ ".cbor")) $ dataToCBOR . uncurry4 sellerOnRampDatum $ params4

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: expected five command-line arguments.\n"


----- HELPER FUNCTIONS -----

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d
