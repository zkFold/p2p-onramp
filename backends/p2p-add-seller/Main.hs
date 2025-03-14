module Main where

import           Crypto.PubKey.Ed25519
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import           P2P.Example                   (paymentInfoHashEx1)
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           PlutusTx.Prelude              hiding (error)
import           Prelude                       (IO, String, error, show)
import           System.Directory              (createDirectoryIfMissing,
                                                getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.Crypto.Utils   (extractKey, extractSecretKey)
import           ZkFold.Cardano.OffChain.Utils (dataToCBOR)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.Parse.Utils    (parseInteger)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..), OnRampRedeemer (..))


sellerOnRampDatum :: String -> BS.ByteString -> Integer -> Integer  -> Integer -> OnRampDatum
sellerOnRampDatum sellerName sellerPubKey sellPrice lovelaceSold deadline =
  OnRampDatum { paymentInfoHash   = paymentInfoHashEx1 sellerName
              , sellPriceUsd      = sellPrice
              , valueSold         = lovelaceValue . Lovelace $ lovelaceSold
              , sellerPubKeyBytes = toBuiltin sellerPubKey
              , buyerPubKeyHash   = Nothing
              , timelock          = Just $ POSIXTime deadline
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
    (fiatAdminName : sellerName : sellPriceStr : lovelaceSoldStr : deadlineStr : _) -> do
      skFiatE   <- extractSecretKey (keysPath </> (fiatAdminName ++ ".skey"))  -- Get fiat admin's private key
      vkSellerE <- extractKey (keysPath </> (sellerName ++ ".vkey"))           -- Get seller's public key

      let argsE = do
            skFiat       <- skFiatE
            vkSeller     <- vkSellerE
            sellPrice    <- parseInteger sellPriceStr
            lovelaceSold <- parseInteger lovelaceSoldStr
            deadline     <- parseInteger deadlineStr

            return (skFiat, vkSeller, sellPrice, lovelaceSold, deadline)

      case argsE of
        Right (skFiat, vkSeller, sellPrice, lovelaceSold, deadline) -> do
          let vkFiat = toPublic skFiat

          -- Fiat admin signs fiat payment info
          let sig = sign skFiat vkFiat . fromBuiltin . dataToBlake . paymentInfoHashEx1 $ sellerName

          let claimRedeemer = Claim . toBuiltin @BS.ByteString . BA.convert $ sig

          BS.writeFile (assetsPath </> (sellerName ++ "SellDatum.cbor")) $ dataToCBOR $
            sellerOnRampDatum sellerName vkSeller sellPrice lovelaceSold deadline

          BS.writeFile (assetsPath </> (sellerName ++ "PaymentInfoRedeemer.cbor")) $ dataToCBOR claimRedeemer

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: expected five command-line arguments.\n"
