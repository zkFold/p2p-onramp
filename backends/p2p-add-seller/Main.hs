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

import           ZkFold.Cardano.Crypto.Utils   (extractSecretKey)
import           ZkFold.Cardano.OffChain.Utils (dataToCBOR, parseAddress)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.Parse.Utils    (parseInteger)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..), OnRampRedeemer (..))


sellerOnRampDatum :: String -> Integer -> Integer  -> V3.Address -> Integer -> OnRampDatum
sellerOnRampDatum sellerName sellPrice lovelaceSold sellerAddr deadline =
  let sellerPkh = case addressCredential sellerAddr of
        PubKeyCredential pkh -> pkh
        _                    -> error $ "Expected 'PubKeyCredential'"

  in OnRampDatum { paymentInfoHash  = paymentInfoHashEx1 sellerName
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
      keysPath   = path </> "e2e-test" </> "p2p" </> "keys"

  createDirectoryIfMissing True $ path </> "assets"

  argsRaw <- getArgs

  case argsRaw of
    (fiatAdminName : sellerName : sellPriceStr : lovelaceSoldStr : sellerAddrStr : deadlineStr : _) -> do
      skE <- extractSecretKey (keysPath </> (fiatAdminName ++ ".skey"))  -- Fiat admin will sign fiat payment info

      let argsE = do
            sk           <- skE
            sellPrice    <- parseInteger sellPriceStr
            lovelaceSold <- parseInteger lovelaceSoldStr
            sellerAddr   <- parseAddress sellerAddrStr
            deadline     <- parseInteger deadlineStr

            return (sk, sellPrice, lovelaceSold, sellerAddr, deadline)

      case argsE of
        Right (sk, sellPrice, lovelaceSold, sellerAddr, deadline) -> do
          let vk  = toPublic sk
              sig = sign sk vk . fromBuiltin . dataToBlake . paymentInfoHashEx1 $ sellerName

          let claimRedeemer = Claim . toBuiltin @BS.ByteString . BA.convert $ sig

          BS.writeFile (assetsPath </> (sellerName ++ "SellDatum.cbor")) $ dataToCBOR $
            sellerOnRampDatum sellerName sellPrice lovelaceSold sellerAddr deadline

          BS.writeFile (assetsPath </> (sellerName ++ "PaymentInfoRedeemer.cbor")) $ dataToCBOR claimRedeemer

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: expected six command-line arguments.\n"
