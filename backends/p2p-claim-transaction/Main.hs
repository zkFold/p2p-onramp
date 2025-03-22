module Main where

import           Crypto.PubKey.Ed25519
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Char8         as B8
import           PlutusTx.Prelude              hiding (error)
import           Prelude                       (IO, error, print, putStr, show)
import           System.Directory              (getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.Crypto.Utils   (extractSecretKey)
import           ZkFold.Cardano.OffChain.Utils (dataToCBOR)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.P2P.Example    (paymentInfoHashEx1)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampRedeemer (..))


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "p2p"      -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  let assetsPath = path </> "assets"
      keysPath   = path </> "e2e-test" </> "p2p" </> "keys"

  argsRaw <- getArgs

  case argsRaw of
    (fiatWitnessName : sellerName : _) -> do
      skFiatE   <- extractSecretKey (keysPath </> (fiatWitnessName ++ ".skey"))  -- Get fiat witness' private key

      case skFiatE of
        Right skFiat -> do
          let vkFiat = toPublic skFiat

          putStr "\nFiat witness signs hash of fiat payment info...\n\n"

          let infoHash = paymentInfoHashEx1 sellerName
              sig = sign skFiat vkFiat . fromBuiltin . dataToBlake $ infoHash

          let claimRedeemer = Claim . toBuiltin @BS.ByteString . BA.convert $ sig

          putStr "Verifies signature:\n"
          print $ verifyEd25519Signature (toBuiltin @BS.ByteString . BA.convert $ vkFiat) (dataToBlake infoHash) (toBuiltin @BS.ByteString . BA.convert $ sig)

          putStr $ "\nSignature: " ++ B8.unpack (B16.encode . BA.convert $ sig) ++ "\n"

          BS.writeFile (assetsPath </> (sellerName ++ "PaymentInfoRedeemer.cbor")) $ dataToCBOR claimRedeemer

          putStr $ "\nWrote " ++ sellerName ++ "PaymentInfoRedeemer.cbor\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: expected a pair of command-line arguments.\n"
