module Main where

import qualified Data.ByteString               as BS
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           Prelude                       (Bool (..), Either (..),
                                                IO, error, putStr, return,
                                                show, ($), (.), (++))
import           System.Directory              (createDirectoryIfMissing,
                                                getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.Crypto.Utils   (extractKey)
import           ZkFold.Cardano.OffChain.Utils (dataToCBOR, parseAddress,
                                                savePlutus)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..),
                                                OnRampRedeemer (..),
                                                onRampCompiled)

onrampFee :: Lovelace
onrampFee = Lovelace 3000000

onRampParams :: V3.Address -> BS.ByteString -> OnRampParams
onRampParams feeAddr fiatPubKey = OnRampParams
                                  { feeAddress      = feeAddr
                                  , feeValue        = lovelaceValue onrampFee
                                  , fiatPubKeyBytes = toBuiltin fiatPubKey
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
    (fiatAdminName : feeAddrStr : _) -> do

      vkE <- extractKey (keysPath </> (fiatAdminName ++ ".vkey"))

      let paramsE = do
            vk      <- vkE
            feeAddr <- parseAddress feeAddrStr

            return (feeAddr, vk)

      case paramsE of
        Right (feeAddr, vk) -> do

          savePlutus (assetsPath </> "onRamp.plutus") . onRampCompiled $ onRampParams feeAddr vk

          BS.writeFile (assetsPath </> "cancel.cbor") $ dataToCBOR Cancel

          putStr "\nDone serializing plutus script.\n\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: please provide two command-line arguments.\n"
