module Main where

import qualified Data.ByteString               as BS
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           Prelude                       (Bool (..), Either (..),
                                                FilePath, IO, error, putStr,
                                                show, ($), (++))
import           System.Directory              (createDirectoryIfMissing,
                                                getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.OffChain.Utils (dataToCBOR, parseAddress,
                                                savePlutus)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampParams (..),
                                                OnRampRedeemer (..),
                                                onRampCompiled)

onrampFee :: Lovelace
onrampFee = Lovelace 15000000

saveOnRampPlutus :: FilePath -> V3.Address  -> IO ()
saveOnRampPlutus path feeAddr = do
  let fiatPkhBytes = case addressCredential feeAddr of
        PubKeyCredential pkh -> getPubKeyHash pkh
        _                    -> error $ "Expected 'PubKeyCredential'"

  let onRampParams = OnRampParams
        { feeAddress          = feeAddr
        , feeValue            = lovelaceValue onrampFee
        , fiatPubKeyHashBytes = fiatPkhBytes
        }

  savePlutus (path </> "onRamp.plutus") $ onRampCompiled onRampParams

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
    (feeAddrStr : _) -> do
      let feeAddrE = parseAddress feeAddrStr

      case feeAddrE of
        Right feeAddr -> do

          saveOnRampPlutus assetsPath feeAddr

          BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
          BS.writeFile (assetsPath </> "cancel.cbor") $ dataToCBOR Cancel

          putStr "\nDone serializing plutus script.\n\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: please provide a command-line argument.\n"
