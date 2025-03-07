module Main where

import qualified Data.ByteString                         as BS
import           P2P.UPLC                                (ValidityRedeemer (..))
import           PlutusLedgerApi.V3                      as V3
import           Prelude                                 (Bool (..), IO, Either (..), error, putStr, show, ($), (++))
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.Environment                      (getArgs)
import           System.FilePath                         (takeFileName, (</>))
import           Text.Parsec                             (parse)

import           ZkFold.Cardano.OffChain.Utils           (dataToCBOR, integerParser)


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
    (markTimeStr : _) -> do
      let markTimeE = parse integerParser "" markTimeStr
      case markTimeE of
        Right markTime -> do
          let validityRedeemer = ValidityRedeemer { vrTimeMark = POSIXTime markTime }

          BS.writeFile (assetsPath </> "validityRedeemer.cbor") $ dataToCBOR validityRedeemer

          putStr $ "Invalid before: " ++ (show markTime) ++ "\n"

        Left err -> error $ "parse error: " ++ show err

    _ -> error "Error: misssing command-line argument.\n"
