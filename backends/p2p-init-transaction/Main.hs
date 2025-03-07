module Main where

import qualified Data.ByteString                         as BS
import           Prelude                                 (Bool (..), IO, ($))
import           P2P.UPLC                                (timedCompiled)
import           System.Directory                        (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath                         (takeFileName, (</>))
import           System.Random                           (randomRIO)
import           ZkFold.Cardano.OffChain.Utils           (dataToCBOR, savePlutus)


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "p2p"      -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  let assetsPath = path </> "assets"

  createDirectoryIfMissing True $ path </> "assets"

  tag <- randomRIO (1, 10000)

  savePlutus (assetsPath </> "timed.plutus") $ timedCompiled tag

  BS.writeFile (assetsPath </> "unit.cbor") $ dataToCBOR ()
