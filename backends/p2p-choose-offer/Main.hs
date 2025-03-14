module Main where

import           Data.List          (maximumBy)
import           Data.Ord           (comparing)
import           P2P.Example        (choiceValue)
import           Prelude
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)
import           System.FilePath    (takeFileName, (</>))
import qualified System.IO          as IO

import ZkFold.Cardano.Parse.Utils   (parseOnRampDatum)


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "p2p"      -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  let assetsPath = path </> "assets"

  argsRaw <- getArgs

  if null argsRaw then error "No sellers!" else
    if length argsRaw `mod` 2 == 1 then error "Expected an even number of command-line arguments."
    else do
      let (names, utxosStr) = splitPairs argsRaw

      case mapM parseOnRampDatum utxosStr of
        Right datums -> do
          let choiceValues = map choiceValue datums
              maxI         = maxIndex choiceValues

          IO.writeFile (assetsPath </> "sellerChoice.txt") $ names !! maxI  -- Selected seller

        Left err     -> error err


----- HELPER FUNCTIONS -----

-- | Split elements according to their even/odd position.
splitPairs :: [a] -> ([a], [a])
splitPairs xs = unzip $ go xs
  where
    go []       = []
    go (x:y:zs) = (x, y) : go zs
    go _        = error "absurd"

-- | Position index of maximum.
maxIndex :: (Ord a) => [a] -> Int
maxIndex xs = snd $ maximumBy (comparing fst) (zip xs [0..])
