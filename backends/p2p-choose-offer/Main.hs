module Main where

import           Data.Aeson                        (eitherDecode, FromJSON, parseJSON, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Coerce
import           Data.List                         (maximumBy)
import           Data.Ord                          (comparing)
import           P2P.Example                       (choiceValue)
import           Prelude
import           System.Directory                  (getCurrentDirectory)
import           System.Environment                (getArgs)
import           System.FilePath                   (takeFileName, (</>))
import qualified System.IO                         as IO

import           ZkFold.Cardano.Parse.Utils        (OnRampDatumJSON (..), OnRampUtxo (..))


data OnRampUtxoKV = OnRampUtxoKV { oref :: String, resolved :: OnRampUtxo }
  deriving Show

instance FromJSON OnRampUtxoKV where
    parseJSON = withObject "OnRampUtxoKV" $ \obj -> do
        oref     <- obj .: "key"
        resolved <- obj .: "value"
        return $ OnRampUtxoKV oref resolved

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let path = case takeFileName currentDir of
        "p2p"      -> ".." </> ".."
        "e2e-test" -> ".."
        _          -> "."

  let assetsPath = path </> "assets"

  argsRaw <- getArgs

  case argsRaw of
    (utxosStr : _) -> do
      case eitherDecode @[OnRampUtxoKV] $ BL8.pack utxosStr of
        Right utxos -> do
          let orefs        = map oref utxos
              choiceValues = map (choiceValue . coerce . inlineDatum . resolved) utxos
              maxI         = maxIndex choiceValues

          IO.writeFile (assetsPath </> "sellChoiceOref.txt") $ orefs !! maxI

        Left err    -> error err
    _ -> error "Expected one command-line argument"


----- HELPER FUNCTIONS -----

-- | Position index of maximum.
maxIndex :: (Ord a) => [a] -> Int
maxIndex xs = snd $ maximumBy (comparing fst) (zip xs [0..])
