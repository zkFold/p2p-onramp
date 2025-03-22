module Main where

import           Data.Aeson                 (FromJSON, eitherDecode, parseJSON,
                                             withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Coerce
import           Data.List                  (maximumBy)
import           Data.Ord                   (comparing)
import           Prelude
import           System.Directory           (getCurrentDirectory)
import           System.Environment         (getArgs)
import           System.FilePath            (takeFileName, (</>))
import qualified System.IO                  as IO

import           ZkFold.Cardano.P2P.Example (choiceValue)
import           ZkFold.Cardano.Parse.Utils (OnRampDatumJSON (..),
                                             OnRampUtxo (..))


data OnRampUtxoKV = OnRampUtxoKV { oref :: String, resolved :: OnRampUtxo }
  deriving Show

instance FromJSON OnRampUtxoKV where
  parseJSON = withObject "OnRampUtxoKV" $
    \obj -> OnRampUtxoKV <$> obj .: "key" <*> obj .: "value"

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
              maxI         = maxIndex choiceValues  -- Selects best sell offer

          let sellChoiceOref = orefs !! maxI

          putStr $ "Selected TxOutRef: " ++ sellChoiceOref ++ "\n"

          IO.writeFile (assetsPath </> "sellChoiceOref.txt") sellChoiceOref

        Left err    -> error err
    _ -> error "Expected one command-line argument"


----- HELPER FUNCTIONS -----

-- | Position index of maximum in a list.
maxIndex :: (Ord a) => [a] -> Int
maxIndex xs = snd $ maximumBy (comparing fst) (zip xs [0..])
