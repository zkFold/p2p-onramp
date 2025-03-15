module Main where

import           Crypto.PubKey.Ed25519
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Char8         as B8
import           PlutusLedgerApi.V3            as V3
import           PlutusTx.Prelude              (verifyEd25519Signature)
import           Prelude                       (Either (..), IO, Maybe (..),
                                                error, print, putStr, return,
                                                ($), (++), (.))
import           System.Directory              (getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.Crypto.Utils   (extractSecretKey)
import           ZkFold.Cardano.OffChain.Utils (dataToCBOR, parseAddress)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.Parse.Utils    (parseOnRampDatum)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..),
                                                OnRampRedeemer (..))


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
    (buyerName : buyerAddrStr : sellerName : sellerUtxoStr : _) -> do
      skE <- extractSecretKey (keysPath </> (sellerName ++ ".skey"))

      let paramsE = do
            sk        <- skE
            buyerAddr <- parseAddress buyerAddrStr
            buyerPkh  <- case addressCredential buyerAddr of
                           PubKeyCredential pkh -> Right pkh
                           _                    -> Left "Expected 'PubKeyCredential'"
            prevDat   <- parseOnRampDatum sellerUtxoStr

            return (sk, buyerPkh, prevDat)

      case paramsE of
        Right (sk, buyerPkh, prevDat) -> do
          let vk = toPublic sk

          let nextDat = prevDat { buyerPubKeyHash = Just buyerPkh }

          putStr "\nSeller signs datum updated with buyer's pub-key-hash...\n\n"

          let sig            = sign sk vk . fromBuiltin $ dataToBlake nextDat
              updateRedeemer = Update . toBuiltin @BS.ByteString . BA.convert $ sig

          putStr "Verifies signature:\n"
          print $ verifyEd25519Signature (toBuiltin @BS.ByteString . BA.convert $ vk) (dataToBlake nextDat) (toBuiltin @BS.ByteString . BA.convert $ sig)

          putStr $ "\nSignature: " ++ B8.unpack (B16.encode . BA.convert $ sig) ++ "\n"

          BS.writeFile (assetsPath </> (buyerName ++ "BoughtDatum.cbor")) $ dataToCBOR nextDat
          BS.writeFile (assetsPath </> (sellerName ++ "SoldRedeemer.cbor")) $ dataToCBOR updateRedeemer

        Left e                        -> error e

    _ -> error "Please provide four command-line arguments.\n"
