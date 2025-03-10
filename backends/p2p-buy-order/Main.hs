module Main where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson                    (eitherDecode, (.:?))
import           Data.Aeson.Types              (parseEither)
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import           Crypto.PubKey.Ed25519
import           Data.Maybe                    (maybe)
import qualified Data.ByteString.Lazy.Char8    as BL8
import           PlutusLedgerApi.V3            as V3
import           PlutusTx.Prelude              (blake2b_224, verifyEd25519Signature)
import           Prelude                       (IO, Either (..), Maybe (..), String, error, print, putStrLn, return,
                                                ($), (.), (++), (>>=))
import           System.Directory              (getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.OffChain.Utils (dataToCBOR, parseAddress, parseInlineDatum)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.Crypto.Utils   (extractSecretKey)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..), OnRampRedeemer (..))


parseDatum :: Aeson.Value -> Either String OnRampDatum
parseDatum (Aeson.Object v) = do
  inlineDatum <- case parseEither (.:? "inlineDatum") v of
                   Right (Just inlineDatumObject) -> parseInlineDatum inlineDatumObject
                   Right Nothing                  -> Right Nothing
                   Left err                       -> Left $ "Failed to parse inlineDatum: " ++ err

  case inlineDatum of
    Just (OutputDatum dat) -> maybe (Left "Missing datum") Right (fromBuiltinData . getDatum $ dat)
    _                      -> Left "Missing inlineDatum"
parseDatum _ = Left "Failed to parse datum"

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
            prevDat   <- eitherDecode (BL8.pack sellerUtxoStr) >>= parseDatum
            buyerPkh  <- case addressCredential buyerAddr of
                           PubKeyCredential pkh -> Right pkh
                           _                    -> Left "Expected 'PubKeyCredential'"
            return (sk, buyerPkh, prevDat)

      case paramsE of
        Right (sk, buyerPkh, prevDat) -> do
          let vk = toPublic sk

          let nextDat = prevDat { buyerPubKeyHash = Just buyerPkh }

          let sig            = sign sk vk . fromBuiltin @BuiltinByteString $ dataToBlake nextDat
              updateRedeemer = Update
                               ( toBuiltin @BS.ByteString . BA.convert $ sig )
                               ( toBuiltin @BS.ByteString . BA.convert $ vk )

          putStrLn "\nVerifies signature:"
          print $ verifyEd25519Signature (toBuiltin @BS.ByteString . BA.convert $ vk) (dataToBlake nextDat) (toBuiltin @BS.ByteString . BA.convert $ sig)

          putStrLn $ "\nPublic Key: " ++ B8.unpack (B16.encode . BA.convert $ vk)
          putStrLn $ "PubKeyHash: " ++ B8.unpack (B16.encode . fromBuiltin . blake2b_224 . toBuiltin @BS.ByteString . BA.convert $ vk)
          putStrLn $ "Signature: " ++ B8.unpack (B16.encode . BA.convert $ sig)

          BS.writeFile (assetsPath </> (buyerName ++ "BoughtDatum.cbor")) $ dataToCBOR nextDat
          BS.writeFile (assetsPath </> (sellerName ++ "SoldRedeemer.cbor")) $ dataToCBOR updateRedeemer

        Left e                    -> error e

    _ -> error "Please provide four command-line arguments.\n"
