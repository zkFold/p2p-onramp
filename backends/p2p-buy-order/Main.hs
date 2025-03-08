module Main where

import qualified Data.Aeson                    as Aeson
import           Data.Aeson                    (FromJSON, eitherDecode, withObject, (.:), (.:?))
import           Data.Aeson.Types              (parseEither)
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Crypto.PubKey.Ed25519
import           Crypto.Error (CryptoFailable(..))
import           Data.Maybe                    (maybe)
import qualified Data.ByteString.Lazy.Char8    as BL8
import           PlutusLedgerApi.V3            as V3
import           PlutusTx.Prelude              (verifyEd25519Signature)
import           Prelude                       (IO, Either (..), FilePath, Maybe (..), String, error, print, putStrLn, return,
                                                ($), (.), (<$>), (-), (++), (>=), (>>=))
import           System.Directory              (getCurrentDirectory)
import           System.Environment            (getArgs)
import           System.FilePath               (takeFileName, (</>))

import           ZkFold.Cardano.OffChain.Utils (dataToCBOR, parseAddress, parseInlineDatum)
import           ZkFold.Cardano.OnChain.Utils  (dataToBlake)
import           ZkFold.Cardano.UPLC.OnRamp    (OnRampDatum (..), OnRampRedeemer (..))


data KeyFile = KeyFile { cborHex :: String }

instance FromJSON KeyFile where
    parseJSON = withObject "KeyFile" $ \v -> KeyFile <$> v .: "cborHex"

-- | Extract the last 32 bytes of a key
extractKey :: FilePath -> IO (Either String B8.ByteString)
extractKey filePath = do
  fileContent <- BL.readFile filePath
  case eitherDecode fileContent of
    Right (KeyFile hexStr) -> 
      case B16.decode (B8.pack hexStr) of
        Right rawBytes | B8.length rawBytes >= 32 -> 
          return $ Right (B8.drop (B8.length rawBytes - 32) rawBytes)
        _ -> return . Left $ "Error: unable to retrieve rawbytes"
    Left e -> return . Left $ "Error: " ++ e

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
      skeyE <- extractKey (keysPath </> (sellerName ++ ".skey"))

      let paramsE = do
            buyerAddr <- parseAddress buyerAddrStr
            prevDat   <- eitherDecode (BL8.pack sellerUtxoStr) >>= parseDatum

            buyerPkh <- case addressCredential buyerAddr of
                          PubKeyCredential pkh -> Right pkh
                          _                    -> Left "Expected 'PubKeyCredential'"

            skBytes <- skeyE
            sk      <- case secretKey skBytes of
                         CryptoPassed key -> Right key
                         CryptoFailed _   -> Left "Invalid skey"

            return (buyerPkh, prevDat, sk)

      case paramsE of
        Right (buyerPkh, prevDat, sk) -> do
          let vk = toPublic sk

          let nextDat = prevDat { buyerPubKeyHash = Just buyerPkh }

          let sig            = sign sk vk . fromBuiltin @BuiltinByteString $ dataToBlake nextDat
              updateRedeemer = Update . toBuiltin @BS.ByteString . BA.convert $ sig

          putStrLn "\nVerifies signature:"
          print $ verifyEd25519Signature (toBuiltin @BS.ByteString . BA.convert $ vk) (dataToBlake nextDat) (toBuiltin @BS.ByteString . BA.convert $ sig)

          putStrLn $ "\nPublic Key: " ++ B8.unpack (B16.encode . BA.convert $ vk)
          putStrLn $ "Signature: " ++ B8.unpack (B16.encode . BA.convert $ sig)

          BS.writeFile (assetsPath </> (buyerName ++ "BoughtDatum.cbor")) $ dataToCBOR nextDat
          BS.writeFile (assetsPath </> (sellerName ++ "SoldRedeemer.cbor")) $ dataToCBOR updateRedeemer

        Left e                    -> error $ "Error: " ++ e

    _ -> error "Error: please provide four command-line arguments.\n"
