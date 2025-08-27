{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module P2POnRamp.Api.Seller where

import           Control.Exception             (throwIO)
-- import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad                 (when)
import           Data.Aeson                    (FromJSON)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           GHC.Generics                  (Generic)
-- import           Network.Wai                   (Middleware)
-- import           Network.Wai.Handler.Warp      (defaultSettings, runSettings, setHost, setPort)
-- import           Network.Wai.Middleware.Cors   (cors, simpleCorsResourcePolicy, corsRequestHeaders, CorsResourcePolicy(..))
-- import           Network.HTTP.Types.Status     (unauthorized401)
import           Servant
-- import           Servant.API                   (PlainText)
-- import           System.IO                     (hFlush, stdout)

-- CBOR
import           Codec.CBOR.Read               (deserialiseFromBytes)
import           Codec.CBOR.Term               (Term(..), encodeTerm, decodeTerm)
import           Codec.CBOR.Write              (toStrictByteString)

-- Ed25519 (cryptonite)
import           Crypto.Error                  (CryptoFailable(..))
import qualified Crypto.PubKey.Ed25519         as Ed25519
import           Prelude


data SignRequest = SignRequest
  { cose_sign1 :: Text   -- hex-encoded COSE_Sign1 (CBOR)
  , signature  :: Text   -- hex-encoded 64-byte Ed25519 signature (R||S)
  , pub_key    :: Text   -- hex-encoded 32-byte Ed25519 public key
  } deriving (Show, Generic)

instance FromJSON SignRequest

expectedMessage :: ByteString
expectedMessage = "Hello messenger."

--------------------------------------------------------------------------------
-- Helpers

badRequest :: String -> IO a
badRequest msg = throwIO $ err400 { errBody = LBS.fromStrict (BSC.pack msg) }

internalErr :: String -> IO a
internalErr msg = throwIO $ err500 { errBody = LBS.fromStrict (BSC.pack msg) }

unauthorizedText :: Text -> IO Text
unauthorizedText t = throwIO $ err401 { errBody = LBS.fromStrict (TE.encodeUtf8 t) }

decodeHexStrict :: Text -> Either String ByteString
decodeHexStrict = B16.decode . TE.encodeUtf8

decodeHexSized :: Int -> Text -> String -> Either String ByteString
decodeHexSized n t what = do
  bs <- decodeHexStrict t
  if BS.length bs == n
    then Right bs
    else Left $ what <> " must be exactly " <> show n <> " bytes (got " <> show (BS.length bs) <> ")"

-- logLn :: String -> Handler ()
-- logLn s = liftIO $ putStrLn s >> hFlush stdout

logLn :: String -> IO ()
logLn = putStrLn

hex :: ByteString -> Text
hex = TE.decodeUtf8 . B16.encode

--------------------------------------------------------------------------------
-- Handler

verifyH :: SignRequest -> IO Text
verifyH req = do
  logLn "--- Incoming verify request ---"
  logLn $ "COSE_Sign1 (hex): " <> T.unpack (cose_sign1 req)
  logLn $ "Signature (hex):  " <> T.unpack (signature  req)
  logLn $ "Public key (hex): " <> T.unpack (pub_key    req)

  coseSign1Bytes <- either badRequest pure $
    decodeHexStrict (cose_sign1 req)

  let coseLazy = LBS.fromStrict coseSign1Bytes

  -- Decode COSE_Sign1 as a generic CBOR term: expected Array [protected_bstr, unprotected_map, payload_bstr, signature_bstr]
  term <- case deserialiseFromBytes decodeTerm coseLazy of
            Left _                 -> badRequest "Invalid COSE_Sign1 CBOR"
            Right (_, t)           -> pure t

  items <- case term of
             TList xs | length xs == 4 -> pure xs
             _                         -> badRequest "Expected COSE_Sign1 array of length 4"

  protectedBytes <- case items !! 0 of
                      TBytes b -> pure b
                      _        -> badRequest "Invalid protected header type"

  payloadBytes <- case items !! 2 of
                    TBytes b -> pure b
                    _        -> badRequest "Invalid payload type"

  logLn $ "Protected (hex): " <> T.unpack (hex protectedBytes)
  logLn $ "Payload   (hex): " <> T.unpack (hex payloadBytes)
  logLn $ "Payload   (text): " <> T.unpack (TE.decodeUtf8With (\_ _ -> Just '�') payloadBytes)

  when (payloadBytes /= expectedMessage) $
    badRequest $ "Payload mismatch (expected " <> BSC.unpack expectedMessage <> ")"

  -- Build Sig_structure = ["Signature1", protected, h'', payload]
  let sigStructureTerm  = TList [ TString "Signature1"
                                , TBytes protectedBytes
                                , TBytes BS.empty      -- external_aad
                                , TBytes payloadBytes
                                ]
      sigStructureBytes = toStrictByteString (encodeTerm sigStructureTerm)

  logLn $ "Sig_structure (hex): " <> T.unpack (hex sigStructureBytes)

  sigBytes <- either badRequest pure $
    decodeHexSized 64 (signature req) "signature"
  pkBytes  <- either badRequest pure $
    decodeHexSized 32 (pub_key req)   "public key"

  -- Parse public key & signature
  pubKey <- case Ed25519.publicKey pkBytes of
              CryptoPassed pk -> pure pk
              CryptoFailed _  -> badRequest "Malformed public key"

  sig    <- case Ed25519.signature sigBytes of
              CryptoPassed s  -> pure s
              CryptoFailed _  -> badRequest "Malformed signature"

  -- Verify (Ed25519)
  if Ed25519.verify pubKey sigStructureBytes sig
     then do
       logLn "✔ Verification OK"
       pure "authorized"
     else do
       logLn "✖ Verification failed"
       -- Match your Rust: 401 with body "not-authorized"
       throwIO $ err401 { errBody = "not-authorized" }
