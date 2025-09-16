{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module P2POnRamp.OrdersDB
  ( CompletedType(..)
  , CompletedData(..)
  , SellerInfo(..)
  , IniInfo(..)
  , Order(..)
  , DB(..)
  , initDB
  , readOrdersDB
  , createOrder
  , setSellPostTxIfNull
  , setBuyPostTxIfNull
  , setFiatSignatureIfNull
  , setCompletedIfNull
  ) where

import           Control.Concurrent   (threadDelay)
import           Control.Exception    (IOException, bracket, bracketOnError,
                                       try)
import           Control.Monad        (void, when)
import           Data.Aeson           (FromJSON, ToJSON, eitherDecode', encode,
                                       withText)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (isNothing)
import           Data.Text            (Text)
import           Foreign.C.Error      (throwErrnoIfMinus1_)
import           Foreign.C.Types      (CInt (..))
import           GHC.Generics         (Generic)
import           Prelude
import           System.Directory     (doesFileExist, removeFile, renameFile)
import           System.FilePath      (takeDirectory, takeFileName)
import           System.IO            (hClose, hFlush, openBinaryTempFile)
import           System.Posix.IO      (OpenFileFlags (..),
                                       OpenMode (ReadOnly, ReadWrite), closeFd,
                                       defaultFileFlags, handleToFd, openFd)
import           System.Posix.Types   (Fd (..), FileMode)

import           P2POnRamp.Api.Context (internalErr)

--------------------------------------------------------------------------------
-- FFI: fsync(2)

foreign import ccall unsafe "fsync"
  c_fsync :: CInt -> IO CInt

fsyncFd :: Fd -> IO ()
fsyncFd (Fd fd) = throwErrnoIfMinus1_ "fsync" (c_fsync fd)

fsyncDir :: FilePath -> IO ()
fsyncDir dir = do
  -- Assumption: running on a POSIX compliant OS
  dfd <- openFd dir ReadOnly defaultFileFlags
  fsyncFd dfd
  closeFd dfd

--------------------------------------------------------------------------------
-- Domain types

data CompletedType = Claim | Cancel
  deriving (Show, Eq, Generic)

instance ToJSON CompletedType where
  toJSON Claim  = A.String "claim"
  toJSON Cancel = A.String "cancel"

instance FromJSON CompletedType where
  parseJSON = withText "CompletedType" $ \t ->
    case t of
      "claim"  -> pure Claim
      "cancel" -> pure Cancel
      _        -> fail "CompletedType must be \"claim\" or \"cancel\""

data CompletedData = CompletedData
  { completedType :: CompletedType
  , completedTx   :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SellerInfo = SellerInfo
  { sellerName    :: Text
  , sellerAccount :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data IniInfo = IniInfo
  { iiSellAda       :: Integer
  , iiPriceUsd      :: Integer
  , iiSellerPKBytes :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Order = Order
  { orderID       :: Int
  , sellerInfo    :: SellerInfo
  , iniInfo       :: IniInfo
  , sellPostTx    :: Maybe Text
  , buyPostTx     :: Maybe Text
  , fiatSignature :: Maybe Text
  , completedData :: Maybe CompletedData
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | DB file wrapper: nextOrderID + orders
data DB = DB
  { nextOrderID :: Int
  , orders      :: [Order]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- File locking (simple cooperating-process lock via O_CREAT|O_EXCL)

-- | Acquire: create <db>.lock with O_EXCL. Release: close + unlink.
withFileLock :: FilePath -> IO a -> IO a
withFileLock dbPath action = bracket acquire release (const action)
  where
    lockPath = dbPath <> ".lock"

    acquire :: IO Fd
    acquire = waitLoop
      where
        waitLoop = do
          let flags :: OpenFileFlags
              flags = defaultFileFlags
                        { creat     = Just (0o600 :: FileMode)  -- create with 0600 perms
                        , exclusive = True                      -- fail if the file exists
                        , trunc     = False
                        , append    = False
                        , nonBlock  = False
                        , noctty    = False
                        }
          eres <- try $ openFd lockPath ReadWrite flags
          case eres of
            Right fd -> pure fd
            Left (_ :: IOException) -> do
              -- someone else holds it; wait and retry
              threadDelay 100_000  -- 100ms
              waitLoop

    release :: Fd -> IO ()
    release fd = do
      closeFd fd
      -- Best-effort: remove the lock file. If it was removed by crash recovery, ignore errors.
      _ <- try (removeFile lockPath) :: IO (Either IOException ())
      pure ()

--------------------------------------------------------------------------------
-- Atomic write: temp file + fsync + rename + fsync(dir)

writeJSONAtomic :: FilePath -> BL.ByteString -> IO ()
writeJSONAtomic target bs = do
  let dir  = takeDirectory target
      stem = takeFileName target
  bracketOnError
    (openBinaryTempFile dir (stem <> ".tmp"))
    (\(tmpPath,h) -> hClose h >> safeRemove tmpPath)
    (\(tmpPath,h) -> do
        BL.hPut h bs
        hFlush h
        fd <- handleToFd h
        fsyncFd fd                 -- flush file content + metadata
        closeFd fd                 -- also closes handle
        renameFile tmpPath target  -- atomic on same filesystem
        fsyncDir dir               -- persist the directory entry (rename)
    )
  where
    safeRemove p = void (try (removeFile p) :: IO (Either IOException ()))

--------------------------------------------------------------------------------
-- DB load / save

loadDB :: FilePath -> IO DB
loadDB path = do
  exists <- doesFileExist path
  if not exists
    then pure (DB 1 []) -- initialize in-memory
    else do
      bs <- BL.readFile path
      case eitherDecode' bs of
        Left err -> internalErr $ "DB decode error: " ++ err
        Right db -> pure db

saveDB :: FilePath -> DB -> IO ()
saveDB path db = writeJSONAtomic path (encode db)

--------------------------------------------------------------------------------
-- Public API

-- | Ensure the DB file exists on disk (idempotent)
initDB :: FilePath -> IO ()
initDB path = withFileLock path $ do
  exists <- doesFileExist path
  when (not exists) $ saveDB path (DB 1 [])

-- | Read full list
readOrdersDB :: FilePath -> IO [Order]
readOrdersDB path = orders <$> loadDB path

-- | Create a new empty order (null fields start as Nothing)
createOrder :: FilePath -> SellerInfo -> IniInfo -> IO Order
createOrder path seller ini = withFileLock path $ do
  db  <- loadDB path
  let oid   = nextOrderID db
      order = Order
        { orderID       = oid
        , sellerInfo    = seller
        , iniInfo       = ini
        , sellPostTx    = Nothing
        , buyPostTx     = Nothing
        , fiatSignature = Nothing
        , completedData = Nothing
        }
      db' = db { nextOrderID = oid + 1, orders = orders db ++ [order] }
  saveDB path db'
  pure order

-- | Update helpers: update an order by ID, but only if it changed
updateOrderIf :: FilePath
              -> Int
              -> (Order -> (Order, Bool))
              -> IO Bool
updateOrderIf path oid upd = withFileLock path $ do
  db <- loadDB path
  let os = orders db
  case break ((== oid) . orderID) os of
    (pre, o:post) ->
      let (o', ch) = upd o
      in if ch
           then saveDB path (db { orders = pre ++ (o' : post) }) >> pure True
           else pure False
    _ -> pure False  -- not found

-- | Only set sellPostTx if it is currently Nothing
setSellPostTxIfNull :: FilePath -> Int -> Text -> IO Bool
setSellPostTxIfNull path oid tx =
  updateOrderIf path oid $ \o ->
    if isNothing (sellPostTx o)
      then (o { sellPostTx = Just tx }, True)
      else (o, False)

-- | Only set buyPostTx if it is currently Nothing
setBuyPostTxIfNull :: FilePath -> Int -> Text -> IO Bool
setBuyPostTxIfNull path oid tx =
  updateOrderIf path oid $ \o ->
    if isNothing (buyPostTx o)
      then (o { buyPostTx = Just tx }, True)
      else (o, False)

-- | Only set fiatSignature if it is currently Nothing
setFiatSignatureIfNull :: FilePath -> Int -> Text -> IO Bool
setFiatSignatureIfNull path oid sigHex =
  updateOrderIf path oid $ \o ->
    if isNothing (fiatSignature o)
      then (o { fiatSignature = Just sigHex }, True)
      else (o, False)

-- | Only set completedData if it is currently Nothing
setCompletedIfNull :: FilePath -> Int -> CompletedType -> Text -> IO Bool
setCompletedIfNull path oid ctype ctx =
  updateOrderIf path oid $ \o ->
    if isNothing (completedData o)
      then (o { completedData = Just (CompletedData ctype ctx) }, True)
      else (o, False)
