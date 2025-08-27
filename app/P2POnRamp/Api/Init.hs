module P2POnRamp.Api.Init where

-- import           Data.Aeson
import qualified Data.ByteString               as BS
-- import qualified Data.ByteString.Lazy                   as BL
import           GeniusYield.GYConfig                   (GYCoreConfig (..))
-- import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V1.Value      (lovelaceValue)
import           PlutusLedgerApi.V3            as V3
import           Prelude
-- import           System.Directory                       (doesFileExist)
-- import           System.FilePath                        ((</>))
-- import           Test.QuickCheck.Arbitrary              (Arbitrary (..))
-- import           Test.QuickCheck.Gen                    (generate)
import           System.Environment            (getArgs)

-- import           P2POnRamp.Api.Context
-- import           ZkFold.Cardano.OffChain.Utils          (currencySymbolOf)
import           ZkFold.Cardano.UPLC.OnRamp             (OnRampParams (..), OnRampDatum (..), OnRampRedeemer (..), onRampCompiled)
import           ZkFold.Cardano.OffChain.Utils (parseAddress)
import           ZkFold.Cardano.Parse.Utils


onrampFee :: Lovelace
onrampFee = Lovelace 3000000

feeAddr :: Address
feeAddr = case parseAddress "addr_test1qq3aha54ed5p5z7wt5e4ru3x2zu7the4y3nd4kfjxexqxt5wa5w9v682wy6sg559qg63g2qxwy9dce73ezrq70xeq9gs0h30gl" of
  Right addr -> addr
  Left e     -> error e

onRampParams :: BuiltinByteString -> OnRampParams
onRampParams = OnRampParams feeAddr (lovelaceValue onrampFee)

main :: IO ()
main = do
  argsRaw <- getArgs

  case argsRaw of
    (feeAddrStr : feeValStr : _) -> do
      let paramsE = do
           feeAddr <- parseAddress feeAddrStr
           feeVal  <- valueSingleton GYLovelace <$> parseInteger feeValStr
           return (feeAddr, feeVal)

      case paramsE of
        Right (feeAddr, feeVal) -> do
          putStrLn $ show feeAddr
          putStrLn $ show feeVal

        Left err -> putStrLn $ "parse error: " ++ show err

    _ -> error "Error: please provide a pair of command-line arguments.\n"
