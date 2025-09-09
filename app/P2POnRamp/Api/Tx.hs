module P2POnRamp.Api.Tx where

import           Data.Aeson
import qualified Data.Text            as T
import           GeniusYield.Types
import           GHC.Generics
import           Prelude

------------------------- :Unsigned response: -------------------------

data UnsignedTxResponse = UnsignedTxResponse
  { urspTxBodyHex :: !T.Text           -- ^ Unsigned transaction cbor.
  , urspTxFee     :: !(Maybe Integer)  -- ^ Tx fees.
  } deriving (Show, Generic)

instance ToJSON UnsignedTxResponse

-- | Construct `UnsignedTxResponse` return type for our endpoint given the transaction body.
unSignedTxWithFee :: GYTxBody -> UnsignedTxResponse
unSignedTxWithFee txBody = UnsignedTxResponse
  { urspTxBodyHex  = T.pack . txToHex $ unsignedTx txBody
  , urspTxFee      = Just $ txBodyFee txBody
  }

---------------------------- :Submit Tx: ----------------------------

-- | Return type of API when submitting a transaction.
data SubmitTxResult = SubmitTxResult
  { submitTxFee :: !Integer
  , submitTxId  :: !GYTxId
  } deriving stock (Show, Generic)

instance ToJSON SubmitTxResult

-- | Construct `SubmitTxResult` return type from the given signed transaction body.
txBodySubmitTxResult :: GYTxBody -> SubmitTxResult
txBodySubmitTxResult txBody = SubmitTxResult
  { submitTxFee = txBodyFee txBody
  , submitTxId  = txBodyTxId txBody
  }
