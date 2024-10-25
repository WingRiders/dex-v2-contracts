module DEX.Types.Treasury where

import Data.Aeson
import GHC.Generics hiding (S)
import Plutarch
import Plutarch.Api.V2 (
  PCurrencySymbol,
  PTokenName,
 )
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutus.Missing ()
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx qualified

data TreasuryHolderRedeemer = Collect
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed
  ''TreasuryHolderRedeemer
  [ ('Collect, 0)
  ]
PlutusTx.makeLift ''TreasuryHolderRedeemer

data PTreasuryHolderRedeemer (s :: S) = PCollect (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PTreasuryHolderRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTreasuryHolderRedeemer where
  type PLifted PTreasuryHolderRedeemer = TreasuryHolderRedeemer

deriving via
  (DerivePConstantViaData TreasuryHolderRedeemer PTreasuryHolderRedeemer)
  instance
    (PConstantDecl TreasuryHolderRedeemer)

data TreasuryHolderParameters = TreasuryHolderParameters
  { voteTokenClass :: AssetClass
  , totalVoteCount :: Integer
  , minVoteCount :: Integer
  }
  deriving (Show, Generic, ToJSON)

data PTreasuryHolderParameters (s :: S) = PTreasuryHolderParameters
  { pvoteCurrencySymbol :: Term s PCurrencySymbol
  -- ^ Voting currency symbol of the token allowing direct manipulation with the treasury
  , pvoteTokenName :: Term s PTokenName
  -- ^ Voting token name of the token allowing direct manipulation with the treasury
  , ptotalVoteCount :: Term s PInteger
  , pminVoteCount :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PTreasuryHolderParameters where
  type DPTStrat _ = PlutusTypeScott
