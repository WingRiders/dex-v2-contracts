{-# OPTIONS_GHC -Wno-orphans #-}

module DEX.Types.Request where

import DEX.Types.Pool
import GHC.Generics hiding (S)
import Plutarch
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Orphans ()
import Plutarch.Prelude
import PlutusLedgerApi.V2
import PlutusTx qualified

type BeneficiaryAddress = Address
type OwnerAddress = Address

{- |
  Represents direction in which we are swapping
-}
data SwapDirection
  = -- | User has asset A and wants asset B
    SwapAToB
  | -- | User has asset B and wants asset A
    SwapBToA
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed
  ''SwapDirection
  [ ('SwapAToB, 0)
  , ('SwapBToA, 1)
  ]
PlutusTx.makeLift ''SwapDirection

type MinWantedTokens = Integer

data DatumType = No | Hash | Inline
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed
  ''DatumType
  [ ('No, 0)
  , ('Hash, 1)
  , ('Inline, 2)
  ]
PlutusTx.makeLift ''DatumType
data RequestAction
  = -- |
    --  User wants to swap the locked asset, receiving at least a given quantity of wanted asset
    Swap SwapDirection Integer
  | -- |
    --  User wants to add liquidity to the pool, receiving minimum number of pool shares in return
    AddLiquidity MinWantedTokens
  | -- |
    --  User wants to withdraw liquidity from the pool.
    WithdrawLiquidity MinWantedTokens MinWantedTokens
  | -- |
    --  User wants to trigger withdrawing the assets that belong to the treasury from the pool.
    ExtractTreasury
  | -- |
    -- Staking agent adds the staking rewards back into the pool
    AddStakingRewards
  | ExtractProjectTreasury
  | ExtractReserveTreasury
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed
  ''RequestAction
  [ ('Swap, 0)
  , ('AddLiquidity, 1)
  , ('WithdrawLiquidity, 2)
  , ('ExtractTreasury, 3)
  , ('AddStakingRewards, 4)
  , ('ExtractProjectTreasury, 5)
  , ('ExtractReserveTreasury, 6)
  ]
PlutusTx.makeLift ''RequestAction

data RequestDatum = RequestDatum
  { oil :: Integer
  -- ^
  --  A user-provided oil that is used to check the amount of Ada on the compensation.
  --  Useful for routed swaps.
  --  This value shouldn't be lower than 2_000_000 (2 ada).
  --  Requests that set it lower should be ignored.
  , beneficiary :: BeneficiaryAddress
  -- ^
  --  Beneficiary of the Apply action. Can be both a script address or a pubkey address.
  --  Only this address can receive funds from this request if applied by the pool.
  --  Note that beneficiary is *not* used when reclaiming.
  , ownerAddress :: OwnerAddress
  -- ^
  --  Owner of the action. Must be a pubkey hash.
  --  Only owner can initiate reclaim.
  --  Note that in reclaim, owner could route assets in any way they desire.
  , compensationDatum :: Datum
  -- ^
  --  The Datum that has to be placed on the compensation output.
  --  If the compensationDatumType is set No, then this field is ignored.
  , compensationDatumType :: DatumType
  -- ^
  --  The type of the Datum placed on the compensation output.
  --  Type "No" for script outputs represents an InlineDatum that is of type EnforcedScriptOutDatum.
  , deadline :: POSIXTime
  , assetASymbol :: CurrencySymbol
  , assetAToken :: TokenName
  , assetBSymbol :: CurrencySymbol
  , assetBToken :: TokenName
  , action :: RequestAction
  -- ^
  --  Action to perform
  , aScale :: Integer
  -- ^ There can be many stableswap pools with different scales, they must match the ones in the request
  -- The scales are ignored for constant product pools
  , bScale :: Integer
  -- ^ There can be many stableswap pools with different scales, they must match the ones in the request
  -- The scales are ignored for constant product pools
  }
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed ''RequestDatum [('RequestDatum, 0)]
PlutusTx.makeLift ''RequestDatum

type PoolInputLocation = Integer

data RequestRedeemer
  = Apply PoolInputLocation
  | Reclaim
  deriving (Show)

PlutusTx.makeIsDataIndexed
  ''RequestRedeemer
  [ ('Apply, 0)
  , ('Reclaim, 1)
  ]
PlutusTx.makeLift ''RequestRedeemer

data PSwapDirection (s :: S)
  = PSwapAToB (Term s (PDataRecord '[]))
  | PSwapBToA (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PSwapDirection where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PSwapDirection where
  type PLifted PSwapDirection = SwapDirection

deriving via
  (DerivePConstantViaData SwapDirection PSwapDirection)
  instance
    (PConstantDecl SwapDirection)

instance PTryFrom PData PSwapDirection

data PDatumType (s :: S)
  = PNo (Term s (PDataRecord '[]))
  | PHash (Term s (PDataRecord '[]))
  | PInline (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PDatumType where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PDatumType where
  type PLifted PDatumType = DatumType

deriving via
  (DerivePConstantViaData DatumType PDatumType)
  instance
    (PConstantDecl DatumType)

instance PTryFrom PData PDatumType

data PRequestAction (s :: S)
  = PSwap (Term s (PDataRecord '["swapDirection" ':= PSwapDirection, "minWantedTokens" ':= PInteger]))
  | PAddLiquidity (Term s (PDataRecord '["minWantedShares" ':= PInteger]))
  | PWithdrawLiquidity (Term s (PDataRecord '["minWantedA" ':= PInteger, "minWantedB" ':= PInteger]))
  | PExtractTreasury (Term s (PDataRecord '[]))
  | PAddStakingRewards (Term s (PDataRecord '[]))
  | PExtractProjectTreasury (Term s (PDataRecord '[]))
  | PExtractReserveTreasury (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PRequestAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRequestAction where
  type PLifted PRequestAction = RequestAction

deriving via
  (DerivePConstantViaData RequestAction PRequestAction)
  instance
    (PConstantDecl RequestAction)

instance PTryFrom PData PRequestAction

data PRequestDatum (s :: S)
  = PRequestDatum
      ( Term
          s
          ( PDataRecord
              '[ "oil" ':= PInteger
               , "beneficiary" ':= PAddress
               , "owner" ':= PAddress
               , "compensationDatum" ':= PDatum
               , "compensationDatumType" ':= PDatumType
               , "deadline" ':= PPOSIXTime
               , "assetASymbol" ':= PCurrencySymbol
               , "assetAToken" ':= PTokenName
               , "assetBSymbol" ':= PCurrencySymbol
               , "assetBToken" ':= PTokenName
               , "action" ':= PRequestAction
               , "aScale" ':= PInteger
               , "bScale" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRequestDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PRequestDatum where
  type PLifted PRequestDatum = RequestDatum

deriving via
  (DerivePConstantViaData RequestDatum PRequestDatum)
  instance
    (PConstantDecl RequestDatum)

instance PTryFrom PData PDatum
instance PTryFrom PData POutputDatum
instance PTryFrom PData PRequestDatum

data PRequestRedeemer (s :: S)
  = PApply (Term s (PDataRecord '["poolInputLocation" ':= PInteger]))
  | PReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PRequestRedeemer where
  type DPTStrat _ = PlutusTypeData

data PParsedRequest (s :: S) = PParsedRequest
  { action :: Term s PRequestAction
  , provided :: Term s PFlatAssets
  , compensation :: Term s PFlatAssets
  , additionalData :: Term s PData
  -- ^
  -- * Stableswap requests pass the new invariant value D.
  -- * Stableswap zap outs pass the new invariant value D and the intermediate invariant value D as a List.
  -- * Stableswap add liquidities pass a list with:
  --    - the D with fully added liquidity before the fees
  --    - the D with added liquidity and taken fees
  --    - the new D after the swap fees are added back
  -- * Constant product zap ins pass the expected swapped number of tokens.
  -- * All other requests ignore the data.
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)

instance DerivePlutusType PParsedRequest where
  type DPTStrat _ = PlutusTypeScott

instance PUnsafeLiftDecl PRequestRedeemer where
  type PLifted PRequestRedeemer = RequestRedeemer

deriving via
  (DerivePConstantViaData RequestRedeemer PRequestRedeemer)
  instance
    (PConstantDecl RequestRedeemer)
