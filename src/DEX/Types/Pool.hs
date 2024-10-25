module DEX.Types.Pool where

import Plutarch
import Plutarch.Api.V2 (
  PAddress,
  PCurrencySymbol,
  PMaybeData,
  PPOSIXTime,
  PScriptHash,
  PTokenName,
 )
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import PlutusLedgerApi.V2
import PlutusTx qualified

type AgentLocation = Integer
type RequestLocation = Integer
type PoolLocation = Integer

data PoolRedeemer
  = Evolve PoolLocation AgentLocation [(RequestLocation, BuiltinData)]
  | EmergencyWithdrawal PoolLocation
  | ChangeFees
  | ChangeAgentFee
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed
  ''PoolRedeemer
  [ ('Evolve, 0)
  , ('EmergencyWithdrawal, 1)
  , ('ChangeFees, 2)
  , ('ChangeAgentFee, 3)
  ]
PlutusTx.makeLift ''PoolRedeemer

-- | Used as a kind.
data PoolType
  = ConstantProduct
  | Stableswap

data PoolState a = PoolState
  { hqtyA :: Integer
  , hqtyB :: Integer
  , hqtyShares :: Integer
  , hqtyTreasuryA :: Integer
  , hqtyTreasuryB :: Integer
  , hqtyProjectTreasuryA :: Integer
  , hqtyProjectTreasuryB :: Integer
  , hqtyReserveTreasuryA :: Integer
  , hqtyReserveTreasuryB :: Integer
  , hswapFeeInBasis :: Integer
  , hprotocolFeeInBasis :: Integer
  , hprojectFeeInBasis :: Integer
  , hreserveFeeInBasis :: Integer
  , hfeeBasis :: Integer
  , hpoolSpecifics :: a
  }
  deriving (Show, Generic)

{- | Scott-encoded pool datum representation.
 Used when evolving the pool to reduce the amount of parsing.
-}
data PPoolState' (a :: PType) (s :: S) = PPoolState'
  { qtyA :: Term s PInteger
  , qtyB :: Term s PInteger
  , qtyShares :: Term s PInteger
  , qtyTreasuryA :: Term s PInteger
  , qtyTreasuryB :: Term s PInteger
  , qtyProjectTreasuryA :: Term s PInteger
  , qtyProjectTreasuryB :: Term s PInteger
  , qtyReserveTreasuryA :: Term s PInteger
  , qtyReserveTreasuryB :: Term s PInteger
  , swapFeeInBasis :: Term s PInteger
  , protocolFeeInBasis :: Term s PInteger
  , projectFeeInBasis :: Term s PInteger
  , reserveFeeInBasis :: Term s PInteger
  , feeBasis :: Term s PInteger
  , poolSpecifics :: Term s a
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow)

instance DerivePlutusType (PPoolState' a) where
  type DPTStrat _ = PlutusTypeScott

data PoolDatum a = PoolDatum
  { requestValidatorHash :: ScriptHash
  , assetASymbol :: CurrencySymbol
  , assetAToken :: TokenName
  , assetBSymbol :: CurrencySymbol
  , assetBToken :: TokenName
  , swapFeeInBasis :: Integer
  , protocolFeeInBasis :: Integer
  , projectFeeInBasis :: Integer
  , reserveFeeInBasis :: Integer
  , feeBasis :: Integer
  , agentFeeAda :: Integer
  , lastInteraction :: POSIXTime
  , treasuryA :: Integer
  , treasuryB :: Integer
  , projectTreasuryA :: Integer
  -- ^ The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the projectBeneficiary
  , projectTreasuryB :: Integer
  -- ^ The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the projectBeneficiary
  , reserveTreasuryA :: Integer
  -- ^ The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the reserveBeneficiary
  , reserveTreasuryB :: Integer
  -- ^ The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the reserveBeneficiary
  , projectBeneficiary :: Maybe Address
  -- ^ Can be freely changed by the fee auth token holder at any point via a direct pool interaction. The new address can receive all accumulated funds.
  , reserveBeneficiary :: Maybe Address
  -- ^ Can be freely changed by the fee auth token holder at any point via a direct pool interaction. The new address can receive all accumulated funds.
  , poolSpecifics :: a
  }
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]
PlutusTx.makeLift ''PoolDatum

data EnforcedScriptOutDatum = EnforcedScriptOutDatum
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed ''EnforcedScriptOutDatum [('EnforcedScriptOutDatum, 0)]
PlutusTx.makeLift ''EnforcedScriptOutDatum

{- | Stores the pool datum.
 Specializes when supplied with a pool-specific type `a`.
 You would probably want to use the `PPoolDatum` type instead.
-}
data PPoolDatum' (a :: PType) (s :: S)
  = PPoolDatum'
      ( Term
          s
          ( PDataRecord
              '[ "requestValidatorHash" ':= PScriptHash
               , "assetASymbol" ':= PCurrencySymbol
               , "assetAToken" ':= PTokenName
               , "assetBSymbol" ':= PCurrencySymbol
               , "assetBToken" ':= PTokenName
               , "swapFeeInBasis" ':= PInteger
               , "protocolFeeInBasis" ':= PInteger
               , "projectFeeInBasis" ':= PInteger
               , "reserveFeeInBasis" ':= PInteger
               , "feeBasis" ':= PInteger
               , "agentFeeAda" ':= PInteger
               , "lastInteraction" ':= PPOSIXTime
               , "treasuryA" ':= PInteger
               , "treasuryB" ':= PInteger
               , -- The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the projectBeneficiary
                 "projectTreasuryA" ':= PInteger
               , -- The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the projectBeneficiary
                 "projectTreasuryB" ':= PInteger
               , -- The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the reserveBeneficiary
                 "reserveTreasuryA" ':= PInteger
               , -- The amount of tokens in this treasury specified by this Integer is fully owned by the fee auth token holder, until they are extracted to the reserveBeneficiary
                 "reserveTreasuryB" ':= PInteger
               , -- Can be freely changed by the fee auth token holder at any point via a direct pool interaction. The new address can receive all accumulated funds.
                 "projectBeneficiary" ':= PMaybeData (PAsData PAddress)
               , -- Can be freely changed by the fee auth token holder at any point via a direct pool interaction. The new address can receive all accumulated funds.
                 "reserveBeneficiary" ':= PMaybeData (PAsData PAddress)
               , "poolSpecifics" ':= a
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType (PPoolDatum' a) where
  type DPTStrat _ = PlutusTypeData

-- | Represents the pool redeemer.
data PPoolRedeemer (s :: S)
  = PEvolve
      ( Term
          s
          ( PDataRecord
              '[ "poolLocation" ':= PInteger
               , "agentLocation" ':= PInteger
               , -- List of request locations and their data.
                 -- Stableswap requests pass the new invariant value D.
                 -- Stableswap zap outs pass the new invariant value D and the intermediate invariant value D.
                 -- Constant product zap ins pass the expected swapped number of tokens.
                 -- All other requests ignore the data.
                 "requestLocations" ':= PBuiltinList (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PData)))
               ]
          )
      )
  | PEmergencyWithdrawal (Term s (PDataRecord '["poolLocation" ':= PInteger]))
  | PChangeFees (Term s (PDataRecord '[]))
  | PChangeAgentFee (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PPoolRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PPoolRedeemer where
  type PLifted PPoolRedeemer = PoolRedeemer

deriving via
  (DerivePConstantViaData PoolRedeemer PPoolRedeemer)
  instance
    (PConstantDecl PoolRedeemer)

{- | Used to store the parsed out values for requests and pools.
This allows us to parse out interesting assets only once.
Note: This doesn't preserve any data about all the other tokens which might be on the utxo.
Note: FlatAssets represent Value without any oilAda
-}
data PFlatAssets (s :: S) = PFlatAssets
  { pfltA :: Term s PInteger
  , pfltB :: Term s PInteger
  , pfltShares :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PEq)

instance DerivePlutusType PFlatAssets where
  type DPTStrat _ = PlutusTypeScott

data PEnforcedScriptOutDatum (s :: S)
  = PEnforcedScriptOutDatum (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PEnforcedScriptOutDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PEnforcedScriptOutDatum where
  type PLifted PEnforcedScriptOutDatum = EnforcedScriptOutDatum

deriving via
  (DerivePConstantViaData EnforcedScriptOutDatum PEnforcedScriptOutDatum)
  instance
    (PConstantDecl EnforcedScriptOutDatum)

instance PTryFrom PData PEnforcedScriptOutDatum
