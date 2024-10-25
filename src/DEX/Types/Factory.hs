module DEX.Types.Factory where

import Plutarch
import Plutarch.Api.V2 (PCurrencySymbol, PTokenName)
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import PlutusLedgerApi.V2
import PlutusTx qualified
import Prelude

data FactoryDatum = FactoryDatum
  { poolRangeFrom :: BuiltinByteString
  , poolRangeTo :: BuiltinByteString
  }
  deriving (Show, Generic)

PlutusTx.makeIsDataIndexed ''FactoryDatum [('FactoryDatum, 0)]
PlutusTx.makeLift ''FactoryDatum

data PFactoryDatum (s :: S) = PFactoryDatum (Term s (PDataRecord '["rangeFrom" ':= PByteString, "rangeTo" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFactoryDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PFactoryDatum where
  type PLifted PFactoryDatum = FactoryDatum

deriving via
  (DerivePConstantViaData FactoryDatum PFactoryDatum)
  instance
    (PConstantDecl FactoryDatum)

instance PTryFrom PData PFactoryDatum

data PoolChoice
  = UseConstantProduct
  | UseStableswap Integer Integer
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed ''PoolChoice [('UseConstantProduct, 0), ('UseStableswap, 1)]
PlutusTx.makeLift ''PoolChoice

data PPoolChoice (s :: S)
  = PUseConstantProduct (Term s (PDataRecord '[]))
  | PUseStableswap (Term s (PDataRecord '["aScale" ':= PInteger, "bScale" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PPoolChoice where
  type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PPoolChoice

instance PUnsafeLiftDecl PPoolChoice where
  type PLifted PPoolChoice = PoolChoice

deriving via
  (DerivePConstantViaData PoolChoice PPoolChoice)
  instance
    (PConstantDecl PoolChoice)

data FactoryRedeemer = Create
  { poolChoice :: PoolChoice
  , assetASymbol :: CurrencySymbol
  , assetAToken :: TokenName
  , assetBSymbol :: CurrencySymbol
  , assetBToken :: TokenName
  }
  deriving (Generic, Show)

PlutusTx.makeIsDataIndexed ''FactoryRedeemer [('Create, 0)]
PlutusTx.makeLift ''FactoryRedeemer

data PFactoryRedeemer (s :: S)
  = PCreate
      ( Term
          s
          ( PDataRecord
              '[ "poolChoice" ':= PPoolChoice
               , "assetASymbol" ':= PCurrencySymbol
               , "assetAToken" ':= PTokenName
               , "assetBSymbol" ':= PCurrencySymbol
               , "assetBToken" ':= PTokenName
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFactoryRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFactoryRedeemer

instance PUnsafeLiftDecl PFactoryRedeemer where
  type PLifted PFactoryRedeemer = FactoryRedeemer

deriving via
  (DerivePConstantViaData FactoryRedeemer PFactoryRedeemer)
  instance
    (PConstantDecl FactoryRedeemer)

pfactoryDatum :: Term s (PByteString :--> PByteString :--> PFactoryDatum)
pfactoryDatum = phoistAcyclic $ plam $ \rangeFrom rangeTo ->
  (pcon . PFactoryDatum) $ (pdcons @"rangeFrom" # pdata rangeFrom) #$ (pdcons @"rangeTo" # pdata rangeTo) #$ pdnil
