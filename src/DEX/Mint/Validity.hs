{-# LANGUAGE BlockArguments #-}

module DEX.Mint.Validity (
  validityMintingPolicy,
  validityCurrencySymbol,
  pvalidityMintingPolicy,
  PValidityConfig (..),
  ValidityConfig (..),
  PValidityRedeemer (..),
  ValidityRedeemer (..),
) where

import DEX.Constants qualified as C
import Plutarch
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext (pisUTXOSpent)
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Mint.Util
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import Plutarch.Util
import PlutusLedgerApi.V2 (CurrencySymbol (..), ScriptHash (..), TxOutRef)
import PlutusTx qualified

data ValidityConfig = ValidityConfig
  { starter :: TxOutRef
  }
  deriving (Generic)

PlutusTx.makeLift ''ValidityConfig
PlutusTx.makeIsDataIndexed ''ValidityConfig [('ValidityConfig, 0)]

data PValidityConfig (s :: S)
  = PValidityConfig (Term s (PDataRecord '["starter" ':= PTxOutRef]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PValidityConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PValidityConfig where
  type PLifted _ = ValidityConfig

deriving via
  (DerivePConstantViaData ValidityConfig PValidityConfig)
  instance
    (PConstantDecl ValidityConfig)

data ValidityRedeemer
  = MintFirstFactory
  | MintNewPool
  deriving (Generic, Show)

PlutusTx.makeLift ''ValidityRedeemer
PlutusTx.makeIsDataIndexed
  ''ValidityRedeemer
  [('MintFirstFactory, 0), ('MintNewPool, 1)]

data PValidityRedeemer (s :: S)
  = PMintFirstFactory (Term s (PDataRecord '[]))
  | PMintNewPool (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PValidityRedeemer where
  type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PValidityRedeemer

instance PUnsafeLiftDecl PValidityRedeemer where
  type PLifted _ = ValidityRedeemer

deriving via
  (DerivePConstantViaData ValidityRedeemer PValidityRedeemer)
  instance
    (PConstantDecl ValidityRedeemer)

pvalidateMintFirstFactory :: Term s PTxOutRef -> Term s PCurrencySymbol -> Term s (PBuiltinList PTxInInfo) -> Term s (PValue 'Sorted 'NoGuarantees) -> Term s PBool
pvalidateMintFirstFactory starter ownSymbol inputs mint =
  let starterSpent = pisUTXOSpent # starter # inputs
      mintedOneToken = pisMintingExactAmountForPolicy # 1 # ownSymbol # pconstant C.factoryTokenName # mint
   in pand'List [starterSpent, mintedOneToken]

pvalidityMintingPolicy :: Term s (PValidityConfig :--> PMintingPolicy)
pvalidityMintingPolicy = plam \config rawRedeemer context ->
  let redeemer = ptryFrom @PValidityRedeemer rawRedeemer fst
   in popaque $ unTermCont do
        configF <- pletFieldsC @'["starter"] config
        c <- pletFieldsC @'["purpose", "txInfo"] context
        tx <- pletFieldsC @'["mint", "inputs"] c.txInfo
        let ownSymbol = pownCurrencySymbol c.purpose
        pure $
          perrorIfFalse #$ pmatch redeemer \case
            PMintFirstFactory _ -> pvalidateMintFirstFactory configF.starter ownSymbol tx.inputs tx.mint
            PMintNewPool _ -> pisTokenSpent # ownSymbol # pconstant C.factoryTokenName # tx.inputs

{- |
This validates minting policy for all dex tokens. Notably, there are three kinds of tokens
   - (singleton) factory token = the identifier of a valid factory utxo
   - (per pool) validity token = the identifier of a valid pool utxo
   - (per pool) share tokens   = these represent the user's share of liquidity in the pool. Pool holds a reserve of pre-minted share tokens.

The general logic goes like this:
- First factory token is minted by us at the time of creating the first factory UTxO.
- All other dex tokens can only be minted when there is a factory token spent in the transaction:
   1. Pool validity token can only be minted when creating a new pool and needs to be added there.
   2. Share tokens for every pool are all pre-minted and added into the newly created pool (at the time of pool creation) except for
      a small portion of them that is given to the initial liquidity provider.
   3. New factory token can be created only at the time of factory splitting into two factories and needs to be put into a new factory utxo.

Safety:
   There are two cases we rely on:
     a) Bootstrap = First mint - must spend hard-coded UTxO and mint exactly one factory token and nothing else.
        Note that *factory token* is *unprotected* until it is put into a valid (bootstrap) factory UTxO.
        We rely on the bootstrapper to not do anything nefarious (e.g. involving factory contract) in the meantime!
     b) Subsequent mints - delegates the logic to the factory validator (which must check the minted amounts).
        Note that we rely on the fact that (after valid bootstrapping) factory token can only be on a factory UTxO address with valid factory datum.
        Spending UTxO with factory token therefore implies that the factory validation will run for this UTxO.
-}
validityMintingPolicy :: ValidityConfig -> Script
validityMintingPolicy config = toScript $ pvalidityMintingPolicy # pconstant config

validityCurrencySymbol :: ValidityConfig -> CurrencySymbol
validityCurrencySymbol config = CurrencySymbol $ getScriptHash $ scriptHash (validityMintingPolicy config)
