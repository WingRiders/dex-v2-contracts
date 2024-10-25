module Other.FixedSupplyPolicy where

import Plutarch (
  Script,
  Term,
  plam,
  popaque,
  unTermCont,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PTxInInfo,
  PTxInfo,
  PTxOutRef,
  scriptHash,
 )
import Plutarch.Extra.TermCont (pletFieldsC)
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude (
  PEq ((#==)),
  PMaybe,
  PUnit,
  pconstant,
  pfield,
  pfind,
  ptraceIfFalse,
 )
import Plutarch.Util (
  perrorIfFalse,
  pisJust,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  ScriptHash (..),
  TxOutRef,
 )

fixedSupplyPolicyUnapplied :: Script
fixedSupplyPolicyUnapplied = toScript pfixedSupplyMintingPolicy

fixedSupplyPolicy :: TxOutRef -> Script
fixedSupplyPolicy oref = toScript (pfixedSupplyMintingPolicy # pconstant oref)

fixedSupplyCurrencySymbol :: TxOutRef -> CurrencySymbol
fixedSupplyCurrencySymbol oref = CurrencySymbol $ getScriptHash $ scriptHash (fixedSupplyPolicy oref)

pfixedSupplyMintingPolicy :: Term s (PTxOutRef :--> PMintingPolicy)
pfixedSupplyMintingPolicy = plam $ \oref _rawRedeemer ctx -> popaque $ pvalidateFixedSupply # oref # ctx

pvalidateFixedSupply :: Term s (PTxOutRef :--> PScriptContext :--> PUnit)
pvalidateFixedSupply = plam $ \oref ctx -> perrorIfFalse #$ unTermCont $ do
  context <- pletFieldsC @'["txInfo", "purpose"] ctx

  let utxoSpent = pisJust #$ pfindTxInByTxOutRef # oref # context.txInfo

  pure (ptraceIfFalse "UTxO not spent" utxoSpent)

pfindTxInByTxOutRef :: Term s (PTxOutRef :--> PTxInfo :--> PMaybe PTxInInfo)
pfindTxInByTxOutRef = plam $ \outRef txInfo ->
  let f inInfo = pfield @"outRef" # inInfo #== outRef in pfind # plam f # (pfield @"inputs" # txInfo)
