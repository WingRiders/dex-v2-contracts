{-# LANGUAGE BlockArguments #-}

module DEX.Staking.RewardMint where

import GHC.Generics as GHC hiding (
  S,
 )
import Plutarch
import Plutarch.Api.V1.AssocMap hiding (
  pany,
 )
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.TermCont
import Plutarch.Mint.Util (plookupCurrency)
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude (ptraceError)
import Plutarch.Prelude hiding (ptraceError)
import Plutarch.Unsafe
import Plutarch.Util
import PlutusLedgerApi.V1.Value (
  AssetClass,
  unAssetClass,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  TokenName,
  getScriptHash,
 )
import PlutusTx qualified

data MintRewardTokensRedeemer
  = MintRewardToken TokenName -- for a given liquidity pool identified by its share token name
  | BurnRewardTokens
  deriving (Show, GHC.Generic)

PlutusTx.makeIsDataIndexed
  ''MintRewardTokensRedeemer
  [ ('MintRewardToken, 0)
  , ('BurnRewardTokens, 1)
  ]
PlutusTx.makeLift ''MintRewardTokensRedeemer

data PMintRewardTokensRedeemer (s :: S)
  = PMintRewardTokensRedeemer (Term s (PDataRecord '["poolShareTokenName" ':= PTokenName]))
  | PBurnRewardsTokens (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PMintRewardTokensRedeemer where
  type DPTStrat _ = PlutusTypeData

pvalidateMintSingleRewardToken ::
  Term s (PCurrencySymbol :--> PTokenName :--> PTokenName :--> PScriptContext :--> PBool)
pvalidateMintSingleRewardToken =
  phoistAcyclic $ plam $ \stakingAgentCurrencySymbol stakingAgentTokenName poolShareTokenName context -> unTermCont $ do
    contextFields <- tcont (pletFields @'["txInfo", "purpose"] context)

    let txInfo = contextFields.txInfo
    txInfoFields <- tcont (pletFields @'["mint", "inputs"] txInfo)

    let mintedValue = txInfoFields.mint
        inputs = txInfoFields.inputs

    PMinting policy <- pmatchC contextFields.purpose

    let mintedMap = pto (pfromData mintedValue)
        ownPolicyId = pfield @"_0" # policy
        allTokens = plookup # ownPolicyId # mintedMap
        ownTokens =
          pmatch
            allTokens
            ( \case
                PJust te -> pto te
                PNothing -> ptraceError "rtNA"
            )
    singleMintedToken <- pletC (passertSingleton "rtS" # ownTokens)
    let tn = pfromData (pfstBuiltin # singleMintedToken)
        qty = pfromData (psndBuiltin # singleMintedToken)

        pisAuthorizedToAddStakingRewards = pisTokenSpent # stakingAgentCurrencySymbol # stakingAgentTokenName # inputs
        isMintingSingleToken = ptraceIfFalse "rtT" (tn #== poolShareTokenName) #&& ptraceIfFalse "rtQ" (qty #== 1)

    pure (ptraceIfFalse "rtMA" pisAuthorizedToAddStakingRewards #&& ptraceIfFalse "rtMT" isMintingSingleToken)

pvalidateBurnRewardTokens :: Term s (PCurrencySymbol :--> PTokenName :--> PScriptContext :--> PBool)
pvalidateBurnRewardTokens = phoistAcyclic $ plam $ \stakingAgentCurrencySymbol stakingAgentTokenName context ->
  unTermCont $ do
    contextFields <- tcont (pletFields @'["txInfo", "purpose"] context)

    let txInfo = contextFields.txInfo
    txInfoFields <- tcont (pletFields @'["mint", "inputs"] txInfo)

    let mintedValue = txInfoFields.mint
        inputs = txInfoFields.inputs

    PMinting policy <- pmatchC contextFields.purpose

    let ownPolicyId = pfield @"_0" # policy

    let isAuthorizedByStakingAgentToken = pisTokenSpent # stakingAgentCurrencySymbol # stakingAgentTokenName # inputs
        isBurningTokens =
          pmatch (plookupCurrency mintedValue ownPolicyId) \case
            PNothing -> ptrue
            PJust m -> pnot #$ AssocMap.pany # plam (#> 0) # m
    pure (ptraceIfFalse "rtA" isAuthorizedByStakingAgentToken #&& ptraceIfFalse "rtB" isBurningTokens)

pvalidateStakingReward ::
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PMintRewardTokensRedeemer
        :--> PScriptContext
        :--> PUnit
    )
pvalidateStakingReward =
  phoistAcyclic $ plam $ \stakingAgentCurrencySymbol stakingAgentTokenName redeemer context ->
    perrorIfFalse
      #$ pmatch
        redeemer
        ( \case
            PMintRewardTokensRedeemer poolShareTokenName ->
              pvalidateMintSingleRewardToken
                # stakingAgentCurrencySymbol
                # stakingAgentTokenName
                # (pfield @"poolShareTokenName" # poolShareTokenName)
                # context
            PBurnRewardsTokens _ -> pvalidateBurnRewardTokens # stakingAgentCurrencySymbol # stakingAgentTokenName # context
        )

pstakingRewardsTokenPolicy :: Term s (PCurrencySymbol :--> PTokenName :--> PMintingPolicy)
pstakingRewardsTokenPolicy =
  phoistAcyclic $ plam $ \stakingAgentCurrencySymbol stakingAgentTokenName rawRedeemer ctx ->
    let redeemer = punsafeCoerce rawRedeemer
     in popaque $ pvalidateStakingReward # stakingAgentCurrencySymbol # stakingAgentTokenName # redeemer # ctx

stakingRewardsTokenPolicy :: AssetClass -> Script
stakingRewardsTokenPolicy stakingAgentToken = toScript $ pstakingRewardsTokenPolicy # pconstant cs # pconstant tn
  where
    (cs, tn) = unAssetClass stakingAgentToken

stakingRewardsTokenCurrency :: AssetClass -> CurrencySymbol
stakingRewardsTokenCurrency stakingAgentToken = CurrencySymbol $ getScriptHash $ scriptHash (stakingRewardsTokenPolicy stakingAgentToken)
