module DEX.Treasury.Holder where

import DEX.Types.Treasury
import Plutarch (Script)
import Plutarch.Api.V2 (
  PScriptContext,
  PValidator,
  scriptHash,
 )
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import Plutarch.Unsafe
import Plutarch.Util
import Plutus.Missing ()
import PlutusLedgerApi.V1.Value (
  unAssetClass,
 )
import PlutusLedgerApi.V2 (ScriptHash)

{-
  The treasury holders are a set of UTxOs protected by treasury voting tokens.
  To be able to collect the funds locked inside the treasury holders,
  the transaction needs to include a minimum number of voting tokens.
  There is a limited number of treasury tokens available. At least minVoteCount
  amount of tokens needs to be presented to allow an action.
-}

pvalidateTreasuryHolder ::
  Term s (PTreasuryHolderParameters :--> PData :--> PTreasuryHolderRedeemer :--> PScriptContext :--> PUnit)
pvalidateTreasuryHolder = phoistAcyclic $ plam $ \params _datum redeemer context ->
  perrorIfFalse
    #$ pmatch
      redeemer
      ( \case
          PCollect _ ->
            ( pmatch params $ \p ->
                let txInfo = pfield @"txInfo" # context
                    inputs = pfield @"inputs" # txInfo

                    voteCount = pvalueOfInputs # pvoteCurrencySymbol p # pvoteTokenName p # inputs
                    enoughVotesPresent = voteCount #> pminVoteCount p
                 in ptraceIfFalse "thV" enoughVotesPresent
            )
      )

pvalidateTreasuryHolderValidator :: Term s (PTreasuryHolderParameters :--> PValidator)
pvalidateTreasuryHolderValidator = phoistAcyclic $ plam $ \params datum rawRedeemer ctx ->
  let redeemer = punsafeCoerce rawRedeemer in popaque $ pvalidateTreasuryHolder # params # datum # redeemer # ctx

treasuryHolderValidator :: TreasuryHolderParameters -> Script
treasuryHolderValidator params =
  toScript $
    pvalidateTreasuryHolderValidator
      # pcon
        ( PTreasuryHolderParameters
            (pconstant cs)
            (pconstant tn)
            (pconstant (totalVoteCount params))
            (pconstant (minVoteCount params))
        )
  where
    (cs, tn) = unAssetClass (voteTokenClass params)

treasuryHolderScriptValidatorHash :: TreasuryHolderParameters -> ScriptHash
treasuryHolderScriptValidatorHash = scriptHash . treasuryHolderValidator
