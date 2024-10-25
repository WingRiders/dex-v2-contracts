module DEX.Request where

import DEX.Types.Request (
  PRequestDatum (..),
  PRequestRedeemer (..),
 )
import Plutarch
import Plutarch.Api.V2 (
  PScriptContext,
  PScriptHash,
  PValidator,
  scriptHash,
 )
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Util
import Plutus.Util
import PlutusLedgerApi.V2 (
  Address,
  ScriptHash,
 )

type RequestConfig = ScriptHash

newtype PRequestConfig (s :: S) = PRequestConfig (Term s (PAsData PScriptHash))
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PRequestConfig where
  type DPTStrat _ = PlutusTypeNewtype

{-
--  Safety: Request validation needs to ensure that either
--  a) user is correctly reclaiming money, or
--  b) the request will be applied to the pool.
--
--  Note that due to efficiency reasons, the request validators try to run as little code as possible and instead delegates
--  all relevant checks to the pool - this is to avoid unnecessary parsing and validation which is costly and would be redundant.
--
--  Note that the checks in both use-cases are *local* in scope which means there might be multiple requests
--  with different actions.
--  We deal with this with a following way:
--   - if there exists an Apply request, `validatePool` will be run
--   - if there is a valid pool present, the pool processes requests as if only Apply requests were
--     in the transaction. The pool ignores the requests' redeemer and checks that all are correctly applied.
--     ! Warning ! In case that there is a request as part of the transaction which received a different redeemer,
--     `validatePool` still treats it is as an Apply request, hence checking if it is applied correctly.
--   - if a pool is not present, it follows that transaction contains only a mix of Reclaim requests.
--     Here we assume that if the user (co-)signed the transaction, the user is OK with resulting transaction outputs (even if this is multi-request interaction)

--  Note that it is possible for a transaction including pool and a mixture of requests with Apply and Reclaim to be successful,
--   - if all requests (including the ones with Reclaim) are correctly applied, as checked in `validatePool`
--   - if all owners of Reclaim requests sign the transaction, hence are responsible for their own actions
--   - if agent composes this transaction and all other checks from `validatePool`
--  The agent is responsible to create transactions only Applying requests. In any case, contracts still ensure:
--   - pool funds are kept intact as if all requests were applied and none was reclaimed
--   - user funds apart from signers are not impacted, as only requests that can be applied are applied by the pool
-}

{- |
  Apply a request against a liquidity pool.

  Safety: We have to verify that the transaction contains a pool validator,
          which takes over responsibility of checking request outputs.
  We use a following proxy check to attestate that `validatePool` will be run
  - `poolIdx` should contain a hint of which input is the pool input
  - we verify this input has the correct validator hash
  - therefore the transaction has to run a pool evolve or one of the other pool paths
  - only the pool evolve path allows any requests to be present

   Note that this logic also delegates checking of deadlines, correct datum contents to the pool due to efficiency.
-}
pvalidateApply :: Term s (PScriptHash :--> PInteger :--> PScriptContext :--> PBool)
pvalidateApply = plam $ \poolHash poolIdx scriptContext ->
  let delegateeValidatorHash = pextractNthInputValidatorHash # scriptContext # poolIdx
   in ptraceIfFalse "raL" (delegateeValidatorHash #== poolHash)

{- |
  The user can reclaim the request at any given point in time.

  Note: There is no limitation for how many requests can be reclaimed at once
        as long as all owners sign the transaction.

  Warning: Since the reclaim can happen at any time, there is a chance that it would block the agent's ability
           to create batch transactions in the same block. A DDOS attack attempt would be possible and
           the agent's offchain logic needs to guard against that. By enforcing FIFO ordering of requests,
           in the presence of valid requests, such attack would effectively only slow down request fulfillment.
-}
pvalidateReclaim :: Term s (PRequestDatum :--> PScriptContext :--> PBool)
pvalidateReclaim = phoistAcyclic $ plam $ \datum context ->
  let owner = pfield @"owner" # datum
      txInfo = pfield @"txInfo" # context
      signatories = pfield @"signatories" # txInfo
      ownerHash = paddressPubKeyCredential # owner
      -- We have to verify that this is signed by the owner. The owner signature is enough
      -- to guarantee funds are returned to the correct owner.
      signedByOwner = ptxSignedByPkh # ownerHash # signatories
   in ptraceIfFalse "rrS" signedByOwner

prequestScriptValidator ::
  Term s (PRequestConfig :--> PRequestDatum :--> PRequestRedeemer :--> PScriptContext :--> PUnit)
prequestScriptValidator = phoistAcyclic $ plam $ \config datum redeemer scriptContext ->
  perrorIfFalse #$ pmatch redeemer $ \case
    PApply index -> pvalidateApply # pfromData (pto config) # (pfield @"poolInputLocation" # index) # scriptContext
    PReclaim _ -> pvalidateReclaim # datum # scriptContext

requestValidator :: Term s (PRequestConfig :--> PValidator)
requestValidator = plam $ \config rawDatum rawRedeemer ctx ->
  let datum = punsafeCoerce rawDatum
      redeemer = punsafeCoerce rawRedeemer
   in popaque $ prequestScriptValidator # config # datum # redeemer # ctx

requestScript :: Script
requestScript = toScript requestValidator

requestScriptValidatorTerm :: RequestConfig -> Term s PValidator
requestScriptValidatorTerm config = requestValidator # pcon (PRequestConfig (pdata (pconstant config)))

requestScriptValidator :: RequestConfig -> Script
requestScriptValidator config = toScript $ requestValidator # pcon (PRequestConfig (pdata (pconstant config)))

requestScriptValidatorHash :: RequestConfig -> ScriptHash
requestScriptValidatorHash = scriptHash . requestScriptValidator

requestScriptAddress :: RequestConfig -> Address
requestScriptAddress = scriptHashToAddress . requestScriptValidatorHash
