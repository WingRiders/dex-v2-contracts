{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DEX.Pool (
  PPoolConfig (..),
  PoolConfig (..),
  poolScriptValidatorHash,
  poolScriptAddress,
  pvalidatePoolValidator,
  poolScript,
  poolScriptValidator,
  porder,
) where

import DEX.Constants qualified as C
import DEX.Pool.Util
import DEX.Types.Base (PPoolTypeId)
import DEX.Types.Classes
import DEX.Types.Pool
import DEX.Types.Request
import Plutarch
import Plutarch.Api.V1 (PCredential (..))
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
import Plutarch.Builtin
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext hiding (pisTokenSpent)
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.List (pconvertLists)
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Types.Classes
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Util
import Plutus.Util
import PlutusLedgerApi.V2
import PlutusTx qualified

data PoolConfig = PoolConfig
  { dexSymbol :: CurrencySymbol
  -- ^ We reuse the dex validity minting policy for pools as well
  , agentToken :: TokenName
  -- ^ Only agents with this token can consume LP utxo
  , agentSymbol :: CurrencySymbol
  -- ^ Only agents with this token can consume LP utxo
  , treasuryHolderValidatorHash :: ScriptHash
  -- ^ Treasury holder validator hash, where the treasury is extracted to
  , stakingRewardsSymbol :: CurrencySymbol
  -- ^ Staking rewards are authorized by a minting policy with this currency symbol
  , feeAuthoritySymbol :: CurrencySymbol
  , feeAuthorityToken :: TokenName
  , agentFeeAuthoritySymbol :: CurrencySymbol
  , agentFeeAuthorityToken :: TokenName
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''PoolConfig [('PoolConfig, 0)]
PlutusTx.makeLift ''PoolConfig

data PPoolConfig (s :: S)
  = PPoolConfig
      ( Term
          s
          ( PDataRecord
              '[ "dexSymbol" ':= PCurrencySymbol
               , "agentToken" ':= PTokenName
               , "agentSymbol" ':= PCurrencySymbol
               , "treasuryHolderValidatorHash" ':= PScriptHash
               , "stakingRewardsSymbol" ':= PCurrencySymbol
               , "feeAuthoritySymbol" ':= PCurrencySymbol
               , "feeAuthorityToken" ':= PTokenName
               , "agentFeeAuthoritySymbol" ':= PCurrencySymbol
               , "agentFeeAuthorityToken" ':= PTokenName
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PPoolConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PPoolConfig where
  type PLifted PPoolConfig = PoolConfig

deriving via
  (DerivePConstantViaData PoolConfig PPoolConfig)
  instance
    (PConstantDecl PoolConfig)

{- |
  This validates the evolution of a liquidity pool. In order to be valid:
  1. The pool needs to be valid - has a pool validity token.
  2. A single agent token needs to be present in the transaction inputs.
  3. All pool datum fields except the treasuries (and maybe poolSpecifics) have to remain the same.
  4. Datum for Pool TxOut needs to be inline.
  5. All swap requests need to be applied in order
     Warning ! the validator treats all requests as if they received the apply redeemer
  6. No request included in the transaction can be left out by the agent.
     - Minimum 1 request needs to be applied.
     - Since requests contain the liquidity pool information and that is explicitly checked while parsing
       requests, **at most a 1 LP UTxO can appear in the Tx**. Assuming that no duplicate LPs exist.
  7. Request indices list is not manipulated by the agent. Every request is present exactly once there
     The swap requests are taken first to last in the order given in the redeemer.
     The order of tx outputs matter! The validator assumes a given order of outputs:
        1. Pool
        2. Request outputs in the order of the redeemer
        3. Rest
  8. Transaction validity range is short enough. That means that we can take start timestamp to approximate the current time and
      save this time inside the pool as the timestamp when the pool was evolved for the last time.
  9. For stableswap pool the initialD is correct for pool liquidity.
-}
pvalidatePoolEvolve ::
  forall (t :: PoolType) (s :: S).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  , PIsData (PPoolSpecificDatum t)
  , PTryFrom PData (PPoolDatum t)
  ) =>
  Term s PPoolConfig ->
  Term s (PPoolDatum t) ->
  Term s PPoolTypeId ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PData)))) ->
  Term s PScriptContext ->
  Term s PBool
pvalidatePoolEvolve poolConfig poolDatum poolIdentifier poolLocation agentLocation requestLocations ctx = unTermCont $ do
  config <-
    pletFieldsC
      @'[ "dexSymbol"
        , "agentToken"
        , "agentSymbol"
        , "treasuryHolderValidatorHash"
        , "stakingRewardsSymbol"
        , "feeAuthoritySymbol"
        , "feeAuthorityToken"
        ]
      poolConfig

  context <- pletFieldsC @'["txInfo", "purpose"] ctx
  tx <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "validRange"] context.txInfo
  inputs <- pletC (pconvertLists @PBuiltinList @PList # pfromData tx.inputs)
  inputCount <- pletC (plength # inputs)
  datum <-
    pletFieldsC
      @'[ "requestValidatorHash"
        , "assetASymbol"
        , "assetAToken"
        , "assetBSymbol"
        , "assetBToken"
        , "lastInteraction"
        , "treasuryA"
        , "treasuryB"
        , "projectBeneficiary"
        , "projectTreasuryA"
        , "projectTreasuryB"
        , "reserveTreasuryA"
        , "reserveTreasuryB"
        , "reserveBeneficiary"
        , "swapFeeInBasis"
        , "protocolFeeInBasis"
        , "reserveFeeInBasis"
        , "feeBasis"
        , "projectFeeInBasis"
        , "agentFeeAda"
        , "poolSpecifics"
        ]
      poolDatum

  assetASymbol <- pletC datum.assetASymbol
  assetAToken <- pletC datum.assetAToken
  assetBSymbol <- pletC datum.assetBSymbol
  assetBToken <- pletC datum.assetBToken
  swapFeeInBasis <- pletC datum.swapFeeInBasis
  protocolFeeInBasis <- pletC datum.protocolFeeInBasis
  projectFeeInBasis <- pletC datum.projectFeeInBasis
  reserveFeeInBasis <- pletC datum.reserveFeeInBasis
  feeBasis <- pletC datum.feeBasis
  agentFeeAda <- pletC datum.agentFeeAda
  requestValidatorHash <- pletC datum.requestValidatorHash
  poolSpecifics <- pletC $ pfromData datum.poolSpecifics

  timestamps <- pletC (pfiniteTxValidityRangeTimestamps # tx.validRange)
  validityTimestamp <- pmatchC timestamps

  let agentTxOut = pfield @"resolved" # (pelemAtOptimized # agentLocation # inputs)
  agentInput <- pletFieldsC @'["value", "address"] agentTxOut

  indexedInput <- pletFieldsC @'["outRef", "resolved"] (pelemAtOptimized # poolLocation # inputs)
  ownPoolInput <- pletFieldsC @'["datum", "value", "address"] indexedInput.resolved
  ownInputScriptHash <- pletC (pgetValidatorHashFromScriptAddress # ownPoolInput.address)

  PPair aScale bScale <- pmatchC (pscaling poolSpecifics)
  shareToken <-
    pletC
      ( pshareTokenName
          # poolIdentifier
          # aScale
          # bScale
          # assetASymbol
          # assetAToken
          # assetBSymbol
          # assetBToken
      )

  let newPoolOut = pfindNewPool # ownInputScriptHash # config.dexSymbol # pconstant C.lpValidityTokenName # tx.outputs
  ownPoolOutput <- pletFieldsC @'["datum", "value", "address"] newPoolOut
  newPoolDatum <- pletC (pfromPDatum #$ ptryFromInlineDatum # ownPoolOutput.datum)

  let initialPoolState =
        ppoolStateFromTx
          # datum.treasuryA
          # datum.treasuryB
          # datum.projectTreasuryA
          # datum.projectTreasuryB
          # datum.reserveTreasuryA
          # datum.reserveTreasuryB
          # assetASymbol
          # assetAToken
          # assetBSymbol
          # assetBToken
          # swapFeeInBasis
          # protocolFeeInBasis
          # projectFeeInBasis
          # reserveFeeInBasis
          # feeBasis
          # config.dexSymbol
          # shareToken
          # ownPoolInput.value
          # poolSpecifics
      -- Unfortunately, Script inputs are represented as *set* in Cardano node and thus transaction order is lost.
      -- See https://github.com/input-output-hk/cardano-ledger/blob/6b4871ba6e5a911138af61c8cf6266c27918a78c/alonzo/impl/src/Cardano/Ledger/Alonzo/TxBody.hs#L135 for details
      -- Here we need to reconstruct intended transaction order.
      orderedInputs = porder inputs requestLocations
      -- Outputs are fortunately passed to script 1:1.
      -- First output is reserved for other use.
      orderedOutputs = ptail # tx.outputs

  expectedPoolState <-
    pletC $
      applyRequests
        aScale
        bScale
        config.treasuryHolderValidatorHash
        config.stakingRewardsSymbol
        agentFeeAda
        assetASymbol
        assetAToken
        assetBSymbol
        assetBToken
        config.dexSymbol
        shareToken
        requestValidatorHash
        validityTimestamp.upperBound
        datum.projectBeneficiary
        datum.reserveBeneficiary
        orderedInputs
        orderedOutputs
        initialPoolState

  let expectedPoolDatum =
        pmatch expectedPoolState \state ->
          ppoolDatum
            requestValidatorHash
            (assetASymbol, assetAToken)
            (assetBSymbol, assetBToken)
            validityTimestamp.lowerBound
            (state.qtyTreasuryA, state.qtyTreasuryB)
            datum.projectBeneficiary
            (state.qtyProjectTreasuryA, state.qtyProjectTreasuryB)
            (state.qtyReserveTreasuryA, state.qtyReserveTreasuryB)
            datum.reserveBeneficiary
            swapFeeInBasis
            protocolFeeInBasis
            projectFeeInBasis
            reserveFeeInBasis
            feeBasis
            agentFeeAda
            (fromScott state.poolSpecifics)

  let
    agentIsNotARequestOrPool =
      pmatch
        (pfield @"credential" # agentInput.address)
        ( \case
            PPubKeyCredential _ -> ptrue
            PScriptCredential agentCred' -> plet (pfield @"_0" # agentCred') $
              \agentValHash ->
                pand'List
                  [ agentValHash #/= requestValidatorHash
                  , agentValHash #/= ownInputScriptHash
                  ]
        )

    txOutRefMatches = indexedInput.outRef #== pownRef context.purpose

    oneValidityTokenPresent = pvalueOf # ownPoolInput.value # config.dexSymbol # pconstant C.lpValidityTokenName #== 1
    agentChecked =
      pand'List
        [ ptraceIfFalse "pAI" (agentLocation #/= poolLocation)
        , ptraceIfFalse "pAT" (pvalueOf # agentInput.value # config.agentSymbol # config.agentToken #== 1)
        , ptraceIfFalse "pAR" agentIsNotARequestOrPool
        ]
    -- At minimum there are exactly 2 UTxOs on the input:
    --  1. pool
    --  2. agent
    -- The pool and agent are distinct utxos enforced by the `agentChecked`.
    -- The rest of the inputs are assumed to be requests. We require at least 1 request.
    --  - If a requestLocation pointed at an agent, the agent check would not pass.
    --  - If a requestLocation was point at a pool,
    --    the request parsing checking the correct validatorhash should not pass.
    --    The request validatorhash should be enforced by the factory and cannot be changed.
    -- The check below ensures that no request was skipped. We assume that all TX inputs on
    -- request address are different requests. Dedupe check needs to happen separately.
    noRequestLeftOut =
      pand'List
        [ inputCount #== (2 + plength # requestLocations)
        , inputCount #> 2
        ]
    -- As we use requestsLocations to reconstruct inputs in correct order,
    -- it is necessary that requestsLocations is injective mapping
    noRequestDuplicated = pnot # (pcontainsDuplicate # requestLocations)

    newPoolDatumChecked = newPoolDatum #== expectedPoolDatum

    newPoolValueChecked =
      pcheckNewPoolValue
        # config.dexSymbol
        # pconstant C.lpValidityTokenName
        # shareToken
        # assetASymbol
        # assetAToken
        # assetBSymbol
        # assetBToken
        # ownPoolOutput.value
        # expectedPoolState

    txValidityShortEnough =
      pisTxValidityRangeShortEnough # validityTimestamp.lowerBound # validityTimestamp.upperBound

  pure $
    pand'List
      [ ptraceIfFalse "pP" txOutRefMatches
      , ptraceIfFalse "pI" oneValidityTokenPresent
      , ptraceIfFalse "pX" agentChecked
      , ptraceIfFalse "pH" newPoolDatumChecked
      , ptraceIfFalse "pV" newPoolValueChecked
      , ptraceIfFalse "pO" noRequestLeftOut
      , ptraceIfFalse "pD" noRequestDuplicated
      , ptraceIfFalse "tV" txValidityShortEnough
      ]

{- |
  Goes over all requests and applies them, returning new pool state.
  This function also checks that user gets properly compensated on the output.

  Parsing requests ensures the basic invariants for requests apart from the AMM actions.
  1. The paired output has to belong the beneficiary. The beneficiary can be both
     a wallet address or a script address.
  2. The transaction needs to be inside the requests validity range
  3. The address of the input has to match the requests address
  4. Matching the correct liquidity pool listed in the datum
  5. The agent cannot create unspendable utxos by adding an unknown datum
     a) For script addresses (e.g. treasury) the datum has to be datum passed in the request
     b) For pubkey addresses no datum is allowed (it would also increase costs unnecessarily)

  ! Warning ! The request's and compensation's values are processed as if only relevant assets were
              included in them. The request fulfillment from these assets only checks the relevant
              ones for the action (e.g. swap would not check LP shares on the compensation output).
              The agent has the liberty to choose where the additional tokens should go.
              Intuitively, no unspendable output can be created this way, as all utxos need to fit
              into the already large batch transaction.
-}
applyRequests ::
  forall t (s :: S).
  Pool t =>
  Term s PInteger ->
  Term s PInteger ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s PInteger ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PScriptHash ->
  Term s PPOSIXTime ->
  Term s (PMaybeData (PAsData PAddress)) ->
  Term s (PMaybeData (PAsData PAddress)) ->
  Term s (PList (PPair PTxInInfo PData)) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PPoolState t) ->
  Term s (PPoolState t)
applyRequests
  scaleA
  scaleB
  treasuryHolderValidatorHash
  stakingRewardsSymbol
  agentFeeAda
  assetASymbol
  assetAToken
  assetBSymbol
  assetBToken
  dexSymbol
  shareToken
  expectedRequestValidatorHash
  txValidityRangeEnd
  projectBeneficiary
  reserveBeneficiary
  orderedInputs
  orderedOutputs
  initialPoolState =
    plet
      ( pparseRequest @t
          # scaleA
          # scaleB
          # treasuryHolderValidatorHash
          # stakingRewardsSymbol
          # agentFeeAda
          # assetASymbol
          # assetAToken
          # assetBSymbol
          # assetBToken
          # dexSymbol
          # shareToken
          # expectedRequestValidatorHash
          # txValidityRangeEnd
          # projectBeneficiary
          # reserveBeneficiary
      )
      \parse ->
        -- We allow more outputs
        -- as we only care that each request gets the compensation
        pfoldl2AllowMore
          # plam
            ( \poolState input' output -> unTermCont do
                PPair input specifics <- pmatchC input'
                txInput <- pletC (pfield @"resolved" # input)
                let datum = pfromPDatum #$ ptryFromInlineDatum #$ pfield @"datum" # txInput
                requestParams <- pletC $ parse # txInput # specifics # datum # output
                params <- pmatchC requestParams
                pmatchC params.action >>= \case
                  PSwap te -> do
                    t <- pletFieldsC @'["swapDirection", "minWantedTokens"] te
                    pure (papplySwap t.swapDirection t.minWantedTokens poolState requestParams)
                  PAddLiquidity te -> pure $ papplyAddLiquidity (pfield @"minWantedShares" # te) poolState requestParams
                  PWithdrawLiquidity te -> do
                    t <- pletFieldsC @'["minWantedA", "minWantedB"] te
                    pure (papplyWithdrawLiquidity t.minWantedA t.minWantedB poolState requestParams)
                  PExtractTreasury _ -> pure $ papplyExtractTreasury poolState requestParams
                  PAddStakingRewards _ -> pure $ papplyAddStakingRewards poolState requestParams
                  PExtractProjectTreasury _ -> pure $ papplyExtractProjectTreasury poolState requestParams
                  PExtractReserveTreasury _ -> pure $ papplyExtractReserveTreasury poolState requestParams
            )
          # initialPoolState
          # orderedInputs
          # orderedOutputs

pisActionAllowed ::
  Term s PRequestAction ->
  Term s PAddress ->
  Term s PDatumType ->
  Term s PScriptHash ->
  Term s PCurrencySymbol ->
  Term s (PMaybeData (PAsData PAddress)) ->
  Term s (PMaybeData (PAsData PAddress)) ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s PBool
pisActionAllowed action beneficiary compensationDatumType treasuryHolderValidatorHash stakingRewardsSymbol projectBeneficiary reserveBeneficiary assetASymbol shareToken inputValue outputValue = unTermCont do
  -- There can be at most 5 different tokens in the compensation output, so malicious agent can not make the utxo unspendable.
  -- This was not restricted specifically per action, due to its negative effects on the performance.
  pguardC "aaT" (pcountOfUniqueTokens # outputValue #<= 5)

  pure $
    pmatch action \case
      -- When extracting treasury the beneficiary has to be the treasury
      -- Note: staking credentials are not checked. The agent already controls the staking credentials of the Pool.
      --       This would allow the flexibility to set the staking to the same address as previous pool staking credential,
      --       or the new one. This staking credential would be active temporarily until the treasury
      --       holder was collected by the treasury owners.
      PExtractTreasury _ ->
        ptraceIfFalse
          "rTA"
          ( pmatch
              (pfield @"credential" # beneficiary)
              ( \case
                  PPubKeyCredential _ -> pfalse
                  PScriptCredential cred -> pfield @"_0" # cred #== treasuryHolderValidatorHash
              )
          )
      -- When adding staking rewards, the action needs to be additionally authorized
      PAddStakingRewards _ ->
        ptraceIfFalse "rSA" $
          pisAuthorizedToAddStakingRewards
            stakingRewardsSymbol
            assetASymbol
            shareToken
            inputValue
            outputValue
      -- When extracting the project rewards, the beneficiary must be the project beneficiary from the pool datum
      -- If the project beneficiary is not set, this will always fail
      -- Also, if the beneficiary is a Script, custom datums are not supported. "No" datum option for scripts results in a dummy "EnforcedScriptOutDatum" inline datum
      PExtractProjectTreasury _ ->
        pand'List
          [ ptraceIfFalse "rPTA" (beneficiary #== (pfromData $ pfromDJust # projectBeneficiary))
          , pmatch (pfield @"credential" # beneficiary) \case
              PPubKeyCredential _ -> ptrue
              PScriptCredential _ -> pmatch compensationDatumType \case
                PNo _ -> ptrue
                _ -> pfalse
          ]
      -- The beneficiary must be the just value from the pool datum
      -- Also, if the beneficiary is a Script, custom datums are not supported. "No" datum option for scripts results in a dummy "EnforcedScriptOutDatum" inline datum
      PExtractReserveTreasury _ ->
        pand'List
          [ ptraceIfFalse "rSTA" (beneficiary #== (pfromData $ pfromDJust # reserveBeneficiary))
          , pmatch (pfield @"credential" # beneficiary) \case
              PPubKeyCredential _ -> ptrue
              PScriptCredential _ -> pmatch compensationDatumType \case
                PNo _ -> ptrue
                _ -> pfalse
          ]
      -- Other requests hold their own beneficiary
      _ -> ptrue

pparseRequest ::
  forall t (s :: S).
  Pool t =>
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PScriptHash
        :--> PCurrencySymbol
        :--> PInteger
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PScriptHash
        :--> PPOSIXTime
        :--> PMaybeData (PAsData PAddress)
        :--> PMaybeData (PAsData PAddress)
        :--> PTxOut
        :--> PData
        :--> PRequestDatum
        :--> PTxOut
        :--> PParsedRequest
    )
pparseRequest =
  phoistAcyclic $
    plam $
      \poolScaleA poolScaleB treasuryHolderValidatorHash stakingRewardsSymbol agentFeeAda assetASymbol assetAToken assetBSymbol assetBToken dexSymbol shareToken expectedRequestValidatorHash txValidityRangeEnd projectBeneficiary reserveBeneficiary input additionalData requestDatum' output ->
        unTermCont $ do
          compensationOutput <- pletFieldsC @'["datum", "address", "value"] output
          requestInput <- pletFieldsC @'["address", "datum", "value"] input
          requestDatum <-
            pletFieldsC
              @'[ "assetASymbol"
                , "assetAToken"
                , "assetBSymbol"
                , "assetBToken"
                , "beneficiary"
                , "action"
                , "deadline"
                , "compensationDatum"
                , "compensationDatumType"
                , "oil"
                , "aScale"
                , "bScale"
                ]
              requestDatum'
          beneficiaryCredential <- pletC (pfield @"credential" # requestDatum.beneficiary)

          -- For pubkey address the Datums are currently ignored by the blockchain, but to be consistent
          -- we enforce that no datum hash is included.
          -- For script addresses we enforce the user-provided inline datum is used
          expectedOutDatum <-
            pletC $ pmatch beneficiaryCredential \case
              PScriptCredential _ -> pmatch requestDatum.compensationDatumType \case
                PNo _ -> pcon (POutputDatum (pdcons @"outputDatum" # pdata (pcon . PDatum . pforgetData . pdata $ pconstant EnforcedScriptOutDatum) # pdnil))
                PHash _ -> pcon (POutputDatumHash (pdcons @"datumHash" # pdata (phashDatum # requestDatum.compensationDatum) # pdnil))
                PInline _ -> pcon (POutputDatum (pdcons @"outputDatum" # pdata requestDatum.compensationDatum # pdnil))
              PPubKeyCredential _ -> pcon (PNoOutputDatum pdnil)

          action <- pletC requestDatum.action
          let actionAllowed =
                pisActionAllowed
                  action
                  requestDatum.beneficiary
                  requestDatum.compensationDatumType
                  treasuryHolderValidatorHash
                  stakingRewardsSymbol
                  projectBeneficiary
                  reserveBeneficiary
                  assetASymbol
                  shareToken
                  requestInput.value
                  compensationOutput.value
              actualValidatorHash = pgetValidatorHashFromScriptAddress # requestInput.address

          rOil <- pletC $ pfromData requestDatum.oil
          pguardC "request parsing" $
            ( pand'List $
                [ ptraceIfFalse "raO" (compensationOutput.address #== requestDatum.beneficiary)
                , ptraceIfFalse "raV" (txValidityRangeEnd #< requestDatum.deadline)
                , ptraceIfFalse "rE" (actualValidatorHash #== expectedRequestValidatorHash)
                , ptraceIfFalse
                    "rP"
                    ( pand'List
                        [ requestDatum.assetASymbol #== assetASymbol
                        , requestDatum.assetAToken #== assetAToken
                        , requestDatum.assetBSymbol #== assetBSymbol
                        , requestDatum.assetBToken #== assetBToken
                        ]
                    )
                , ptraceIfFalse "rCD" (compensationOutput.datum #== expectedOutDatum)
                , ptraceIfFalse "rOil" (rOil #>= C.requestOilAda)
                , ptraceIfFalse "rAN" actionAllowed
                ]
                  <> case poolType @t of
                    ConstantProduct -> []
                    -- we additionally validate the scales from the request for stableswap pools
                    Stableswap ->
                      [ requestDatum.aScale #== poolScaleA
                      , requestDatum.bScale #== poolScaleB
                      ]
            )

          pure $
            pcon
              PParsedRequest
                { action
                , provided =
                    pflatAssetsFromValue
                      # assetASymbol
                      # assetAToken
                      # assetBSymbol
                      # assetBToken
                      # dexSymbol
                      # shareToken
                      # requestInput.value
                      # (rOil + agentFeeAda)
                , compensation =
                    pflatAssetsFromValue
                      # assetASymbol
                      # assetAToken
                      # assetBSymbol
                      # assetBToken
                      # dexSymbol
                      # shareToken
                      # compensationOutput.value
                      # rOil
                , additionalData
                }

{- |
  Checks whether the add staking rewards request is authorized
   1. The pool has to be an ADA <> token pool
   2. The request contains a staking rewards token with the tokenName matching the LP assets
   3. The reward token was returned as a compensation
-}
pisAuthorizedToAddStakingRewards ::
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s PBool
pisAuthorizedToAddStakingRewards stakingRewardsSymbol assetASymbol shareToken requestValue compensationValue =
  pand'List
    [ pisAda # assetASymbol
    , pvalueOf # requestValue # stakingRewardsSymbol # shareToken #== 1
    , pvalueOf # compensationValue # stakingRewardsSymbol # shareToken #== 1
    ]

papplyExtractTreasury :: Term s (PPoolState t) -> Term s PParsedRequest -> Term s (PPoolState t)
papplyExtractTreasury poolState parsedRequest = unTermCont do
  state <- pmatchC poolState
  req <- pmatchC parsedRequest
  cAssets <- pmatchC req.compensation
  -- Checking if the treasury is not empty is a DDOS protection that a pool cannot be spammed
  -- with extract treasury requests by a malicious agent or users.
  -- There are no requirements for order of fulfillment for the treasury extraction.
  -- The agents should include at most 1 of these requests per interaction.
  let isTreasuryNotEmpty = state.qtyTreasuryA #> 0 #|| state.qtyTreasuryB #> 0
      newPoolState = state {qtyTreasuryA = 0, qtyTreasuryB = 0}
      isTreasuryFairlyCompensated = pand'List [pfltA cAssets #>= state.qtyTreasuryA, pfltB cAssets #>= state.qtyTreasuryB]
  pguardC "pT" $ pand'List [isTreasuryNotEmpty, isTreasuryFairlyCompensated]
  pure (pcon newPoolState)

papplyExtractProjectTreasury :: Term s (PPoolState t) -> Term s PParsedRequest -> Term s (PPoolState t)
papplyExtractProjectTreasury poolState parsedRequest = unTermCont do
  state <- pmatchC poolState
  request <- pmatchC parsedRequest
  compensation <- pmatchC request.compensation
  -- Checking if the project treasury is not empty is a DDOS protection that a pool cannot be spammed
  -- with extract project treasury requests by a malicious agent or users.
  -- There are no requirements for order of fulfillment for the treasury extraction.
  -- The agents should include at most 1 of these requests per interaction.
  let notEmpty = state.qtyProjectTreasuryA #> 0 #|| state.qtyProjectTreasuryB #> 0
      compensated =
        pand'List
          [ compensation.pfltA #>= state.qtyProjectTreasuryA
          , compensation.pfltB #>= state.qtyProjectTreasuryB
          ]
  pguardC "pT" (pand'List [notEmpty, compensated])
  pure (pcon state {qtyProjectTreasuryA = 0, qtyProjectTreasuryB = 0})

papplyExtractReserveTreasury :: Term s (PPoolState t) -> Term s PParsedRequest -> Term s (PPoolState t)
papplyExtractReserveTreasury poolState parsedRequest = unTermCont do
  state <- pmatchC poolState
  request <- pmatchC parsedRequest
  compensation <- pmatchC request.compensation
  -- Checking if the reserve treasury is not empty is a DDOS protection that a pool cannot be spammed
  -- with extract reserve treasury requests by a malicious agent or users.
  -- There are no requirements for order of fulfillment for the treasury extraction.
  -- The agents should include at most 1 of these requests per interaction.
  let notEmpty = state.qtyReserveTreasuryA #> 0 #|| state.qtyReserveTreasuryB #> 0
      compensated =
        pand'List
          [ compensation.pfltA #>= state.qtyReserveTreasuryA
          , compensation.pfltB #>= state.qtyReserveTreasuryB
          ]
  pguardC "pT" (pand'List [notEmpty, compensated])
  pure (pcon state {qtyReserveTreasuryA = 0, qtyReserveTreasuryB = 0})

{- | This reconstructs the inputs by the list of the given indices.

The naive approach would be to map over the indices and just fetch the corresponding elements.
However, than gives us a O(n) lookup for each index.
Given an array of indices [3,6,1,2,4,5] the naive approach would traverse the inputs 6 times,
albeit not to the end.

A more optimized solution would be to remember the unprocessed rest of the list in-between each lookup.
Then if the indices are rising monotonically, we can look up in the rest of the list, avoiding
the duplicate work of traversing from the very beginning.
In the [3,6,1,2,4,5] example, we would reorder the elements with two iterations:
1. [3,6] -> are rising monotonically and we can look up the 6th element without restarting.
2. [1,2,4,5] -> we restart as 1 < 6, but then we extract all the elements in one traversal as well.

NOTE:
  if supplied with duplicates after each other, will enter an endless loop and exhaust all ex units => error out
  999 is chosen as an initial last' value with the assumption no first index will be actually bigger
  that holds if we have less than 999 requests in the inputs, which should be a safe enough assumption.
  if the batcher tries to game this index and tries to pass in e.g 1000, the initial rest being [] will
  error out on unpacking.
-}
porder ::
  forall a b (s :: S).
  PIsData b =>
  Term s (PList a) ->
  Term s (PBuiltinList (PAsData (PBuiltinPair (PAsData PInteger) (PAsData b)))) ->
  Term s (PList (PPair a b))
porder inputs locations =
  go # inputs # pnil # 999 # locations
  where
    go =
      pfix #$ plam \recur all' rest last' locs' ->
        pelimList
          ( \loc locs ->
              plet (pfromData loc) \i' -> plet (pfromData (pfstBuiltin # i')) \i ->
                pelimList
                  ( \headNewRest tailNewRest ->
                      pcons
                        # ppair headNewRest (pfromData (psndBuiltin # i'))
                        # (recur # all' # tailNewRest # i # locs)
                  )
                  perror
                  (pif (i #< last') (pdrop' # i # all') (pdrop' # (i - last' - 1) # rest))
          )
          (pnil @PList)
          locs'

pcontainsDuplicate :: Term s (PBuiltinList (PAsData (PBuiltinPair (PAsData PInteger) (PAsData a))) :--> PBool)
pcontainsDuplicate = plam $ \l -> (pfix # plam f) # l # (pnil @PList)
  where
    f recur l xs =
      pelimList
        ( \y ys -> plet (pfromData $ pfstBuiltin # pfromData y) \location ->
            pif (pelem # location # xs) ptrue (recur # ys # (pcons # location # xs))
        )
        pfalse
        l

{- |
  This validates an emergency withdrawal of liquidity.
  1. The pool needs to be considered abandoned to validate an emergency withdrawal.
     That means that no Evolve action happened for at least [C.minTimeToConsiderPoolAbandoned] time.
     Additionally the validity range needs to be finite.
  2. Only 1 pool can be present
  3. No requests can be present
  4. The LP value remains sound after the application of the withdrawal
  5. The LP staking address nor the datum haven't changed (apart from the D param for stableswap pools).
     Most importantly the last interaction date hasn't changed.
     This is a protection against a user setting it to the far future, blocking other users
     to withdraw their liquidity. Disabling interaction date change opens up the pool for spamming
     small emergency withdraws.
-}
pvalidateEmergencyWithdrawal ::
  forall (t :: PoolType) (s :: S).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  , PIsData (PPoolSpecificDatum t)
  ) =>
  Term s PPoolConfig ->
  Term s (PPoolDatum t) ->
  Term s PPoolTypeId ->
  Term s PInteger ->
  Term s PScriptContext ->
  Term s PBool
pvalidateEmergencyWithdrawal poolConfig poolDatum poolIdentifier poolLocation ctx = unTermCont $ do
  config <- pletFieldsC @'["dexSymbol", "poolToken"] poolConfig

  context <- pletFieldsC @'["txInfo", "purpose"] ctx
  txInfo <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "validRange"] context.txInfo
  indexedInput <- pletFieldsC @'["outRef", "resolved"] (pelemAtOptimized # poolLocation # pfromData txInfo.inputs)
  let ownRef = pownRef context.purpose

  ownPoolInputFields <- pletFieldsC @'["datum", "value", "address"] indexedInput.resolved
  ownValidatorHash <- pletC $ pgetValidatorHashFromScriptAddress # ownPoolInputFields.address

  timestamps <- pletC (pfiniteTxValidityRangeTimestamps # txInfo.validRange)
  validityTimestamp <- pmatchC timestamps

  datum <-
    pletFieldsC
      @'[ "requestValidatorHash"
        , "assetASymbol"
        , "assetAToken"
        , "assetBSymbol"
        , "assetBToken"
        , "lastInteraction"
        , "treasuryA"
        , "treasuryB"
        , "projectTreasuryA"
        , "projectTreasuryB"
        , "reserveTreasuryA"
        , "reserveTreasuryB"
        , "swapFeeInBasis"
        , "protocolFeeInBasis"
        , "feeBasis"
        , "projectFeeInBasis"
        , "reserveFeeInBasis"
        , "agentFeeAda"
        , "poolSpecifics"
        ]
      poolDatum

  PPair aScale bScale <- pmatchC (pscaling $ pfromData datum.poolSpecifics)
  shareTokenName <-
    pletC
      ( pshareTokenName
          # poolIdentifier
          # aScale
          # bScale
          # datum.assetASymbol
          # datum.assetAToken
          # datum.assetBSymbol
          # datum.assetBToken
      )

  requests <- pletC $ pfilter # (ppaysToCredential # datum.requestValidatorHash #. ptxInInfoResolved) # txInfo.inputs

  -- check that only a single pool output exists
  let ownPoolOutput = pfindNewPool # ownValidatorHash # config.dexSymbol # pconstant C.lpValidityTokenName # txInfo.outputs
  ownPoolOutputFields <- pletFieldsC @'["datum", "value", "address"] ownPoolOutput

  initialPoolStateTerm <-
    pletC $
      ppoolStateFromTx
        # datum.treasuryA
        # datum.treasuryB
        # datum.projectTreasuryA
        # datum.projectTreasuryB
        # datum.reserveTreasuryA
        # datum.reserveTreasuryB
        # datum.assetASymbol
        # datum.assetAToken
        # datum.assetBSymbol
        # datum.assetBToken
        # datum.swapFeeInBasis
        # datum.protocolFeeInBasis
        # datum.projectFeeInBasis
        # datum.reserveFeeInBasis
        # datum.feeBasis
        # config.dexSymbol
        # shareTokenName
        # ownPoolInputFields.value
        # pfromData datum.poolSpecifics
  initialPoolState <- pmatchC initialPoolStateTerm

  oldPoolDatum <- pletC $ ptryFromInlineDatum #$ ownPoolInputFields.datum
  newPoolDatum <- pletC $ ptryFromInlineDatum #$ ownPoolOutputFields.datum

  redeemedShares <-
    pletC $
      pvalueOf # ownPoolOutputFields.value # config.dexSymbol # shareTokenName - initialPoolState.qtyShares

  -- Ensure that we are only removing liquidity
  pguardC "rS" (redeemedShares #> 0)

  -- checks if only a single script is used on our address => no other pools
  _onlyPool <-
    pletC $ passertSingleton "pM" #$ pfilter # (ppaysToCredential # ownValidatorHash #. ptxInInfoResolved) # txInfo.inputs

  let expectedPoolState = premoveLiquidity # initialPoolStateTerm # pnothing # redeemedShares

  pure $
    pand'List
      [ ptraceIfFalse "pP" (indexedInput.outRef #== ownRef)
      , ptraceIfFalse
          "eT"
          ( pto (pfromData datum.lastInteraction)
              + (pconstant C.minTimeToConsiderPoolAbandoned)
              #< pto validityTimestamp.lowerBound
          )
      , -- there can be no request spent in emergency withdrawal transaction
        -- since the requests would validate and let the spender take the funds
        -- locked inside the scripts
        ptraceIfFalse "eR" (pnull # requests)
      , ptraceIfFalse "eS" (ownPoolInputFields.address #== ownPoolOutputFields.address)
      , pisDatumCorrectAfterEmergency @t oldPoolDatum newPoolDatum expectedPoolState
      , ptraceIfFalse "eI" (pvalueOf # ownPoolInputFields.value # config.dexSymbol # pconstant C.lpValidityTokenName #== 1)
      , ptraceIfFalse
          "eV"
          ( pcheckNewPoolValue
              # config.dexSymbol
              # pconstant C.lpValidityTokenName
              # shareTokenName
              # datum.assetASymbol
              # datum.assetAToken
              # datum.assetBSymbol
              # datum.assetBToken
              # ownPoolOutputFields.value
              # expectedPoolState
          )
      ]

{- |
  This validates the fees change.
  1. There is an input with a fee authority token in the inputs
  2. No requests can be present on inputs
  3. Only 1 pool can be present on inputs
  4. Only 1 pool can be present on outputs
  5. The output pool has the same address and value
  6. The only changed datum fields are:
         - swapFeeInBasis
         - protocolFeeInBasis
         - projectFeeInBasis
         - reserveFeeInBasis
         - feeBasis
         - projectBeneficiary (must not be StakePtr addresses)
         - reserveBeneficiary (must not be StakePtr addresses)
  7. The new fees produce positive fees below the swap amounts.

NOTE: The holders of the fee auth token may set the beneficiaries to any address at any time
      since they have the control over the project and reserve treasuries.
-}
pvalidateChangeFees ::
  forall t (s :: S).
  PTryFrom PData (PPoolDatum t) =>
  Term s PPoolConfig ->
  Term s (PPoolDatum t) ->
  Term s PScriptContext ->
  Term s PBool
pvalidateChangeFees poolConfig ownDatum ctx = unTermCont do
  config <- pletFieldsC @'["feeAuthoritySymbol", "feeAuthorityToken"] poolConfig
  context <- pletFieldsC @'["txInfo", "purpose"] ctx
  tx <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "validRange"] context.txInfo

  let ownInput = pownInput # context.purpose # tx.inputs
  ownInputF <- pletFieldsC @'["datum", "value", "address"] ownInput
  selfValidatorHash <- pletC $ pgetValidatorHashFromScriptAddress # ownInputF.address

  -- We don't need to check the pool token
  -- as we can just allow one pool output that preserves the value
  -- and additionally disallow any requests
  PTriplet ownOutAddress ownOutValue ownOutDatum <-
    pmatchC
      ( passertSingleton "one pool output expected"
          # (pfindScriptOutputsWithAddress # selfValidatorHash # tx.outputs)
      )

  ownInputDatumF <-
    pletFieldsC
      @'[ "requestValidatorHash"
        , "assetASymbol"
        , "assetAToken"
        , "assetBSymbol"
        , "assetBToken"
        , "lastInteraction"
        , "agentFeeAda"
        , "treasuryA"
        , "treasuryB"
        , "projectTreasuryA"
        , "projectTreasuryB"
        , "reserveTreasuryA"
        , "reserveTreasuryB"
        , "poolSpecifics"
        ]
      ownDatum
  ownOutDatumF <-
    pletFieldsC
      @'[ "requestValidatorHash"
        , "assetASymbol"
        , "assetAToken"
        , "assetBSymbol"
        , "assetBToken"
        , "swapFeeInBasis"
        , "protocolFeeInBasis"
        , "projectFeeInBasis"
        , "reserveFeeInBasis"
        , "feeBasis"
        , "lastInteraction"
        , "treasuryA"
        , "treasuryB"
        , "projectTreasuryA"
        , "projectTreasuryB"
        , "reserveTreasuryA"
        , "reserveTreasuryB"
        , "agentFeeAda"
        , "projectBeneficiary"
        , "reserveBeneficiary"
        , "poolSpecifics"
        ]
      (pfromPDatum @(PPoolDatum t) # (ptryFromInlineDatum # ownOutDatum))
  feeBasis <- pletC $ pfromData ownOutDatumF.feeBasis
  swapFeeInBasis <- pletC $ pfromData ownOutDatumF.swapFeeInBasis
  protocolFeeInBasis <- pletC $ pfromData ownOutDatumF.protocolFeeInBasis
  projectFeeInBasis <- pletC $ pfromData ownOutDatumF.projectFeeInBasis
  reserveFeeInBasis <- pletC $ pfromData ownOutDatumF.reserveFeeInBasis

  let requests = pfilter # (ppaysToCredential # ownInputDatumF.requestValidatorHash #. ptxInInfoResolved) # tx.inputs
      pools = pfilter # (ppaysToCredential # selfValidatorHash #. ptxInInfoResolved) # tx.inputs

  pure $
    pand'List
      [ pnull # requests
      , plength # pools #== 1
      , phaveSameStakingCredentials # ownInputF.address # ownOutAddress
      , ownInputF.value #== ownOutValue
      , -- All the fields except the fees are the same
        ownInputDatumF.requestValidatorHash #== ownOutDatumF.requestValidatorHash
      , ownInputDatumF.assetASymbol #== ownOutDatumF.assetASymbol
      , ownInputDatumF.assetAToken #== ownOutDatumF.assetAToken
      , ownInputDatumF.assetBSymbol #== ownOutDatumF.assetBSymbol
      , ownInputDatumF.assetBToken #== ownOutDatumF.assetBToken
      , ownInputDatumF.lastInteraction #== ownOutDatumF.lastInteraction
      , ownInputDatumF.treasuryA #== ownOutDatumF.treasuryA
      , ownInputDatumF.treasuryB #== ownOutDatumF.treasuryB
      , ownInputDatumF.projectTreasuryA #== ownOutDatumF.projectTreasuryA
      , ownInputDatumF.projectTreasuryB #== ownOutDatumF.projectTreasuryB
      , ownInputDatumF.reserveTreasuryA #== ownOutDatumF.reserveTreasuryA
      , ownInputDatumF.reserveTreasuryB #== ownOutDatumF.reserveTreasuryB
      , ownInputDatumF.poolSpecifics #== ownOutDatumF.poolSpecifics
      , ownInputDatumF.agentFeeAda #== ownOutDatumF.agentFeeAda
      , -- the fees make sense
        feeBasis #> 0
      , feeBasis #<= pconstant C.maxInt64
      , swapFeeInBasis #> 0
      , protocolFeeInBasis #> 0
      , projectFeeInBasis #>= 0
      , reserveFeeInBasis #>= 0
      , -- the fees are less than 100%
        (swapFeeInBasis + protocolFeeInBasis + projectFeeInBasis + reserveFeeInBasis) #< feeBasis
      , -- there is an authority token among the inputs
        pisTokenSpent # config.feeAuthoritySymbol # config.feeAuthorityToken # tx.inputs
      , -- make sure projectBeneficiary and reserveBeneficiary are not StakePtr addresses
        pmatch ownOutDatumF.projectBeneficiary \case
          PDNothing _ -> ptrue
          PDJust pAddr ->
            plet (pfromData $ pfield @"_0" # pAddr) \projectBeneficiaryAddr ->
              pnot # (pisStakePtrAddress projectBeneficiaryAddr)
      , pmatch ownOutDatumF.reserveBeneficiary \case
          PDNothing _ -> ptrue
          PDJust rAddr ->
            plet (pfromData $ pfield @"_0" # rAddr) \reserveBeneficiaryAddr ->
              pnot # (pisStakePtrAddress reserveBeneficiaryAddr)
      ]

{- |
  This validates the agent fee change
  1. There is an input with an agent fee authority token in the inputs
  2. No requests can be present on inputs
  3. Only 1 pool can be present on inputs
  4. Only 1 pool can be present on outputs
  5. The output pool has the same address and value
  6. The only changed datum field is:
         - agentFeeAda
  7. The new agent fee is positive
-}
pvalidateChangeAgentFee ::
  forall t (s :: S).
  PTryFrom PData (PPoolDatum t) =>
  Term s PPoolConfig ->
  Term s (PPoolDatum t) ->
  Term s PScriptContext ->
  Term s PBool
pvalidateChangeAgentFee poolConfig ownDatum ctx = unTermCont do
  config <- pletFieldsC @'["agentFeeAuthoritySymbol", "agentFeeAuthorityToken"] poolConfig
  context <- pletFieldsC @'["txInfo", "purpose"] ctx
  tx <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "validRange"] context.txInfo

  let ownInput = pownInput # context.purpose # tx.inputs
  ownInputF <- pletFieldsC @'["datum", "value", "address"] ownInput
  selfValidatorHash <- pletC $ pgetValidatorHashFromScriptAddress # ownInputF.address

  -- We don't need to check the pool token
  -- as we can just allow one pool output that preserves the value
  -- and additionally disallow any requests
  PTriplet ownOutAddress ownOutValue ownOutDatum <-
    pmatchC
      ( passertSingleton "one pool output expected"
          # (pfindScriptOutputsWithAddress # selfValidatorHash # tx.outputs)
      )

  ownInputDatumF <-
    pletFieldsC
      @'[ "requestValidatorHash"
        , "assetASymbol"
        , "assetAToken"
        , "assetBSymbol"
        , "assetBToken"
        , "lastInteraction"
        , "treasuryA"
        , "treasuryB"
        , "projectTreasuryA"
        , "projectTreasuryB"
        , "reserveTreasuryA"
        , "reserveTreasuryB"
        , "projectBeneficiary"
        , "swapFeeInBasis"
        , "protocolFeeInBasis"
        , "projectFeeInBasis"
        , "reserveFeeInBasis"
        , "feeBasis"
        , "reserveBeneficiary"
        , "poolSpecifics"
        ]
      ownDatum
  ownOutDatumF <-
    pletFieldsC
      @'[ "requestValidatorHash"
        , "assetASymbol"
        , "assetAToken"
        , "assetBSymbol"
        , "assetBToken"
        , "swapFeeInBasis"
        , "protocolFeeInBasis"
        , "projectFeeInBasis"
        , "reserveFeeInBasis"
        , "feeBasis"
        , "agentFeeAda"
        , "lastInteraction"
        , "treasuryA"
        , "treasuryB"
        , "projectTreasuryA"
        , "projectTreasuryB"
        , "reserveTreasuryA"
        , "reserveTreasuryB"
        , "projectBeneficiary"
        , "reserveBeneficiary"
        , "poolSpecifics"
        ]
      (pfromPDatum @(PPoolDatum t) # (ptryFromInlineDatum # ownOutDatum))

  let requests = pfilter # (ppaysToCredential # ownInputDatumF.requestValidatorHash #. ptxInInfoResolved) # tx.inputs
      pools = pfilter # (ppaysToCredential # selfValidatorHash #. ptxInInfoResolved) # tx.inputs

  pure $
    pand'List
      [ pnull # requests
      , plength # pools #== 1
      , phaveSameStakingCredentials # ownInputF.address # ownOutAddress
      , ownInputF.value #== ownOutValue
      , -- All the fields but the agent fee are the same
        ownInputDatumF.requestValidatorHash #== ownOutDatumF.requestValidatorHash
      , ownInputDatumF.assetASymbol #== ownOutDatumF.assetASymbol
      , ownInputDatumF.assetAToken #== ownOutDatumF.assetAToken
      , ownInputDatumF.assetBSymbol #== ownOutDatumF.assetBSymbol
      , ownInputDatumF.assetBToken #== ownOutDatumF.assetBToken
      , ownInputDatumF.lastInteraction #== ownOutDatumF.lastInteraction
      , ownInputDatumF.treasuryA #== ownOutDatumF.treasuryA
      , ownInputDatumF.treasuryB #== ownOutDatumF.treasuryB
      , ownInputDatumF.projectTreasuryA #== ownOutDatumF.projectTreasuryA
      , ownInputDatumF.projectTreasuryB #== ownOutDatumF.projectTreasuryB
      , ownInputDatumF.reserveTreasuryA #== ownOutDatumF.reserveTreasuryA
      , ownInputDatumF.reserveTreasuryB #== ownOutDatumF.reserveTreasuryB
      , ownInputDatumF.projectBeneficiary #== ownOutDatumF.projectBeneficiary
      , ownInputDatumF.poolSpecifics #== ownOutDatumF.poolSpecifics
      , ownInputDatumF.swapFeeInBasis #== ownOutDatumF.swapFeeInBasis
      , ownInputDatumF.protocolFeeInBasis #== ownOutDatumF.protocolFeeInBasis
      , ownInputDatumF.projectFeeInBasis #== ownOutDatumF.projectFeeInBasis
      , ownInputDatumF.reserveFeeInBasis #== ownOutDatumF.reserveFeeInBasis
      , ownInputDatumF.feeBasis #== ownOutDatumF.feeBasis
      , ownInputDatumF.reserveBeneficiary #== ownOutDatumF.reserveBeneficiary
      , -- the new agent fee is non-negative
        pfromData ownOutDatumF.agentFeeAda #>= 0
      , -- the new agent fee is not maliciously high
        pfromData ownOutDatumF.agentFeeAda #<= pconstant C.maxInt64
      , -- there is an authority token among the inputs
        pisTokenSpent # config.agentFeeAuthoritySymbol # config.agentFeeAuthorityToken # tx.inputs
      ]

pvalidatePool ::
  forall (t :: PoolType) (s :: S).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , PIsData (PPoolSpecificDatum t)
  , PTryFrom PData (PPoolDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  ) =>
  Term s PPoolConfig ->
  Term s (PPoolDatum t) ->
  Term s PPoolRedeemer ->
  Term s PScriptContext ->
  Term s PUnit
pvalidatePool poolConfig poolDatum poolRedeemer ctx = unTermCont do
  poolIdentifier <- pletC (ppoolIdentifier @t)
  pure $ perrorIfFalse #$ pmatch poolRedeemer $ \case
    PEvolve r -> unTermCont $ do
      fields <- pletFieldsC @'["poolLocation", "agentLocation", "requestLocations"] r
      pure $
        pvalidatePoolEvolve @t
          poolConfig
          poolDatum
          poolIdentifier
          fields.poolLocation
          fields.agentLocation
          fields.requestLocations
          ctx
    PEmergencyWithdrawal r ->
      pvalidateEmergencyWithdrawal
        poolConfig
        poolDatum
        poolIdentifier
        (pfield @"poolLocation" # r)
        ctx
    PChangeFees _ -> pvalidateChangeFees poolConfig poolDatum ctx
    PChangeAgentFee _ -> pvalidateChangeAgentFee poolConfig poolDatum ctx

pvalidatePoolValidator ::
  forall (t :: PoolType) (s :: S).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , PIsData (PPoolSpecificDatum t)
  , PTryFrom PData (PPoolDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  ) =>
  Term s (PPoolConfig :--> PValidator)
pvalidatePoolValidator = plam \config rawDatum rawRedeemer ctx ->
  let datum = pfromData $ punsafeCoerce rawDatum
      redeemer = punsafeCoerce rawRedeemer
   in popaque $ pvalidatePool @t config datum redeemer ctx

poolScriptValidator ::
  forall (t :: PoolType).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , PIsData (PPoolSpecificDatum t)
  , PTryFrom PData (PPoolDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  ) =>
  PoolConfig ->
  Script
poolScriptValidator config = toScript (pvalidatePoolValidator @t # pconstant config)

poolScript ::
  forall (t :: PoolType).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , PIsData (PPoolSpecificDatum t)
  , PTryFrom PData (PPoolDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  ) =>
  Script
poolScript = toScript (pvalidatePoolValidator @t)

poolScriptValidatorHash ::
  forall (t :: PoolType).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , PIsData (PPoolSpecificDatum t)
  , PTryFrom PData (PPoolDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  ) =>
  PoolConfig ->
  ScriptHash
poolScriptValidatorHash = scriptHash . (poolScriptValidator @t)

poolScriptAddress ::
  forall (t :: PoolType).
  ( Pool t
  , ScottConvertible (PPoolSpecificDatum t)
  , PIsData (PPoolSpecificDatum t)
  , PTryFrom PData (PPoolDatum t)
  , ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t
  ) =>
  PoolConfig ->
  Address
poolScriptAddress = scriptHashToAddress . (poolScriptValidatorHash @t)
