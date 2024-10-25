{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DEX.Pool.Stableswap where

import DEX.Constants (maxShareTokens, protocolStableswapA)
import DEX.Constants qualified as C
import DEX.Types.Classes
import DEX.Types.Pool
import DEX.Types.Request
import Plutarch
import Plutarch.Api.V2 (PCurrencySymbol, PDatum (..), PScriptHash, PTokenName)
import Plutarch.Bool (pand')
import Plutarch.Builtin (pasInt, pasList)
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Num (pabs)
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Types.Classes
import Plutarch.Util
import PlutusTx qualified

data PStableswapPoolState (s :: S) = PStableswapPoolState
  { parameterD :: Term s PInteger
  , aScale :: Term s PInteger
  , bScale :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PEq)

instance DerivePlutusType PStableswapPoolState where
  type DPTStrat _ = PlutusTypeScott

data StableswapPoolDatum = StableswapPoolDatum
  { parameterD :: Integer
  , aScale :: Integer
  -- ^ Scales the token A before each operations
  --   the primary usecase is supporting stable coins with different decimals.
  --   Given aScale = 1 and bScale = 10 means that one B token is worth 10 tokens A,
  --   and the user expects to get 10 tokens A for each token B.
  --   Must be positive. One of a, b must be 1.
  , bScale :: Integer
  -- ^ Scales the token B before each operations
  --   the primary usecase is supporting stable coins with different decimals.
  --   Given aScale = 1 and bScale = 10 means that one B token is worth 10 tokens A,
  --   and the user expects to get 10 tokens A for each token B.
  --   Must be positive. One of a, b must be 1.
  }
  deriving stock (Generic, Show, Eq)

PlutusTx.makeIsDataIndexed ''StableswapPoolDatum [('StableswapPoolDatum, 0)]
PlutusTx.makeLift ''StableswapPoolDatum

data PStableswapPoolDatum (s :: S)
  = PStableswapPoolDatum
      ( Term
          s
          ( PDataRecord
              '[ "parameterD" ':= PInteger
               , "aScale" ':= PInteger
               , "bScale" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PStableswapPoolDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PStableswapPoolDatum where
  type PLifted PStableswapPoolDatum = StableswapPoolDatum

deriving via
  (DerivePConstantViaData StableswapPoolDatum PStableswapPoolDatum)
  instance
    (PConstantDecl StableswapPoolDatum)

instance PTryFrom PData PStableswapPoolDatum
instance PTryFrom PData (PPoolDatum' PStableswapPoolDatum)

instance PUnsafeLiftDecl (PPoolDatum' PStableswapPoolDatum) where
  type PLifted (PPoolDatum' PStableswapPoolDatum) = PoolDatum StableswapPoolDatum

deriving via
  (DerivePConstantViaData (PoolDatum StableswapPoolDatum) (PPoolDatum' PStableswapPoolDatum))
  instance
    (PConstantDecl (PoolDatum StableswapPoolDatum))

instance ScottConvertible PStableswapPoolDatum where
  type ScottOf PStableswapPoolDatum = PStableswapPoolState
  toScott a = f # a
    where
      f = phoistAcyclic $ plam
        \datum ->
          pletFields @'["parameterD", "aScale", "bScale"] datum \d ->
            pcon PStableswapPoolState {parameterD = d.parameterD, aScale = d.aScale, bScale = d.bScale}
  fromScott v = f # v
    where
      f = phoistAcyclic $ plam \datum -> pmatch datum \state ->
        pcon . PStableswapPoolDatum $
          (pdcons # pdata state.parameterD)
            #$ (pdcons # pdata state.aScale)
            #$ (pdcons # pdata state.bScale)
            #$ pdnil

instance Pool 'Stableswap where
  type PPoolSpecificDatum 'Stableswap = PStableswapPoolDatum
  type PPoolSpecificState 'Stableswap = PStableswapPoolState

  ppoolIdentifier = pconstant "1"

  poolType = Stableswap

  pscaling d = pletFields @'["aScale", "bScale"] d \dF -> ppair dF.aScale dF.bScale

  -- For stableswap pools the emergency withdrawals must preserve the pool's datum apart from the new D
  -- which must pass the invariant for the new pool value
  pisDatumCorrectAfterEmergency oldPoolDatum newPoolDatum newPoolState = unTermCont do
    newState <- pmatchC newPoolState
    old <-
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
        (pfromPDatum @(PPoolDatum 'Stableswap) # oldPoolDatum)
    oldParams <- pletFieldsC @'["aScale", "bScale"] old.poolSpecifics
    new <-
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
        (pfromPDatum @(PPoolDatum 'Stableswap) # newPoolDatum)
    newParams <- pletFieldsC @'["aScale", "bScale", "parameterD"] new.poolSpecifics
    pure $
      ptraceIfFalse "eH" $
        pand'List
          [ old.requestValidatorHash #== new.requestValidatorHash
          , old.assetASymbol #== new.assetASymbol
          , old.assetAToken #== new.assetAToken
          , old.assetBSymbol #== new.assetBSymbol
          , old.assetBToken #== new.assetBToken
          , old.swapFeeInBasis #== new.swapFeeInBasis
          , old.protocolFeeInBasis #== new.protocolFeeInBasis
          , old.projectFeeInBasis #== new.projectFeeInBasis
          , old.reserveFeeInBasis #== new.reserveFeeInBasis
          , old.feeBasis #== new.feeBasis
          , old.agentFeeAda #== new.agentFeeAda
          , old.lastInteraction #== new.lastInteraction
          , old.treasuryA #== new.treasuryA
          , old.treasuryB #== new.treasuryB
          , old.projectTreasuryA #== new.projectTreasuryA
          , old.projectTreasuryB #== new.projectTreasuryB
          , old.reserveTreasuryA #== new.reserveTreasuryA
          , old.reserveTreasuryB #== new.reserveTreasuryB
          , old.projectBeneficiary #== new.projectBeneficiary
          , old.reserveBeneficiary #== new.reserveBeneficiary
          , oldParams.aScale #== newParams.aScale
          , oldParams.bScale #== newParams.bScale
          , pcheckDInvariant # pconstant C.protocolStableswapA # newParams.parameterD # (newState.qtyA * newParams.aScale) # (newState.qtyB * newParams.bScale)
          ]

  premoveLiquidity = phoistAcyclic $ plam \poolState newD' redeemedShares -> unTermCont do
    state <- pmatchC poolState
    params <- pmatchC state.poolSpecifics
    let totalEmmittedShares = maxShareTokens - state.qtyShares
        removeA = pdiv # (redeemedShares * state.qtyA) # totalEmmittedShares
        removeB = pdiv # (redeemedShares * state.qtyB) # totalEmmittedShares
        newD = pmatch newD' \case
          PJust n -> n
          PNothing -> params.parameterD
    pure
      ( pcon
          state
            { qtyA = state.qtyA - removeA
            , qtyB = state.qtyB - removeB
            , qtyShares = state.qtyShares + redeemedShares
            , -- NOTE: the logic here doesn't depend on the parameterD,
              --       but the poolState needs to be updated for consecutive requests
              poolSpecifics = pcon @PStableswapPoolState params {parameterD = newD}
            }
      )

  -- \|
  --  Applies swap request, returning new pool state.
  --  This function throws if the request is not applied correctly.
  --  In particular, we check that
  --  1) this isn't a spam order and both lockedAmount and compensations are > 0
  --  2) user got at least minWantedTokens in return
  --  3) user is fairly compensated
  --  4) the invariant holds with the new state and old D parameter
  --  5) the provided newD is correct for changed liquidity (because of added fees)
  papplySwap swapDir minWantedTokens poolState parsedRequest = unTermCont do
    req <- pmatchC parsedRequest
    provided <- pmatchC req.provided
    compensation <- pmatchC req.compensation

    let newD = pasInt # req.additionalData
        rA = provided.pfltA
        rB = provided.pfltB
        cA = compensation.pfltA
        cB = compensation.pfltB

    swapAToB <- pletC $ pmatch swapDir \case
      PSwapAToB _ -> ptrue
      PSwapBToA _ -> pfalse

    PPair lockedTokens receivedTokens <-
      pmatchC (pif swapAToB (ppair rA cB) (ppair rB cA))

    state <- pmatchC poolState
    params <- pmatchC state.poolSpecifics
    PTriplet newPoolState feeCollectedA feeCollectetB <-
      pmatchC (pstableswapSwapAssets # poolState # swapAToB # lockedTokens # receivedTokens # newD)
    newState <- pmatchC newPoolState

    let lockedNonZero =
          ptraceIfFalse "l>0" (lockedTokens #> 0)
        outNonZero =
          ptraceIfFalse "r>0" (receivedTokens #> 0)
        minWantedReceived =
          ptraceIfFalse "mwt" (receivedTokens #>= minWantedTokens)
        -- We scale the assets A and B by the numbers stored as scale factors in the datum.
        -- That allows us to support stable coins with different decimal points.
        -- Note that we do it only for the stableswap invariant checks, it doesn't affect the fees nor the compensation
        stableswapChecked =
          pand'List
            [ ptraceIfFalse "sodi" $
                pcheckStableswap
                  # swapAToB
                  # pconstant C.protocolStableswapA
                  # params.parameterD
                  # ((newState.qtyA - feeCollectedA) * params.aScale)
                  # ((newState.qtyB - feeCollectetB) * params.bScale)
                  # params.aScale
                  # params.bScale
            , ptraceIfFalse "sndi" $
                pcheckDInvariant
                  # pconstant C.protocolStableswapA
                  # newD
                  # (newState.qtyA * params.aScale)
                  # (newState.qtyB * params.bScale)
            ]
    pguardC
      "stableswap application"
      ( pand'List
          [ lockedNonZero
          , outNonZero
          , minWantedReceived
          , stableswapChecked
          ]
      )
    pure newPoolState

  -- \|
  --  Applies addLiquidity request, returning new pool state.
  --  This function throws if the request is not applied correctly.
  --  In particular, we check that
  --  1) this isn't a spam order and at least one of addA, addB is > 0 and compensations are > 0
  --  2) user got at least minWantedShares > 0 in return
  --  3) user is fairly compensated
  --  4) the provided newD is correct for the final state with the changed liquidity
  --  5) the provided newDFullyAdded is correct for the fully added liquidity
  --  6) the provided newDFeesTaken is correct for the fully added liquidity but with fees subtracted
  papplyAddLiquidity minWantedShares poolState parsedRequest = unTermCont do
    req <- pmatchC parsedRequest
    provided <- pmatchC req.provided
    compensation <- pmatchC req.compensation
    newDTriple <- pletC $ pasList # req.additionalData
    -- new d fully added represents the D value after the addition of liquidity before the fees are taken
    -- new d fees taken represents the D value after the addition of liquidity and the fees are taken
    -- new d represents the final D value after the addition of liquidity, the fees are taken
    -- and the swap fee is added back
    PTriplet newDFullyAdded newDFeesTaken newD <-
      pmatchC
        ( pelimList
            ( \d1 d23 ->
                pelimList
                  ( \d2 d3r ->
                      pelimList
                        ( \d3 _ ->
                            ptriplet (pasInt # d1) (pasInt # d2) (pasInt # d3)
                        )
                        perror
                        d3r
                  )
                  perror
                  d23
            )
            perror
            newDTriple
        )

    let addA = provided.pfltA
        addB = provided.pfltB
        earnedShares = compensation.pfltShares

    state <- pmatchC poolState
    params <- pmatchC state.poolSpecifics

    -- if state.qtyA * (state.qtyB + addB) <= (state.qtyA + addA) * state.qtyB that means assetA is swapped,
    -- note that fees are only taken from the swapped asset
    swapAToB <- pletC (state.qtyA * (state.qtyB + addB) #<= (state.qtyA + addA) * state.qtyB)

    let balancedA = pdiv # (newDFullyAdded * state.qtyA) # params.parameterD
        balancedB = pdiv # (newDFullyAdded * state.qtyB) # params.parameterD
    newAFull <- pletC $ state.qtyA + addA
    newBFull <- pletC $ state.qtyB + addB
    PPair feesA feesB <-
      pmatchC $
        pif
          swapAToB
          ( ppair
              ( plet (pabs # (newAFull - balancedA)) \deltaA ->
                  pquartet
                    (pdivideCeil # (deltaA * state.swapFeeInBasis) # state.feeBasis)
                    (pdiv # (deltaA * state.protocolFeeInBasis) # state.feeBasis)
                    (pdiv # (deltaA * state.projectFeeInBasis) # state.feeBasis)
                    (pdiv # (deltaA * state.reserveFeeInBasis) # state.feeBasis)
              )
              (pquartet 0 0 0 0)
          )
          ( ppair
              (pquartet 0 0 0 0)
              ( plet (pabs # (newBFull - balancedB)) \deltaB ->
                  pquartet
                    (pdivideCeil # (deltaB * state.swapFeeInBasis) # state.feeBasis)
                    (pdiv # (deltaB * state.protocolFeeInBasis) # state.feeBasis)
                    (pdiv # (deltaB * state.projectFeeInBasis) # state.feeBasis)
                    (pdiv # (deltaB * state.reserveFeeInBasis) # state.feeBasis)
              )
          )
    PQuartet swapFeeA protocolFeeA projectFeeA reserveFeeA <- pmatchC feesA
    PQuartet swapFeeB protocolFeeB projectFeeB reserveFeeB <- pmatchC feesB
    newAFeesTaken <- pletC $ newAFull - (protocolFeeA + projectFeeA + reserveFeeA + swapFeeA)
    newBFeesTaken <- pletC $ newBFull - (protocolFeeB + projectFeeB + reserveFeeB + swapFeeB)
    newA <- pletC $ newAFeesTaken + swapFeeA
    newB <- pletC $ newBFeesTaken + swapFeeB

    let totalEmittedShares = C.maxShareTokens - state.qtyShares
        deltaD = newDFeesTaken - params.parameterD
        expectedShares = pdiv # (totalEmittedShares * deltaD) # params.parameterD
        sharesRight = earnedShares #== expectedShares

        invariantFull =
          pcheckDInvariant # pconstant C.protocolStableswapA # newDFullyAdded # (newAFull * params.aScale) # (newBFull * params.bScale)
        invariantFeesTaken =
          pcheckDInvariant # pconstant C.protocolStableswapA # newDFeesTaken # (newAFeesTaken * params.aScale) # (newBFeesTaken * params.bScale)
        invariantNew =
          pcheckDInvariant # pconstant C.protocolStableswapA # newD # (newA * params.aScale) # (newB * params.bScale)

        newState =
          state
            { qtyA = newA
            , qtyB = newB
            , qtyShares = state.qtyShares - earnedShares
            , qtyTreasuryA = state.qtyTreasuryA + protocolFeeA
            , qtyTreasuryB = state.qtyTreasuryB + protocolFeeB
            , qtyProjectTreasuryA = state.qtyProjectTreasuryA + projectFeeA
            , qtyProjectTreasuryB = state.qtyProjectTreasuryB + projectFeeB
            , qtyReserveTreasuryA = state.qtyReserveTreasuryA + reserveFeeA
            , qtyReserveTreasuryB = state.qtyReserveTreasuryB + reserveFeeB
            , poolSpecifics = pcon @PStableswapPoolState params {parameterD = newD}
            }

        notSpam = pand'List [addA #> 0 #|| addB #> 0, earnedShares #> 0]
        sharesPositive = newState.qtyShares #>= 0
        earnedEnough = earnedShares #>= minWantedShares
    pure
      ( pif
          ( pand'List
              [ ptraceIfFalse "s>0" $ sharesPositive
              , ptraceIfFalse "spam" $ notSpam
              , ptraceIfFalse "sn" $ earnedEnough
              , ptraceIfFalse "sr" $ sharesRight
              , ptraceIfFalse "dfl" $ invariantFull
              , ptraceIfFalse "dn" $ invariantFeesTaken
              , ptraceIfFalse "df" $ invariantNew
              ]
          )
          (pcon newState)
          perror
      )

  -- \|
  --  Given pool state, number of returned pool shares, and the new expected D,
  --  calculate the new state of the pool as well as number of returned assets to the user
  --
  --  Note: This function assumes valid pool state as well as reasonable redemption.
  --  In particular, we assume that we are redeeming less shares than pool issued.
  --  This should hold true as long as we initially burn some pool shares.
  papplyWithdrawLiquidity minWantedA minWantedB poolState parsedRequest =
    unTermCont do
      req <- pmatchC parsedRequest
      state <- pmatchC poolState
      params <- pmatchC state.poolSpecifics
      PFlatAssets _ _ redeemedShares <- pmatchC req.provided
      PFlatAssets cA cB _ <- pmatchC req.compensation
      PQuartet newPoolState removeA removeB newD <-
        pmatchC $
          pcond
            [
              ( minWantedA #== 0
              , -- The additional data is an [after remove D, new D] list for zap outs
                unTermCont do
                  PPair afterRemoveD newD <-
                    pmatchC
                      ( pelimList
                          (\d1 d2r -> pelimList (\d2 _ -> ppair (pasInt # d1) (pasInt # d2)) perror d2r)
                          perror
                          (pasList # req.additionalData)
                      )
                  newPoolState <- pletC (pstableswapZapOut SwapAToB poolState afterRemoveD newD redeemedShares cB)
                  newState <- pmatchC newPoolState
                  pure $ pquartet newPoolState 0 (state.qtyB - newState.qtyB) newD
              )
            ,
              ( minWantedB #== 0
              , -- The additional data is an [after remove D, new D] list for zap outs
                unTermCont do
                  PPair afterRemoveD newD <-
                    pmatchC
                      ( pelimList
                          (\d1 d2r -> pelimList (\d2 _ -> ppair (pasInt # d1) (pasInt # d2)) perror d2r)
                          perror
                          (pasList # req.additionalData)
                      )
                  newPoolState <- pletC (pstableswapZapOut SwapBToA poolState afterRemoveD newD redeemedShares cA)
                  newState <- pmatchC newPoolState
                  pure $ pquartet newPoolState (state.qtyA - newState.qtyA) 0 newD
              )
            ]
            ( -- The additional data is the new D for normal liquidity withdrawals
              unTermCont do
                newD <- pletC (pasInt # req.additionalData)
                newPoolState <- pletC (premoveLiquidity # poolState # pjust newD # redeemedShares)
                newState <- pmatchC newPoolState
                pure $ pquartet newPoolState (state.qtyA - newState.qtyA) (state.qtyB - newState.qtyB) newD
            )

      newState <- pmatchC newPoolState

      let userFairlyCompensated =
            pand'List
              [ ptraceIfFalse "ra" $ cA #>= removeA
              , ptraceIfFalse "mwa" $ cA #>= minWantedA
              , ptraceIfFalse "rb" $ cB #>= removeB
              , ptraceIfFalse "mwb" $ cB #>= minWantedB
              ]
          -- NOTE: Remove liquidity does not depend on parameterD,
          -- The check here ensures a consistent state for consecutive requests
          -- and that the zap-out's swap is correct.
          --
          -- We scale the assets A and B by the numbers stored as scale factors in the datum.
          -- That allows us to support stable coins with different decimal points.
          -- Note that we do it only for the stableswap invariant checks only.
          isInvariantSatisfied =
            ptraceIfFalse "dic_w" $
              pcheckDInvariant
                # pconstant C.protocolStableswapA
                # newD
                # (newState.qtyA * params.aScale)
                # (newState.qtyB * params.bScale)

      pguardC "s wl" $
        pand'List
          [ redeemedShares #> 0
          , userFairlyCompensated
          , isInvariantSatisfied
          ]
      pure newPoolState

  -- \|
  -- Applies adding staking rewards into the pool. This application checks
  -- that the balances that have an impact on the pool match and that the new D is correct for those.
  -- The action authorization needs to happen before being able to apply to this during the parsing.
  -- Assumptions:
  --  1. The add staking rewards are authorized by a reward token
  --  2. The reward token was returned to the staking agent
  --  3. The pool is an ADA <> Token pool, so token A in the pool is ADA
  --
  -- The application should ensure:
  --  1. All ADA from the action were added into the pool
  --  2. The staking agent doesn't receive any additional ada back (only oilAda, which is deducted during parsing)
  --  3. The passed newD is correct for the changed liquidity (as token A is added)
  papplyAddStakingRewards poolState parsedRequest =
    unTermCont $ do
      req <- pmatchC parsedRequest
      rAssets <- pmatchC req.provided
      cAssets <- pmatchC req.compensation
      state <- pmatchC poolState
      params <- pmatchC state.poolSpecifics
      -- The additional data is the new D
      let newD = pasInt # req.additionalData
          allStakingRewardsAdded = cAssets.pfltA #== 0
          rewardsQuantity = rAssets.pfltA
          newPoolState =
            pcon
              state
                { qtyA = rewardsQuantity + state.qtyA
                , poolSpecifics = pcon @PStableswapPoolState params {parameterD = newD}
                }
          checkedNewD =
            pcheckDInvariant
              # pconstant C.protocolStableswapA
              # newD
              -- new A = old A + added rewards
              # ((rewardsQuantity + state.qtyA) * params.aScale)
              -- new B = old B
              # (state.qtyB * params.bScale)
      pure $ pif (pand' # allStakingRewardsAdded # checkedNewD) newPoolState (ptraceError "pL")

pinitialStableswapPoolCorrect ::
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PDatum
        :--> PScriptHash
        :--> PTimestamps
        :--> PInteger
        :--> PInteger
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PBool
    )
pinitialStableswapPoolCorrect =
  plam $
    \aScale
     bScale
     outputDatum
     requestValidatorHash
     timestamps
     outA
     outB
     assetASymbol
     assetAToken
     assetBSymbol
     assetBToken
     poolOutShares -> unTermCont $ do
        poolDatum <-
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
              , "agentFeeAda"
              , "swapFeeInBasis"
              , "protocolFeeInBasis"
              , "projectFeeInBasis"
              , "reserveFeeInBasis"
              , "reserveBeneficiary"
              , "feeBasis"
              , "poolSpecifics"
              ]
            (pfromPDatum @(PPoolDatum 'Stableswap) # outputDatum)
        params <- pletFieldsC @'["parameterD", "aScale", "bScale"] poolDatum.poolSpecifics
        validityTimestamps <- pmatchC timestamps
        let totalEmmittedShares = maxShareTokens - poolOutShares
            -- Refer to the doc/stableswap.md documentation for more details.
            -- 2 * sqrt(x * y) <= D <= x + y; x, y < overflow limit
            --
            -- Worst case: x = y = overflow limit
            -- D = aScale * x + bScale * y
            -- D = overflow limit * (aScale + bScale)
            -- D / (aScale + bScale) = overflow limit
            --
            -- 2 * aScale * bScale >= aScale + bScale, aScale >= 1, bScale >= 1
            -- 1 / (2 * aScale * bScale) <= 1 / (aScale + bScale) | D
            -- D / (2 * aScale * bScale) <= D / (aScale + bScale)
            -- D / (2 * aScale * bScale) <= overflow limit
            --
            -- So initial emitted shares = D / (2 * aScale * bScale)
            --
            -- With this emitted shares calculation we expect
            -- earnedShares = (newD - oldD) / (2 * aScale * bScale) for addLiquidity
            --
            -- Alternatively we can set up the emitted shares as D / (aScale + bScale)
            -- That will change the offchain formula to
            -- earnedShares = (newD - oldD) / (aScale + bScale) for addLiquidity
            expectedEmittedShares = pdiv # initialD # (2 * aScale * bScale)
            initialD = params.parameterD
        pure $
          pand'List
            [ ptraceIfFalse "s d" $
                pand'List
                  [ poolDatum.requestValidatorHash #== requestValidatorHash
                  , poolDatum.assetASymbol #== assetASymbol
                  , poolDatum.assetAToken #== assetAToken
                  , poolDatum.assetBSymbol #== assetBSymbol
                  , poolDatum.assetBToken #== assetBToken
                  , pfromData poolDatum.agentFeeAda #== C.agentFeeAda
                  , pfromData poolDatum.swapFeeInBasis #== C.stableswapSwapFeeInBasis
                  , pfromData poolDatum.feeBasis #== C.stableswapFeeBasis
                  , pfromData poolDatum.protocolFeeInBasis #== C.stableswapProtocolFeeInBasis
                  , pfromData poolDatum.projectFeeInBasis #== C.projectFeeInBasis
                  , pfromData poolDatum.reserveFeeInBasis #== C.reserveFeeInBasis
                  , pfromData poolDatum.treasuryA #== 0
                  , pfromData poolDatum.treasuryB #== 0
                  , pfromData poolDatum.projectTreasuryA #== 0
                  , pfromData poolDatum.projectTreasuryB #== 0
                  , pfromData poolDatum.reserveTreasuryA #== 0
                  , pfromData poolDatum.reserveTreasuryB #== 0
                  , pfromData poolDatum.projectBeneficiary #== pdnothing
                  , pfromData poolDatum.reserveBeneficiary #== pdnothing
                  , pfromData poolDatum.lastInteraction #== validityTimestamps.lowerBound
                  , params.aScale #== aScale
                  , params.bScale #== bScale
                  ]
            , ptraceIfFalse "s em" $ totalEmmittedShares #== expectedEmittedShares
            , ptraceIfFalse "stableswap d invariant" $
                ptraceIfFalse "D" (pcheckDInvariant # protocolStableswapA # initialD # (outA * aScale) # (outB * bScale))
                  #|| ptraceIfFalse "D + 1" (pcheckDInvariant # protocolStableswapA # (initialD + 1) # (outA * aScale) # (outB * bScale))
            , ptraceIfFalse "asc>0" $
                aScale #> 0
            , ptraceIfFalse "bsc>0" $
                bScale #> 0
            , ptraceIfFalse "is 1" $
                aScale #== 1 #|| bScale #== 1
            ]

data PStableswapNewReserves (s :: S) = PStableswapNewReserves
  { newX :: Term s PInteger
  , newY :: Term s PInteger
  , swapFeeX :: Term s PInteger
  , protocolFeeX :: Term s PInteger
  , projectFeeX :: Term s PInteger
  , reserveFeeX :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PStableswapNewReserves where
  type DPTStrat _ = PlutusTypeScott

pstableswapZapOut ::
  SwapDirection ->
  Term s (PPoolState 'Stableswap) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PPoolState 'Stableswap)
pstableswapZapOut SwapAToB initialPoolState afterRemoveD newD redeemedShares compensationB = unTermCont do
  initialState <- pmatchC initialPoolState
  params <- pmatchC initialState.poolSpecifics
  afterRemove <- pletC (premoveLiquidity # initialPoolState # pjust afterRemoveD # redeemedShares)
  afterRemoveState <- pmatchC afterRemove
  let lockedTokens = initialState.qtyA - afterRemoveState.qtyA
      removedB = initialState.qtyB - afterRemoveState.qtyB
      receivedTokens = compensationB - removedB
  PTriplet newPoolState feeCollectedA feeCollectedB <-
    pmatchC (pstableswapSwapAssets # afterRemove # ptrue # lockedTokens # receivedTokens # newD)
  newState <- pmatchC newPoolState
  -- we don't need to check the new D invariant
  -- as it's done in the papplyWithdrawLiquidity call
  pguardC "zap out" $
    pand'List
      [ ptraceIfFalse "rdi ab" $
          pcheckDInvariant
            # pconstant C.protocolStableswapA
            # afterRemoveD
            # (afterRemoveState.qtyA * params.aScale)
            # (afterRemoveState.qtyB * params.bScale)
      , ptraceIfFalse "rdsi ab" $
          pcheckStableswap
            # ptrue
            # pconstant C.protocolStableswapA
            # afterRemoveD
            # ((newState.qtyA - feeCollectedA) * params.aScale)
            # ((newState.qtyB - feeCollectedB) * params.bScale)
            # params.aScale
            # params.bScale
      ]
  pure newPoolState
pstableswapZapOut SwapBToA initialPoolState afterRemoveD newD redeemedShares compensationA = unTermCont do
  initialState <- pmatchC initialPoolState
  params <- pmatchC initialState.poolSpecifics
  afterRemove <- pletC (premoveLiquidity # initialPoolState # pjust afterRemoveD # redeemedShares)
  afterRemoveState <- pmatchC afterRemove
  let lockedTokens = initialState.qtyB - afterRemoveState.qtyB
      removedA = initialState.qtyA - afterRemoveState.qtyA
      receivedTokens = compensationA - removedA
  PTriplet newPoolState feeCollectedA feeCollectedB <-
    pmatchC (pstableswapSwapAssets # afterRemove # pfalse # lockedTokens # receivedTokens # newD)
  newState <- pmatchC newPoolState
  -- we don't need to check the new D invariant
  -- as it's done in the papplyWithdrawLiquidity call
  pguardC "zap out" $
    pand'List
      [ ptraceIfFalse "rdi ba" $
          pcheckDInvariant
            # pconstant C.protocolStableswapA
            # afterRemoveD
            # (afterRemoveState.qtyA * params.aScale)
            # (afterRemoveState.qtyB * params.bScale)
      , ptraceIfFalse "rdsi ba" $
          pcheckStableswap
            # pfalse
            # pconstant C.protocolStableswapA
            # afterRemoveD
            # ((newState.qtyA - feeCollectedA) * params.aScale)
            # ((newState.qtyB - feeCollectedB) * params.bScale)
            # params.aScale
            # params.bScale
      ]
  pure newPoolState

{- |
 This takes the previous amounts of pool reserves and returns the new amounts.
 The oldX argument needs to be the pool reserve amount in the locked asset and the oldY one is the wanted asset pool reserve.
 The lockX argument is the locked amount of the first asset.
 The outY argument is the compensation value.
 Note that (lockX - protocolFeeX - projectFeeX - reserveFeeX) tokens are put back into the pool,
 but (lockX - swapFeeX - protocolFeeX - projectFeeX - reserveFeeX) tokens are used to derive the number of tokens that should be given to the user.
 All fees are separate from each other.
 The fee basis and all fees in basis are passed in as arguments.
 For example:
   Given fee basis = 10_000, swap fee in basis = 5 and protocol fee in basis = 1 (with the rest being 0).
   If a user wants to swap 10 ada, which is 10_000_000 Lovelace.
   The swap fee is 5_000 lovelace. The protocol fee is 1_000 lovelace. (10_000_000 - 5_000 - 1_000 - 0 - 0) is used to determine the exchanged token amount.
   The protocol, reserve and project fees are put into their treasuries,
   and the rest of the locked amount (the swap fee) is put into the pool = (10_000_000 - 1_000 - 0 - 0)
-}
pstableswapNewReserves :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PStableswapNewReserves)
pstableswapNewReserves = phoistAcyclic $ plam \swapFeeInBasis protocolFeeInBasis projectFeeInBasis reserveFeeInBasis feeBasis oldX lockX oldY outY -> unTermCont do
  let swapFeeX = pdivideCeil # (lockX * swapFeeInBasis) # feeBasis
  protocolFeeX <- pletC $ pdiv # (lockX * protocolFeeInBasis) # feeBasis
  projectFeeX <- pletC $ pdiv # (lockX * projectFeeInBasis) # feeBasis
  reserveFeeX <- pletC $ pdiv # (lockX * reserveFeeInBasis) # feeBasis
  let newX = oldX + lockX - protocolFeeX - projectFeeX - reserveFeeX
      newY = oldY - outY
  (pure . pcon) PStableswapNewReserves {newX, protocolFeeX, newY, swapFeeX, projectFeeX, reserveFeeX}

{- | Given the state of the pool, the swap direction, the offered quantity, tha expected compensation, and the value of D,
calculate the new pool state and the expected number of fees.

Note: this function assumes a valid pool state and can never fail.
-}
pstableswapSwapAssets ::
  Term
    s
    ( PPoolState 'Stableswap
        :--> PBool
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PTriplet (PPoolState 'Stableswap) PInteger PInteger
    )
pstableswapSwapAssets = phoistAcyclic $ plam \poolState swapAToB givenTokens receivedTokens newD ->
  pmatch poolState \state -> pmatch state.poolSpecifics \params ->
    pif
      swapAToB
      ( unTermCont do
          reserves <-
            pmatchC $
              pstableswapNewReserves # state.swapFeeInBasis # state.protocolFeeInBasis # state.projectFeeInBasis # state.reserveFeeInBasis # state.feeBasis # state.qtyA # givenTokens # state.qtyB # receivedTokens
          newState <-
            pletC . pcon $
              state
                { qtyA = reserves.newX
                , qtyB = reserves.newY
                , qtyTreasuryA = reserves.protocolFeeX + state.qtyTreasuryA
                , qtyProjectTreasuryA = reserves.projectFeeX + state.qtyProjectTreasuryA
                , qtyReserveTreasuryA = reserves.reserveFeeX + state.qtyReserveTreasuryA
                , poolSpecifics = pcon @PStableswapPoolState params {parameterD = newD}
                }
          pure $ ptriplet newState reserves.swapFeeX 0
      )
      ( unTermCont do
          reserves <-
            pmatchC $
              pstableswapNewReserves # state.swapFeeInBasis # state.protocolFeeInBasis # state.projectFeeInBasis # state.reserveFeeInBasis # state.feeBasis # state.qtyB # givenTokens # state.qtyA # receivedTokens
          newState <-
            pletC . pcon $
              state
                { qtyA = reserves.newY
                , qtyB = reserves.newX
                , qtyTreasuryB = reserves.protocolFeeX + state.qtyTreasuryB
                , qtyProjectTreasuryB = reserves.projectFeeX + state.qtyProjectTreasuryB
                , qtyReserveTreasuryB = reserves.reserveFeeX + state.qtyReserveTreasuryB
                , poolSpecifics = pcon @PStableswapPoolState params {parameterD = newD}
                }
          pure $ ptriplet newState 0 reserves.swapFeeX
      )

-- All checks are based on Stableswap invariant: 4xy (4A(x + y) + D) = 16ADxy + D^3
--    - x, y represent amounts of pool reserves
--    - D represents liquidity in the pool if the pool were balanced.
--
-- Note: when two of the values are > 0, the third value needs to be > 0 as well.
--       The checks return false if the validated value is negative.
-- It works because for two positive values, there is exactly one positive root:
--    - in D it is cubic equation with one positive and two imaginary roots
--    - in x (y) it is quadratic equation with one positive and one negative root,
--  and we adjust the check to pass only for the positive root.

{- | Perform a check of the stableswap invariant with fixed x and D.
Treats y as a variable.
Warning: assumes x and D are > 0. Then it passes only for y > 0

The equation is:
f(y) = 4xy (4A(x + y) + D) - (16ADxy + D^3) = 0
f(y) = left(y) - right(y)
Unfortunately, we can't enforce exact equality because we operate with integers.
We want the upper approximation, so the passed y must satisfy:
f(y - 1) < 0 and f(y) >= 0

As we also support scaling one of the tokens to support decimals, the step in y can be different from 1.
So instead we check f(y - 1 * scaleY) < 0 and f(y) >= 0.
Otherwise we can have a situation when there's no integer solution in x.

Note: Exact positive root is > 0. Therefore upper approximation y is >= 1
      and it's not possible to take out the whole amount of one pool reserve.

We need direction here as well to avoid some rounding errors
given:     a = 4, x = 1000000, D = 174910
computed:  y = 100
but y = 100 works as well for
x = 999995, D = 174910
Providing the direction helps us avoid this issue
-}
pcheckStableswap :: Term s (PBool :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
pcheckStableswap = phoistAcyclic $ plam \swapAToB a d numberOfA numberOfB scaleA scaleB -> unTermCont do
  PTriplet x y scaleY <-
    pmatchC
      ( pif
          swapAToB
          (ptriplet numberOfA numberOfB scaleB)
          (ptriplet numberOfB numberOfA scaleA)
      )
  xy <- pletC (x * y)
  xy1 <- pletC (x * (y - scaleY))
  xpy <- pletC (x + y)
  let l0 = pleftEquation # a # d # xy # xpy
      l1 = pleftEquation # a # d # xy1 # (xpy - scaleY)
      r0 = prightEquation # a # d # xy
      r1 = prightEquation # a # d # xy1
      fy1 = l1 - r1
      fy = l0 - r0
  pure $ pand'List [ptraceIfFalse "fy1 >= 0" $ fy1 #< 0, ptraceIfFalse "fy < 0" $ fy #>= 0]

{- | Perform a check of the stableswap invariant with fixed x and y.
Treats D as a variable.
Warning: assumes x and y are whole and > 0 (precisely >= 1). Then it will pass only for d > 0 (precisely >= 2)

The equation is:
f(D) = 16ADxy + D^3 - 4xy(4A(x + y) + D) = 0
f(D) = right(D) - left(D)
Unfortunately, we can't enforce exact equality because we operate with integers.
We want the lower approximation, so the passed D must satisfy:
f(D) <= 0 and f(D + 1) > 0
-}
pcheckDInvariant :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
pcheckDInvariant = phoistAcyclic $ plam \a d x y -> unTermCont do
  xy <- pletC (x * y)
  xpy <- pletC (x + y)
  d1 <- pletC (d + 1)
  let l0 = pleftEquation # a # d # xy # xpy
      l1 = pleftEquation # a # d1 # xy # xpy
      r0 = prightEquation # a # d # xy
      r1 = prightEquation # a # d1 # xy
      fd = r0 - l0
      fd1 = r1 - l1
  pure $
    pand'List
      [ ptraceIfFalse "fd > 0" $ fd #<= 0
      , ptraceIfFalse "fd1 <= 0" $ fd1 #> 0
      ]

-- | 4xy (4A(x + y) + D)
pleftEquation :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger)
pleftEquation = phoistAcyclic $ plam \a d xy xpy -> 4 * xy * (a * 4 * xpy + d)

-- | 16ADxy + D^3
prightEquation :: Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
prightEquation = phoistAcyclic $ plam \a d xy -> a * 16 * d * xy + d * d * d
