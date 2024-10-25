{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DEX.Pool.ConstantProduct where

import DEX.Constants (maxShareTokens)
import DEX.Constants qualified as C
import DEX.Types.Classes
import DEX.Types.Pool
import DEX.Types.Request
import Plutarch.Api.V2 (PCurrencySymbol, PDatum (..), PScriptHash, PTokenName)
import Plutarch.Builtin (pasInt, pforgetData)
import Plutarch.DataRepr
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Types.Classes
import Plutarch.Util
import PlutusTx qualified

data ConstantProductPoolDatum = ConstantProductPoolDatum
  deriving (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''ConstantProductPoolDatum [('ConstantProductPoolDatum, 0)]
PlutusTx.makeLift ''ConstantProductPoolDatum

data PConstantProductPoolDatum (s :: S)
  = PConstantProductPoolDatum (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

pconstantProductPoolDatum :: ClosedTerm PConstantProductPoolDatum
pconstantProductPoolDatum = pcon (PConstantProductPoolDatum pdnil)

instance DerivePlutusType PConstantProductPoolDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PConstantProductPoolDatum where
  type PLifted PConstantProductPoolDatum = ConstantProductPoolDatum

deriving via
  (DerivePConstantViaData ConstantProductPoolDatum PConstantProductPoolDatum)
  instance
    (PConstantDecl ConstantProductPoolDatum)

instance PTryFrom PData PConstantProductPoolDatum

instance PUnsafeLiftDecl (PPoolDatum' PConstantProductPoolDatum) where
  type PLifted (PPoolDatum' PConstantProductPoolDatum) = (PoolDatum ConstantProductPoolDatum)

deriving via
  (DerivePConstantViaData (PoolDatum ConstantProductPoolDatum) (PPoolDatum' PConstantProductPoolDatum))
  instance
    (PConstantDecl (PoolDatum ConstantProductPoolDatum))

instance PTryFrom PData (PPoolDatum' PConstantProductPoolDatum)

data PConstantProductPoolScott (s :: S) = PConstantProductPoolScott
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PEq)

instance DerivePlutusType PConstantProductPoolScott where
  type DPTStrat _ = PlutusTypeScott

instance ScottConvertible PConstantProductPoolDatum where
  type ScottOf PConstantProductPoolDatum = PConstantProductPoolScott
  toScott _ = pcon PConstantProductPoolScott
  fromScott _ = pconstantProductPoolDatum

instance Pool 'ConstantProduct where
  type PPoolSpecificDatum 'ConstantProduct = PConstantProductPoolDatum
  type PPoolSpecificState 'ConstantProduct = PConstantProductPoolScott

  ppoolIdentifier = pconstant "0"

  poolType = ConstantProduct

  pscaling _ = ppair 1 1

  -- For constant product pools the emergency withdrawals must preserve the pool's datum
  pisDatumCorrectAfterEmergency oldPoolDatum newPoolDatum _ = ptraceIfFalse "eH" (oldPoolDatum #== newPoolDatum)

  premoveLiquidity = phoistAcyclic $ plam $ \poolState _ redeemedShares ->
    unTermCont $ do
      state <- pmatchC poolState
      totalEmittedShares <- pletC $ maxShareTokens - state.qtyShares
      let removeA = pdiv # (redeemedShares * state.qtyA) # totalEmittedShares
          removeB = pdiv # (redeemedShares * state.qtyB) # totalEmittedShares
      pure $
        pcon
          state
            { qtyA = state.qtyA - removeA
            , qtyB = state.qtyB - removeB
            , qtyShares = state.qtyShares + redeemedShares
            }

  -- \|
  --  Applies swap request, returning new pool state.
  --  This function throws is request is not applied correctly.
  --  In particular, we check that
  --  1) this isn't spam order and both lockedAmount and compensations is > 0
  --  2) user is fairly compensated
  papplySwap swapDir minWantedTokens poolState parsedRequest = unTermCont do
    req <- pmatchC parsedRequest
    PFlatAssets rA rB _ <- pmatchC req.provided
    PFlatAssets cA cB _ <- pmatchC req.compensation
    swapAToB <- pletC $ pmatch swapDir \case
      PSwapAToB _ -> ptrue
      PSwapBToA _ -> pfalse
    lockedTokens <- pletC (pif swapAToB rA rB)
    swapResult <- pmatchC (pswapAssets # poolState # swapAToB # lockedTokens)
    let isUserFairlyCompensated =
          pif
            swapAToB
            ( pand'List
                [ ptraceIfFalse "otb" $ cB #>= swapResult.outTokens
                , ptraceIfFalse "mwtb" $ cB #>= minWantedTokens
                ]
            )
            ( pand'List
                [ ptraceIfFalse "ota" $ cA #>= swapResult.outTokens
                , ptraceIfFalse "mwta" $ cA #>= minWantedTokens
                ]
            )
    pure
      ( pif
          ( pand'List
              [ ptraceIfFalse "sl>0" $ lockedTokens #> 0
              , ptraceIfFalse "so>0" $ swapResult.outTokens #> 0
              , ptraceIfFalse "sufc" isUserFairlyCompensated
              ]
          )
          swapResult.newState
          perror
      )

  papplyAddLiquidity minWantedShares poolState parsedRequest = unTermCont do
    req <- pmatchC parsedRequest
    rAssets <- pmatchC req.provided
    cAssets <- pmatchC req.compensation
    addData <- pletC $ pasInt # req.additionalData

    result <-
      pmatchC $
        pcond
          [ (rAssets.pfltA #== 0, paddLiquidityZapIn SwapBToA poolState rAssets.pfltB addData)
          , (rAssets.pfltB #== 0, paddLiquidityZapIn SwapAToB poolState rAssets.pfltA addData)
          ]
          (paddLiquidity # poolState # rAssets.pfltA # rAssets.pfltB)
    newState <- pmatchC result.newState

    pure
      ( pif
          ( pand'List
              [ ptraceIfFalse "a pl" $ newState.qtyShares #>= 0
              , ptraceIfFalse "a es" $ result.earnedShares #> 0
              , ptraceIfFalse "a omws" $ cAssets.pfltShares #>= minWantedShares
              , ptraceIfFalse "a oes" $ cAssets.pfltShares #>= result.earnedShares
              ]
          )
          result.newState
          perror
      )

  papplyWithdrawLiquidity minWantedA minWantedB poolState parsedRequest =
    unTermCont do
      state <- pmatchC poolState
      req <- pmatchC parsedRequest
      parsedRequestAssets <- pmatchC req.provided
      parsedCompensationAssets <- pmatchC req.compensation

      compensationFltA <- pletC (pfltA parsedCompensationAssets)
      compensationFltB <- pletC (pfltB parsedCompensationAssets)
      redeemedShares <- pletC (pfltShares parsedRequestAssets)

      PTriplet newPoolState removeA removeB <-
        pmatchC $
          pcond
            [
              ( minWantedA #== 0
              , plet (premoveLiquidityZapOut SwapAToB poolState redeemedShares) \newS ->
                  pmatch newS \newState -> ptriplet newS 0 (state.qtyB - newState.qtyB)
              )
            ,
              ( minWantedB #== 0
              , plet (premoveLiquidityZapOut SwapBToA poolState redeemedShares) \newS ->
                  pmatch newS \newState -> ptriplet newS (state.qtyA - newState.qtyA) 0
              )
            ]
            ( plet (premoveLiquidity # poolState # pnothing # redeemedShares) \newS ->
                pmatch newS \newState -> ptriplet newS (state.qtyA - newState.qtyA) (state.qtyB - newState.qtyB)
            )

      let isUserFairlyCompensated =
            pand'List
              [ ptraceIfFalse "removeA" $ compensationFltA #>= removeA
              , ptraceIfFalse "minWantedA" $ compensationFltA #>= minWantedA
              , ptraceIfFalse "removeB" $ compensationFltB #>= removeB
              , ptraceIfFalse "minWantedB" $ compensationFltB #>= minWantedB
              ]
          requestCorrect = pand'List [ptraceIfFalse "redeemedShares" $ redeemedShares #> 0, isUserFairlyCompensated]
      pure (pif requestCorrect newPoolState (ptraceError "pR"))

  -- \|
  -- Applies adding staking rewards into the pool. This application only checks
  -- that the balances that have an impact on the pool match
  -- The action authorization needs to happen before being able to apply to this during the parsing.
  -- Assumptions:
  --  1. The add staking rewards are authorized by a reward token
  --  2. The reward token was returned to the staking agent
  --  3. The pool is an ADA <> Token pool, so token A in the pool is ADA
  --
  -- The application should ensure:
  --  1. All ADA from the action were added into the pool
  --  2. The staking agent doesn't receive any additional ada back (only oilAda, which is deducted during parsing)
  papplyAddStakingRewards poolState parsedRequest =
    unTermCont $ do
      req <- pmatchC parsedRequest
      rAssets <- pmatchC req.provided
      cAssets <- pmatchC req.compensation
      state <- pmatchC poolState
      let allStakingRewardsAdded = cAssets.pfltA #== 0
          rewardsQuantity = rAssets.pfltA
          newPoolState = pcon state {qtyA = rewardsQuantity + state.qtyA}
      pure $ pif allStakingRewardsAdded newPoolState (ptraceError "pL")

pinitialConstantProductPoolCorrect ::
  Term
    s
    ( PDatum
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
pinitialConstantProductPoolCorrect =
  plam $
    \outputDatum
     requestValidatorHash
     timestamps
     outA
     outB
     assetASymbol
     assetAToken
     assetBSymbol
     assetBToken
     poolOutShares -> unTermCont $ do
        validityTimestamps <- pmatchC timestamps
        totalEmittedShares <- pletC (maxShareTokens - poolOutShares)
        totalEmittedSharesP1 <- pletC (totalEmittedShares + 1)
        outAB <- pletC (outA * outB)
        let expectedPoolDatum =
              ppoolDatum
                requestValidatorHash
                (assetASymbol, assetAToken)
                (assetBSymbol, assetBToken)
                validityTimestamps.lowerBound
                (0, 0)
                pdnothing
                (0, 0)
                (0, 0)
                pdnothing
                C.swapFeeInBasis
                C.protocolFeeInBasis
                C.projectFeeInBasis
                C.reserveFeeInBasis
                C.feeBasis
                C.agentFeeAda
                pconstantProductPoolDatum
        pure $
          pand'List
            [ ptraceIfFalse "id" $ (pcon . PDatum . pforgetData . pdata $ expectedPoolDatum) #== outputDatum
            , ptraceIfFalse "s^2" $ totalEmittedShares * totalEmittedShares #<= outAB
            , ptraceIfFalse "s+1^2" $ totalEmittedSharesP1 * totalEmittedSharesP1 #> outAB
            ]

pswapAssets :: Term s (PPoolState 'ConstantProduct :--> PBool :--> PInteger :--> PSwapResult 'ConstantProduct)
pswapAssets = plam $ \poolState swapAToB lockedQty -> pmatch poolState $ \state ->
  pif
    swapAToB
    ( unTermCont $ do
        reserves <- pmatchC $ pnewReserves # state.swapFeeInBasis # state.protocolFeeInBasis # state.projectFeeInBasis # state.reserveFeeInBasis # state.feeBasis # state.qtyA # state.qtyB # lockedQty
        let newA = reserves.newX
            newB = reserves.newY
            outTokens = reserves.outY
            protocolA = reserves.protocolFeeX
            projectA = reserves.projectFeeX
            reserveFeeA = reserves.reserveFeeX
            -- The newA is bigger as the swap is from A to B
            _swappedA = newA - state.qtyA
            -- The newB is smaller as B is deducted from the pool
            _swappedB = state.qtyB - newB
        pure . pcon $
          PSwapResult
            { newState =
                ( pcon
                    ( state
                        { qtyA = newA
                        , qtyB = newB
                        , qtyTreasuryA = protocolA + state.qtyTreasuryA
                        , qtyProjectTreasuryA = projectA + state.qtyProjectTreasuryA
                        , qtyReserveTreasuryA = reserveFeeA + state.qtyReserveTreasuryA
                        }
                    )
                )
            , outTokens
            }
    )
    ( unTermCont $ do
        reserves <- pmatchC $ pnewReserves # state.swapFeeInBasis # state.protocolFeeInBasis # state.projectFeeInBasis # state.reserveFeeInBasis # state.feeBasis # state.qtyB # state.qtyA # lockedQty
        let newB = reserves.newX
            newA = reserves.newY
            outTokens = reserves.outY
            protocolB = reserves.protocolFeeX
            projectB = reserves.projectFeeX
            reserveFeeB = reserves.reserveFeeX
            -- The newA is smaller as A is deducted from the pool
            _swappedA = state.qtyA - newA
            -- The newB is bigger as the swap is from B to A
            _swappedB = newB - state.qtyB
        pure . pcon $
          PSwapResult
            { newState =
                ( pcon
                    ( state
                        { qtyA = newA
                        , qtyB = newB
                        , qtyTreasuryB = protocolB + state.qtyTreasuryB
                        , qtyProjectTreasuryB = projectB + state.qtyProjectTreasuryB
                        , qtyReserveTreasuryB = reserveFeeB + state.qtyReserveTreasuryB
                        }
                    )
                )
            , outTokens
            }
    )

pnewReserves ::
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PNewReserves
    )
pnewReserves = phoistAcyclic $ plam $ \swapFeeInBasis protocolFeeInBasis projectFeeInBasis reserveFeeInBasis feeBasis oldX oldY lockX -> unTermCont $ do
  swapFeeX <- pletC (pdivideCeil # (lockX * swapFeeInBasis) # feeBasis)
  protocolFeeX <- pletC (pdiv # (lockX * protocolFeeInBasis) # feeBasis)
  projectFeeX <- pletC (pdiv # (lockX * projectFeeInBasis) # feeBasis)
  reserveFeeX <- pletC (pdiv # (lockX * reserveFeeInBasis) # feeBasis)
  newX <- pletC (oldX + lockX - protocolFeeX - projectFeeX - reserveFeeX)
  newY <- pletC (pdivideCeil # (oldX * oldY) # (oldX + lockX - swapFeeX - protocolFeeX - projectFeeX - reserveFeeX))
  outY <- pletC (oldY - newY)
  (pure . pcon) PNewReserves {newX, newY, outY, protocolFeeX, projectFeeX, reserveFeeX}

premoveLiquidityZapOut ::
  SwapDirection ->
  Term s (PPoolState 'ConstantProduct) ->
  Term s PInteger ->
  Term s (PPoolState 'ConstantProduct)
premoveLiquidityZapOut SwapAToB poolState redeemedShares = unTermCont do
  state <- pmatchC poolState
  afterRemoveLiquidity <- pletC (premoveLiquidity # poolState # pnothing # redeemedShares)
  afterRemoveLiquidityState <- pmatchC afterRemoveLiquidity
  afterSwap <- pmatchC (pswapAssets # afterRemoveLiquidity # ptrue # (state.qtyA - afterRemoveLiquidityState.qtyA))
  pure afterSwap.newState
premoveLiquidityZapOut SwapBToA poolState redeemedShares = unTermCont do
  state <- pmatchC poolState
  afterRemoveLiquidity <- pletC (premoveLiquidity # poolState # pnothing # redeemedShares)
  afterRemoveLiquidityState <- pmatchC afterRemoveLiquidity
  afterSwap <- pmatchC (pswapAssets # afterRemoveLiquidity # pfalse # (state.qtyB - afterRemoveLiquidityState.qtyB))
  pure afterSwap.newState

{- |
We want to have a swap with a balanced add liquidity afterwards.
The intended swapped values is passed in the redeemer.
We need to verify that it indeed produces a correctly balanced add liquidity.
That means the user gets roughly the same number of shares out of both assets.
If a token X is swapped to a token Y, we can expect that there exists a swap X such that
the expected shares from X are slightly bigger than the expected shares from Y, but
this relation reverses if one less token X is swapped.
We calculate the expected shares from swap X and from (swap X - 1) and ensure
the given swap X indeed produces roughly equal shares for the user.
-}
paddLiquidityZapIn ::
  SwapDirection ->
  Term s (PPoolState 'ConstantProduct) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAddLiquidityResult 'ConstantProduct)
paddLiquidityZapIn SwapAToB poolState addA swapA = unTermCont do
  -- First we calculate the shares received with the provided intended swapA value
  swapResult <- pmatchC (pswapAssets # poolState # ptrue # swapA)
  afterSwap <- pmatchC swapResult.newState
  totalEmittedShares <- pletC $ pconstant maxShareTokens - afterSwap.qtyShares
  addLiquidityA <- pletC $ addA - swapA
  let sharesFromA = pdiv # (addLiquidityA * totalEmittedShares) # afterSwap.qtyA
      sharesFromB = pdiv # (swapResult.outTokens * totalEmittedShares) # afterSwap.qtyB
  -- Now we calculate the shares received if the swapped value is -1 from the intended one
  swapResult1 <- pmatchC (pswapAssets # poolState # ptrue # (swapA - 1))
  afterSwap1 <- pmatchC swapResult1.newState
  addLiquidityA1 <- pletC $ addA - (swapA - 1)
  let sharesFromA1 = pdiv # (addLiquidityA1 * totalEmittedShares) # afterSwap1.qtyA
      sharesFromB1 = pdiv # (swapResult1.outTokens * totalEmittedShares) # afterSwap1.qtyB
  -- A zap-in is correct when the given swapA produces the most balanced swap possible.
  -- That means user gets more shares from B, but providing even one less token
  -- will tip the scales and the shares from A will get bigger
  pure $
    pif
      ( pand'List
          [ -- failing this check means you need to swap more
            ptraceIfFalse "a<=b" $
              sharesFromA #<= sharesFromB
          , -- failing this check means you need to swap less
            ptraceIfFalse "a1>b1" $
              sharesFromA1 #> sharesFromB1
          , -- restrict values that do not make sense
            ptraceIfFalse "s>0" $
              swapA #> 0
          , -- restrict values that do not make sense
            ptraceIfFalse "s<a" $
              swapA #< addA
          ]
      )
      (paddLiquidity # swapResult.newState # addLiquidityA # swapResult.outTokens)
      perror
paddLiquidityZapIn SwapBToA poolState addB swapB = unTermCont do
  -- First we calculate the shares received with the provided intended swapB value
  swapResult <- pmatchC (pswapAssets # poolState # pfalse # swapB)
  afterSwap <- pmatchC swapResult.newState
  totalEmittedShares <- pletC $ pconstant maxShareTokens - afterSwap.qtyShares
  addLiquidityB <- pletC $ addB - swapB
  let sharesFromA = pdiv # (swapResult.outTokens * totalEmittedShares) # afterSwap.qtyA
      sharesFromB = pdiv # (addLiquidityB * totalEmittedShares) # afterSwap.qtyB
  -- Then we calculate the shares received if the swapped value is -1 from the intended one
  swapResult1 <- pmatchC (pswapAssets # poolState # pfalse # (swapB - 1))
  afterSwap1 <- pmatchC swapResult1.newState
  addLiquidityB1 <- pletC $ addB - (swapB - 1)
  let sharesFromA1 = pdiv # (swapResult1.outTokens * totalEmittedShares) # afterSwap1.qtyA
      sharesFromB1 = pdiv # (addLiquidityB1 * totalEmittedShares) # afterSwap1.qtyB
  -- A zap-in is correct when the given swapB produces the most balanced swap possible.
  -- That means user gets more shares from A, but providing even one less token
  -- will tip the scales and the shares from B will get bigger
  pure $
    pif
      ( pand'List
          [ -- failing this check means you need to swap more
            ptraceIfFalse "b<=a" $
              sharesFromB #<= sharesFromA
          , -- failing this check means you need to swap less
            ptraceIfFalse "b1>a1" $
              sharesFromB1 #> sharesFromA1
          ]
      )
      (paddLiquidity # swapResult.newState # swapResult.outTokens # addLiquidityB)
      perror

paddLiquidity :: Term s (PPoolState 'ConstantProduct :--> PInteger :--> PInteger :--> PAddLiquidityResult 'ConstantProduct)
paddLiquidity = phoistAcyclic $ plam $ \poolState addA addB -> unTermCont $ do
  state <- pmatchC poolState
  totalEmittedShares <- pletC (pconstant maxShareTokens - state.qtyShares)
  let earnedSharesFromA = pdiv # (addA * totalEmittedShares) # state.qtyA
      earnedSharesFromB = pdiv # (addB * totalEmittedShares) # state.qtyB
  earnedShares <- pletC (pmin # earnedSharesFromA # earnedSharesFromB)
  (pure . pcon)
    PAddLiquidityResult
      { newState =
          pcon
            state
              { qtyA = state.qtyA + addA
              , qtyB = state.qtyB + addB
              , qtyShares = state.qtyShares - earnedShares
              }
      , earnedShares
      }

data NewReserves = NewReserves
  { newX :: Integer
  , newY :: Integer
  , outY :: Integer
  , protocolFeeX :: Integer
  , projectFeeX :: Integer
  , reserveFeeX :: Integer
  }
  deriving (Show, Eq)

data PNewReserves (s :: S) = PNewReserves
  { newX :: Term s PInteger
  , newY :: Term s PInteger
  , outY :: Term s PInteger
  , protocolFeeX :: Term s PInteger
  , projectFeeX :: Term s PInteger
  , reserveFeeX :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PNewReserves where
  type DPTStrat _ = PlutusTypeScott
