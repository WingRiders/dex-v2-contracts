{-# LANGUAGE BlockArguments #-}

module DEX.Pool.Util (
  pfindNewPool,
  pshareTokenName,
  ppoolStateFromTx,
  pcheckNewPoolValue,
  pflatAssetsFromValue,
  pexpectedNumberOfTokensInThePool,
) where

import DEX.Constants qualified as C
import DEX.Types.Base
import DEX.Types.Classes
import DEX.Types.Pool
import Plutarch
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Api.V2
import Plutarch.Bool (pand')
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Prelude
import Plutarch.Types.Classes
import Plutarch.Util

pflatAssetsFromValue ::
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> (PValue 'Sorted 'Positive)
        :--> PInteger
        :--> PFlatAssets
    )
pflatAssetsFromValue = phoistAcyclic $ plam $ \assetASymbol assetAToken assetBSymbol assetBToken shareSymbol shareToken val oilAda ->
  pcon
    PFlatAssets
      { pfltA = pvalueOfWithOilCheck # val # assetASymbol # assetAToken # oilAda
      , pfltB = pvalueOf # val # assetBSymbol # assetBToken
      , pfltShares = pvalueOf # val # shareSymbol # shareToken
      }

{- | Share token name must be unique per pool.
It encodes the pool type, the asset scaling factors (useful for stableswap pools with assets with different decimals) and the assets:
shareTokenName = blake2b (blake2b poolType <> blake2b aScale <> blake2b bScale <> blake2b assetA <> blake2b assetB)
-}
pshareTokenName ::
  Term
    s
    ( PPoolTypeId
        :--> PInteger
        :--> PInteger
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PTokenName
    )
pshareTokenName = phoistAcyclic $ plam \poolTypeId aScale bScale assetASymbol assetAToken assetBSymbol assetBToken ->
  pcon $
    PTokenName $
      pblake2b_256
        #$ (pblake2b_256 # poolTypeId)
        <> (pblake2b_256 #$ pencodeUtf8 # pshow aScale)
        <> (pblake2b_256 #$ pencodeUtf8 # pshow bScale)
        <> (pblake2b_256 # (pto assetASymbol <> pto assetAToken))
        <> (pblake2b_256 # (pto assetBSymbol <> pto assetBToken))

{- |
  We have
  - assetA > 0
  - assetB > 0
  - oil ada if assetAClass != ada
  - share token class >= 0 (can be 0)
  - a validity token
-}
pexpectedNumberOfTokensInThePool :: Term s (PCurrencySymbol :--> PInteger :--> PInteger)
pexpectedNumberOfTokensInThePool =
  phoistAcyclic $ plam $ \assetASymbol qtyShares -> pif (pisAda # assetASymbol) 4 5 - pif (qtyShares #== 0) 1 0

{- |
  Finds a new pool among transaction outputs. Also checks that there is only one valid pool there.
  The search ignores the staking credentials, which allows the transaction creators to change it.
  ! Warning ! This function will find **only valid pools**. There might still be invalid
              outputs on the pool's address. The correctness of the single valid pool should
              be ensured by the contracts.
-}
pfindNewPool :: Term s (PScriptHash :--> PCurrencySymbol :--> PTokenName :--> PBuiltinList PTxOut :--> PTxOut)
pfindNewPool = phoistAcyclic $ plam $ \poolValidatorHash dexSymbol poolToken txOutputs ->
  passertSingleton "pN"
    #$ pfilter
    # plam
      ( \o ->
          (ppaysToCredential # poolValidatorHash # o)
            #&& (pvalueOf # (pfield @"value" # o) # dexSymbol # poolToken #== 1)
      )
    # txOutputs

pcheckNewPoolValue ::
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PValue 'Sorted 'Positive
        :--> PPoolState t
        :--> PBool
    )
pcheckNewPoolValue =
  phoistAcyclic $ plam $ \dexSymbol poolToken shareToken assetASymbol assetAToken assetBSymbol assetBToken poolValue poolState ->
    pmatch poolState $ \state ->
      let
        assetAChecked =
          pif
            (pisAda # assetASymbol)
            ( (pvalueOf # poolValue # assetASymbol # assetAToken)
                #== (state.qtyA + state.qtyTreasuryA + state.qtyProjectTreasuryA + state.qtyReserveTreasuryA + C.poolOilAda)
            )
            ( pand'
                # ( (pvalueOf # poolValue # assetASymbol # assetAToken)
                      #== (state.qtyA + state.qtyTreasuryA + state.qtyProjectTreasuryA + state.qtyReserveTreasuryA)
                  )
                # (pvalueOf # poolValue # padaSymbol # padaToken #>= C.poolOilAda)
            )
        assetBChecked =
          (pvalueOf # poolValue # assetBSymbol # assetBToken)
            #== (state.qtyB + state.qtyTreasuryB + state.qtyProjectTreasuryB + state.qtyReserveTreasuryB)
        sharesChecked = pvalueOf # poolValue # dexSymbol # shareToken #== state.qtyShares
        validityChecked = pvalueOf # poolValue # dexSymbol # poolToken #== 1
        expectedTokensNumberChecked =
          (pexpectedNumberOfTokensInThePool # assetASymbol # state.qtyShares)
            #== (pcountOfUniqueTokens # poolValue)
       in
        pand'List
          [ ptraceIfFalse "Invalid asset A pool value" assetAChecked
          , ptraceIfFalse "Invalid asset B pool value" assetBChecked
          , ptraceIfFalse "Invalid shares pool value" sharesChecked
          , ptraceIfFalse "Invalid validity token pool value" validityChecked
          , ptraceIfFalse "Invalid numbers of tokens in the pool" expectedTokensNumberChecked
          ]

ppoolStateFromTx ::
  (ScottConvertible (PPoolSpecificDatum t), ScottOf (PPoolSpecificDatum t) ~ PPoolSpecificState t) =>
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PCurrencySymbol
        :--> PTokenName
        :--> PValue 'Sorted 'Positive
        :--> PPoolSpecificDatum t
        :--> PPoolState t
    )
ppoolStateFromTx =
  phoistAcyclic $
    plam $
      \treasuryA
       treasuryB
       projectTreasuryA
       projectTreasuryB
       reserveTreasuryA
       reserveTreasuryB
       assetASymbol
       assetAToken
       assetBSymbol
       assetBToken
       swapFeeInBasis
       protocolFeeInBasis
       projectFeeInBasis
       reserveFeeInBasis
       feeBasis
       shareSymbol
       shareToken
       poolValue
       specifics ->
          let fullA = pvalueOfWithOilCheck # poolValue # assetASymbol # assetAToken # C.poolOilAda
              fullB = pvalueOf # poolValue # assetBSymbol # assetBToken
              fullShares = pvalueOf # poolValue # shareSymbol # shareToken
           in ppoolState
                ( fullA - treasuryA - projectTreasuryA - reserveTreasuryA
                , fullB - treasuryB - projectTreasuryB - reserveTreasuryB
                , fullShares
                )
                (treasuryA, treasuryB)
                (projectTreasuryA, projectTreasuryB)
                (reserveTreasuryA, reserveTreasuryB)
                swapFeeInBasis
                protocolFeeInBasis
                projectFeeInBasis
                reserveFeeInBasis
                feeBasis
                (toScott specifics)
