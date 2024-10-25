{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module DEX.Factory where

import DEX.Constants qualified as C
import DEX.Pool.ConstantProduct (pinitialConstantProductPoolCorrect)
import DEX.Pool.Stableswap (pinitialStableswapPoolCorrect)
import DEX.Pool.Util
import DEX.Types.Classes
import DEX.Types.Factory
import DEX.Types.Pool
import Plutarch
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Api.V2
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.Mint.Util
import Plutarch.PlutusScript (toScript)
import Plutarch.Prelude
import Plutarch.Types.Base
import Plutarch.Util
import Plutus.Util
import PlutusLedgerApi.V2 (Address, CurrencySymbol, ScriptHash)
import PlutusTx qualified

data FactoryConfig = FactoryConfig
  { poolValidatorHashConstantProduct :: ScriptHash
  , requestValidatorHashConstantProduct :: ScriptHash
  , poolValidatorHashStableswap :: ScriptHash
  , requestValidatorHashStableswap :: ScriptHash
  , dexSymbol :: CurrencySymbol
  }
  deriving (Generic)

PlutusTx.makeIsDataIndexed ''FactoryConfig [('FactoryConfig, 0)]
PlutusTx.makeLift ''FactoryConfig

data PFactoryConfig (s :: S)
  = PFactoryConfig
      ( Term
          s
          ( PDataRecord
              '[ "poolValidatorHashConstantProduct" ':= PScriptHash
               , "requestValidatorHashConstantProduct" ':= PScriptHash
               , "poolValidatorHashStableswap" ':= PScriptHash
               , "requestValidatorHashStableswap" ':= PScriptHash
               , "dexSymbol" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PFactoryConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PFactoryConfig where
  type PLifted PFactoryConfig = FactoryConfig

deriving via
  (DerivePConstantViaData FactoryConfig PFactoryConfig)
  instance
    (PConstantDecl FactoryConfig)

{- |
This validates the creation of a liquidity pool and splitting of the factory.
The splitting needs to happen so unlimited number of pools can exist. In order for a transaction to be valid:

Factory:
   1. Both new factories need to contain a Factory token.
   2. New factories do not contain any other token except the factory token and ada (not to create an unspendable factory).
   3. Datums of the new Factories are correct. This means that the ranges are split as follows:
      (poolRangeFrom, poolRangeTo) --> (poolRangeFrom, newPoolHash) (newPoolHash, poolRangeTo).
      Note: Correct ordering of tx outputs is necessary: Left factory tx out index needs to be < right factory index.

New pool:
   1. We have to be exchanging different coins.
   2. Created pool fits in the factory pool range, meaning the correct factory creates the pool. This also means that the pool
      can not exist already, because when a pool is created, it would not fit into any factory range again.
   3. New pool does not contain any other token except:
      a. share tokens reserve
      b. lp validity token
      c. ada
      d. assetA lp (can overlap with ada)
      e. the other liquidity token = assetB lp
The pool can be either a constant product pool or a stableswap one.

Minted tokens:
   1. We minted 1 lp validity token and put it into the newly created pool
   2. We minted [maxShares - burnedShareTokens] pool share tokens and put all of them into the newly created pool except those that were
      given to the initial liquidity provider creating the pool
   3. 1 factory token is minted and put into the new factory
   4. No other dex tokens are minted

Liquidity pool:
   1. The new pool utxo is created, with correct datum and value (liquidity + pool validity token + share tokens reserve)
   2. The supplied liquidity is >0 and is enough such that liquidity provider receives >0 share tokens (after burning initial [burnedShareTokens])
   3. Transaction validity range is short enough. That means that we can take start timestamp to approximate the current time.

Liquidity provider:
   1. Adequate amount of minted share tokens are left outside the pool (and given to the liquidity provider)

The validators doesn't protect the user from sending the minted reward shares to an unspendable address.
It is expected that the front-end creates the right transaction and the user reviews it before signing.
-}
pvalidateCreatePool ::
  Term s PFactoryConfig ->
  Term s PPoolChoice ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PFactoryDatum ->
  Term s PScriptContext ->
  Term s PBool
pvalidateCreatePool
  factoryConfig
  poolChoice
  (assetASymbol, assetAToken)
  (assetBSymbol, assetBToken)
  datum
  context = unTermCont $ do
    self <- pletFieldsC @'["rangeFrom", "rangeTo"] datum

    config <-
      pletFieldsC
        @'[ "poolValidatorHashConstantProduct"
          , "requestValidatorHashConstantProduct"
          , "poolValidatorHashStableswap"
          , "requestValidatorHashStableswap"
          , "dexSymbol"
          ]
        factoryConfig
    contextF <- pletFieldsC @'["txInfo", "purpose"] context
    txInfoFields <- pletFieldsC @'["validRange", "inputs", "outputs", "redeemers", "mint"] contextF.txInfo
    txMint <- pletC $ txInfoFields.mint

    ownInput <- pletC (pownInput # contextF.purpose # txInfoFields.inputs)
    factoryValidatorHash <- pletC (pgetValidatorHashFromScriptAddress #$ pfield @"address" # ownInput)

    factoryInputs <-
      pletC
        ( pfilter
            # plam
              ( \(inInfo :: Term s PTxInInfo) ->
                  plet (pfield @"resolved" # inInfo) $ \o ->
                    (ppaysToCredential # factoryValidatorHash # o)
                      #&& (pvalueOf # (ptxOutValue # o) # config.dexSymbol # pconstant C.factoryTokenName #== 1)
              )
            # txInfoFields.inputs
        )
    -- NOTE: This validation allows having multiple factory outputs on the same address.
    --       However, we enforce there are only two factories with one token
    --       and that only one factory token was minted
    factoryOutputs <-
      pletC
        ( pfilter
            # plam
              ( \o ->
                  (ppaysToCredential # factoryValidatorHash # o)
                    #&& (pvalueOf # (ptxOutValue # o) # config.dexSymbol # pconstant C.factoryTokenName #== 1)
              )
            # txInfoFields.outputs
        )

    timestamps' <- pletC (pfiniteTxValidityRangeTimestamps # txInfoFields.validRange)
    timestamps <- pmatchC timestamps'

    PQuartet shareToken poolHash requestHash isDatumCorrect <-
      pmatchC
        ( pmatch poolChoice \case
            PUseConstantProduct _ ->
              pquartet
                ( pshareTokenName
                    # ppoolIdentifier @'ConstantProduct
                    # 1
                    # 1
                    # assetASymbol
                    # assetAToken
                    # assetBSymbol
                    # assetBToken
                )
                config.poolValidatorHashConstantProduct
                config.requestValidatorHashConstantProduct
                pinitialConstantProductPoolCorrect
            PUseStableswap r -> pletFields @'["aScale", "bScale"] r \rF ->
              pquartet
                ( pshareTokenName
                    # ppoolIdentifier @'Stableswap
                    # rF.aScale
                    # rF.bScale
                    # assetASymbol
                    # assetAToken
                    # assetBSymbol
                    # assetBToken
                )
                config.poolValidatorHashStableswap
                config.requestValidatorHashStableswap
                (pinitialStableswapPoolCorrect # rF.aScale # rF.bScale)
        )

    let splitPoint = pto shareToken

    -- NOTE: This validation allows having multiple pool outputs on the same address.
    --       However, we enforce here that only one has exactly one pool token.
    --       And later on we enforce that only one pool token was minted.
    poolOut <-
      pletC
        ( passertSingleton "expected 1 pool output"
            #$ pfilter
            # plam
              ( \o ->
                  (ppaysToCredential # poolHash # o)
                    #&& (pvalueOf # (ptxOutValue # o) # config.dexSymbol # pconstant C.lpValidityTokenName #== 1)
              )
            # txInfoFields.outputs
        )
    poolOutFields <- pletFieldsC @'["datum", "value"] poolOut
    poolOutValue <- pletC poolOutFields.value
    poolOutDatum <- pletC (ptryFromInlineDatum # poolOutFields.datum)

    let isAdaPool = pisAda # assetASymbol

    outAda <- pletC ((pvalueOf # poolOutValue # padaSymbol # padaToken) - C.poolOilAda)
    outA <- pletC (pif isAdaPool outAda (pvalueOf # poolOutValue # assetASymbol # assetAToken))
    outB <- pletC (pvalueOf # poolOutValue # assetBSymbol # assetBToken)

    poolOutShares <- pletC (pvalueOf # poolOutValue # config.dexSymbol # shareToken)
    totalEmittedShares <- pletC (C.maxShareTokens - poolOutShares)
    let expectedMintedShares = C.maxShareTokens - C.burnedShareTokens
        userGivenShares = totalEmittedShares - C.burnedShareTokens

    let validityShortEnough =
          pisTxValidityRangeShortEnough # timestamps.lowerBound # timestamps.upperBound
        mintedOnePoolToken =
          pvalueOf # txMint # config.dexSymbol # pconstant C.lpValidityTokenName #== 1
        mintedOneFactoryToken =
          pvalueOf # txMint # config.dexSymbol # pconstant C.factoryTokenName #== 1
        mintedThreeTokens = plength # pto (pvalueOfCurrency txMint config.dexSymbol) #== 3
        mintExactAmountOfShareTokens =
          pvalueOf # txMint # config.dexSymbol # shareToken #== expectedMintedShares
        factorySplitCorrectly =
          pisCorrectSplit self.rangeFrom self.rangeTo factoryOutputs splitPoint
        singleFactorySplit =
          plength # factoryInputs #== 1
        initialPoolDatumRight =
          isDatumCorrect
            # poolOutDatum
            # requestHash
            # timestamps'
            # outA
            # outB
            # assetASymbol
            # assetAToken
            # assetBSymbol
            # assetBToken
            # poolOutShares
        -- NOTE: there are additional type-related checks for each pool in pinitialPoolCorrect
        poolLiquidityFulfilled =
          pand'List
            [ outA #> 0
            , outB #> 0
            , -- outAda = utxo.ada - pool oil
              -- this check enforces the pool has at least the pool oil ada
              -- which we also enforce when we apply requests in later transactions
              -- if the asset A is ada, that's fine as well
              outAda #>= 0 #|| pisAda # assetASymbol
            ]
        assetsChecked =
          pand'List
            [ -- asset A < asset B, so if it's an ada pool, asset a is always ada
              pif
                (assetASymbol #== assetBSymbol)
                (assetAToken #< assetBToken)
                (assetASymbol #< assetBSymbol)
            , -- assets can't overlap with the shares/pool tokens
              pnot # (assetASymbol #== config.dexSymbol #&& assetAToken #== shareToken)
            , pnot # (assetBSymbol #== config.dexSymbol #&& assetBToken #== shareToken)
            , pnot # (assetASymbol #== config.dexSymbol #&& assetAToken #== pconstant C.lpValidityTokenName)
            , pnot # (assetBSymbol #== config.dexSymbol #&& assetBToken #== pconstant C.lpValidityTokenName)
            ]
        numberOfPoolTokensChecked =
          (pcountOfUniqueTokens # poolOutValue)
            #== (pexpectedNumberOfTokensInThePool # assetASymbol # poolOutShares)
        userLiquidityFulfilled = userGivenShares #> 0

    pure $
      pand'List
        [ ptraceIfFalse "f-vse" validityShortEnough
        , ptraceIfFalse "f-mpt" mintedOnePoolToken
        , ptraceIfFalse "f-mst" mintExactAmountOfShareTokens
        , ptraceIfFalse "f-m3t" mintedThreeTokens
        , ptraceIfFalse "f-pdr" initialPoolDatumRight
        , ptraceIfFalse "f-plf" poolLiquidityFulfilled
        , ptraceIfFalse "f-ulf" userLiquidityFulfilled
        , ptraceIfFalse "f-ach" assetsChecked
        , ptraceIfFalse "f-pnt" numberOfPoolTokensChecked
        , -- If a single factory token was minted and put directly into the factory script
          -- From:
          --   1. Single valid factory is consumed
          --   2. at most 1 new factory token can be minted
          --   3. exactly 2 factory outputs can exist
          --   4. both factories need to include a factory token (below)
          -- follows that no other output can contain a factory token.
          ptraceIfFalse "f-sfs" singleFactorySplit
        , ptraceIfFalse "f-m1f" mintedOneFactoryToken
        , ptraceIfFalse "f-fsc" factorySplitCorrectly
        ]

{- | Checks the following:
1. The split point is set correctly for new factories
2. There are 2 correct output factories
3. The output factories have 2 unique tokens
-}
pisCorrectSplit ::
  Term s PByteString ->
  Term s PByteString ->
  Term s (PBuiltinList PTxOut) ->
  Term s PByteString ->
  Term s PBool
pisCorrectSplit rangeFrom rangeTo factoryOutputs splitPoint = unTermCont do
  let leftFactoryDatumData = pupcast $ pfactoryDatum # rangeFrom # splitPoint
      rightFactoryDatumData = pupcast $ pfactoryDatum # splitPoint # rangeTo
      splitPointInRange =
        pand'List [splitPoint #> rangeFrom, splitPoint #< rangeTo]
      validOutputs =
        pisMatchingDoubleton
          (pmatchFactory # leftFactoryDatumData)
          (pmatchFactory # rightFactoryDatumData)
          factoryOutputs

  pure $ pand'List [splitPointInRange, validOutputs]

pmatchFactory :: Term s (PData :--> PTxOut :--> PBool)
pmatchFactory = phoistAcyclic $ plam \expected out -> pletFields @'["datum", "value"] out \o ->
  pand'List
    [ pupcast (ptryFromInlineDatum # o.datum) #== expected
    , -- We allow factories to hold ada and factory validity tokens
      pcountOfUniqueTokens # o.value #== 2
    ]

pvalidateFactory :: Term s (PFactoryConfig :--> PFactoryDatum :--> PFactoryRedeemer :--> PScriptContext :--> PUnit)
pvalidateFactory = phoistAcyclic $ plam $ \config datum redeemer context -> perrorIfFalse #$ pmatch redeemer $ \case
  PCreate p -> pletFields @'["poolChoice", "assetASymbol", "assetAToken", "assetBSymbol", "assetBToken"] p \pF ->
    pvalidateCreatePool config pF.poolChoice (pF.assetASymbol, pF.assetAToken) (pF.assetBSymbol, pF.assetBToken) datum context

pfactoryValidator :: Term s (PFactoryConfig :--> PValidator)
pfactoryValidator = phoistAcyclic $ plam $ \config rawDatum rawRedeemer ctx ->
  let redeemer = ptryFrom rawRedeemer fst
      datum = ptryFrom rawDatum fst
   in popaque $ pvalidateFactory # config # datum # redeemer # ctx

factoryValidatorScript :: Script
factoryValidatorScript = toScript pfactoryValidator

factoryValidator :: FactoryConfig -> Script
factoryValidator c = toScript (pfactoryValidator # pconstant c)

factoryScriptAddress :: FactoryConfig -> Address
factoryScriptAddress config =
  scriptHashToAddress (factoryScriptValidatorHash config)

factoryScriptValidatorHash :: FactoryConfig -> ScriptHash
factoryScriptValidatorHash c = scriptHash (factoryValidator c)
