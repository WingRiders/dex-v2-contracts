{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module DEX.Types.Classes where

import DEX.Types.Base
import DEX.Types.Pool
import DEX.Types.Request
import Plutarch
import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PDatum, PMaybeData, PPOSIXTime, PScriptHash, PTokenName)
import Plutarch.Prelude

type PWantedShares = PInteger
type PWantedAsset = PInteger

{- | Use that type to mean the pool datum.
 It ensures you include the parameters common for all pools.
-}
type PPoolDatum (t :: PoolType) = PPoolDatum' (PPoolSpecificDatum t)

{- | Use that type to mean the scott-encoded pool data.
 It ensures you include the parameters common for all pools.
-}
type PPoolState (t :: PoolType) = PPoolState' (PPoolSpecificState t)

class Pool (t :: PoolType) where
  -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#injective-type-families
  -- We need injective type families here so GHC can figure out
  -- the types, as t appears only under the type families in the type class

  -- | Unique pool data.
  -- Use `PPoolDatum t` type to ensure you include the common parameters as well.
  type PPoolSpecificDatum t = (t1 :: PType) | t1 -> t

  -- | Unique pool scott-encoded data.
  -- Use `PPoolState t` type to ensure you include the common parameters as well.
  type PPoolSpecificState t = (t1 :: PType) | t1 -> t

  -- | A unique pool type identifier.
  -- Used to compute the pool hash.
  ppoolIdentifier :: ClosedTerm PPoolTypeId

  poolType :: PoolType

  pscaling :: Term s (PPoolSpecificDatum t) -> Term s (PPair PInteger PInteger)

  pisDatumCorrectAfterEmergency :: Term s PDatum -> Term s PDatum -> Term s (PPoolState t) -> Term s PBool

  -- | Remove liquidity formula.
  -- Doesn't do any validation, just calculates the next expected state
  -- from the given wanted tokens, inputs, and outputs assuming remove liquidity request.
  premoveLiquidity ::
    Term
      s
      ( PPoolState t
          :--> PMaybe PInteger -- it is nothing for emergency withdrawals
          :--> PInteger
          :--> PPoolState t
      )

  -- | Swap request application.
  -- Includes the validation.
  papplySwap ::
    Term s PSwapDirection ->
    Term s PInteger ->
    Term s (PPoolState t) ->
    Term s PParsedRequest ->
    Term s (PPoolState t)

  -- | Add liquidity request application.
  -- Includes the validation.
  papplyAddLiquidity ::
    Term s PWantedShares ->
    Term s (PPoolState t) ->
    Term s PParsedRequest ->
    Term s (PPoolState t)

  -- | Applies adding staking rewards into the pool.
  papplyAddStakingRewards ::
    Term s (PPoolState t) ->
    Term s PParsedRequest ->
    Term s (PPoolState t)

  -- | Withdraw liquidity request application.
  -- Includes the validation.
  papplyWithdrawLiquidity ::
    Term s PWantedAsset ->
    Term s PWantedAsset ->
    Term s (PPoolState t) ->
    Term s PParsedRequest ->
    Term s (PPoolState t)

data PRemoveLiquidityResult a (s :: S) = PRemoveLiquidityResult
  { prlrNewPoolState :: Term s (PPoolState a)
  , prlrRemoveA :: Term s PWantedShares
  , prlrRemoveB :: Term s PWantedShares
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PRemoveLiquidityResult a) where
  type DPTStrat _ = PlutusTypeScott

data AddLiquidityResult a = AddLiquidityResult
  { newState :: PoolState a
  , earnedShares :: Integer
  }
  deriving stock (Show)

data PAddLiquidityResult a (s :: S) = PAddLiquidityResult
  { newState :: Term s (PPoolState a)
  , earnedShares :: Term s PWantedShares
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PAddLiquidityResult a) where
  type DPTStrat _ = PlutusTypeScott

data SwapResult a = SwapResult
  { newState :: PoolState a
  , outTokens :: Integer
  }
  deriving stock (Show)

data PSwapResult a (s :: S) = PSwapResult
  { newState :: Term s (PPoolState a)
  , outTokens :: Term s PWantedAsset
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (PSwapResult a) where
  type DPTStrat _ = PlutusTypeScott

ppoolState ::
  (Term s PInteger, Term s PInteger, Term s PInteger) ->
  (Term s PInteger, Term s PInteger) ->
  (Term s PInteger, Term s PInteger) ->
  (Term s PInteger, Term s PInteger) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PPoolSpecificState t) ->
  Term s (PPoolState t)
ppoolState
  (qtyA, qtyB, qtyShares)
  (qtyTreasuryA, qtyTreasuryB)
  (qtyProjectTreasuryA, qtyProjectTreasuryB)
  (qtyReserveTreasuryA, qtyReserveTreasuryB)
  swapFeeInBasis
  protocolFeeInBasis
  projectFeeInBasis
  reserveFeeInBasis
  feeBasis
  poolSpecifics =
    pcon
      PPoolState'
        { qtyA
        , qtyB
        , qtyShares
        , qtyTreasuryA
        , qtyTreasuryB
        , qtyProjectTreasuryA
        , qtyProjectTreasuryB
        , qtyReserveTreasuryA
        , qtyReserveTreasuryB
        , swapFeeInBasis
        , protocolFeeInBasis
        , projectFeeInBasis
        , reserveFeeInBasis
        , feeBasis
        , poolSpecifics
        }

ppoolDatum ::
  PIsData a =>
  Term s PScriptHash ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PPOSIXTime ->
  (Term s PInteger, Term s PInteger) ->
  (Term s (PMaybeData (PAsData PAddress))) ->
  (Term s PInteger, Term s PInteger) ->
  (Term s PInteger, Term s PInteger) ->
  Term s (PMaybeData (PAsData PAddress)) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s a ->
  Term s (PPoolDatum' a)
ppoolDatum
  requestScriptHash
  (assetASymbol, assetAToken)
  (assetBSymbol, assetBToken)
  lastInteraction
  (treasuryA, treasuryB)
  projectBeneficiary
  (projectTreasuryA, projectTreasuryB)
  (reserveTreasuryA, reserveTreasuryB)
  reserveBeneficiary
  swapFeeInBasis
  protocolFeeInBasis
  projectFeeInBasis
  reserveFeeInBasis
  feeBasis
  agentFeeAda
  poolSpecifics =
    (pcon . PPoolDatum') $
      (pdcons # pdata requestScriptHash)
        #$ (pdcons # pdata assetASymbol)
        #$ (pdcons # pdata assetAToken)
        #$ (pdcons # pdata assetBSymbol)
        #$ (pdcons # pdata assetBToken)
        #$ (pdcons # pdata swapFeeInBasis)
        #$ (pdcons # pdata protocolFeeInBasis)
        #$ (pdcons # pdata projectFeeInBasis)
        #$ (pdcons # pdata reserveFeeInBasis)
        #$ (pdcons # pdata feeBasis)
        #$ (pdcons # pdata agentFeeAda)
        #$ (pdcons # pdata lastInteraction)
        #$ (pdcons # pdata treasuryA)
        #$ (pdcons # pdata treasuryB)
        #$ (pdcons # pdata projectTreasuryA)
        #$ (pdcons # pdata projectTreasuryB)
        #$ (pdcons # pdata reserveTreasuryA)
        #$ (pdcons # pdata reserveTreasuryB)
        #$ (pdcons # pdata projectBeneficiary)
        #$ (pdcons # pdata reserveBeneficiary)
        #$ (pdcons # pdata poolSpecifics)
        #$ pdnil
