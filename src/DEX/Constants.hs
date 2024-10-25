module DEX.Constants where

import PlutusLedgerApi.V1.Value (tokenName)
import PlutusLedgerApi.V2 (TokenName)

{- |
  This is the exact amount of share tokens minted for every pool.

  Note: This corresponds to the maximum amount of any token in a single UTxO
  (https://github.com/input-output-hk/cardano-ledger-specs/blob/master/alonzo/test/cddl-files/alonzo.cddl#L364)
-}
maxShareTokens :: Num a => a
maxShareTokens = maxInt64

-- | The maximum number of tokens on one utxo.
maxInt64 :: Num a => a
maxInt64 = 9_223_372_036_854_775_807

{- |
  This is the minimum amount of ada that needs to be added in requests. It will not be used and will be fully returned
-}
requestOilAda :: Num a => a
requestOilAda = 2_000_000

{- |
  Swap fee is the fee that is returned to the pool for the liquidity providers.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
swapFeeInBasis :: Num a => a
swapFeeInBasis = 30

-- | The basis of any fee
feeBasis :: Num a => a
feeBasis = 10_000

{- |
  Protocol fee is the fee that is placed in the treasury.
  It is located in the pool eUTxO, but is not counted towards the pool liquidity.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
protocolFeeInBasis :: Num a => a
protocolFeeInBasis = 5

{- |
  Project fee is the fee that is placed in the project treasury.
  It is located in the pool eUTxO, but is not counted towards the pool liquidity.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
projectFeeInBasis :: Num a => a
projectFeeInBasis = 0

{- |
  Reserve fee is the fee that is placed in the reserve treasury.
  It is located in the pool eUTxO, but is not counted towards the pool liquidity.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
reserveFeeInBasis :: Num a => a
reserveFeeInBasis = 0

{- |
  Every swap is subject to a 0.06% = 6 basis points fee.
  0.05% is returned in the pool for liquidity providers.
  0.01% goes into the treasury.
  Note: those are just the default values and the actual numbers are controlled by the pool datum.
-}
stableswapSwapFeeInBasis :: Num a => a
stableswapSwapFeeInBasis = 5

stableswapFeeBasis :: Num a => a
stableswapFeeBasis = 10_000

stableswapProtocolFeeInBasis :: Num a => a
stableswapProtocolFeeInBasis = 1

-- | The chosen stableswap parameter A.
protocolStableswapA :: Num a => a
protocolStableswapA = 75

-- | The number of iterations for calculating the next D.
protocolStableswapIterationsD :: Num a => a
protocolStableswapIterationsD = 255

{- |
  This is a fee that needs to be included in every request. It compensates the agents for the tx fee.
  Note: this is just the default value and the actual numbers are controlled by the pool datum.
-}
agentFeeAda :: Num a => a
agentFeeAda = 2_000_000

maxTxValidityRangeSize :: Num a => a
maxTxValidityRangeSize = 86_400_000 -- 24 * 60 * 60 * 1_000 = one day

{- |
  This is sufficient min-ada that needs to be always present in pools and is paid by first liquidity provider.
-}
poolOilAda :: Num a => a
poolOilAda = 3_000_000

factoryTokenName :: TokenName
factoryTokenName = tokenName "F"

lpValidityTokenName :: TokenName
lpValidityTokenName = tokenName "L"

agentTokenName :: TokenName
agentTokenName = tokenName "X"

agentFeeAuthorityTokenName :: TokenName
agentFeeAuthorityTokenName = tokenName "Y"

feeAuthorityTokenName :: TokenName
feeAuthorityTokenName = tokenName "V"

stakingAgentTokenName :: TokenName
stakingAgentTokenName = tokenName "S"

treasuryVoteTokenName :: TokenName
treasuryVoteTokenName = tokenName "T"

{- |
  Amount of shares to be burned upon new pool creation.
  The value is the same as in Uniswap v2 [https://uniswap.org/whitepaper.pdf]
-}
burnedShareTokens :: Num a => a
burnedShareTokens = 1_000

vestingOwnerTokenName :: TokenName
vestingOwnerTokenName = tokenName "C"

addFarmingRewardsTokenName :: TokenName
addFarmingRewardsTokenName = tokenName "A"

defaultStakingAgentTokenCount :: Integer
defaultStakingAgentTokenCount = 1

minTimeToConsiderPoolAbandoned :: Integer
minTimeToConsiderPoolAbandoned = 1_209_600_000 -- 14 * 24 * 60 * 60 * 1_000 = 2 weeks

defaultTreasuryTotalVoteCount :: Integer
defaultTreasuryTotalVoteCount = 100_000

defaultTreasuryMinVoteCount :: Integer
defaultTreasuryMinVoteCount = 66_666
