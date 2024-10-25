{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
  This executable should should export all contracts written in Plutarch,
  so that they can be imported into the frontend/backend.
-}
module ExportDEX where

import DEX.Constants qualified as C
import DEX.Factory
import DEX.Mint.Validity (ValidityConfig (..), validityMintingPolicy)
import DEX.Pool
import DEX.Pool.ConstantProduct ()
import DEX.Pool.Stableswap ()
import DEX.Request
import DEX.Staking.RewardMint
import DEX.Treasury.Holder
import DEX.Types.Classes
import DEX.Types.Pool (PoolType (..))
import DEX.Types.Treasury (TreasuryHolderParameters (..))
import Data.Aeson (ToJSON)
import Data.Char (toLower)
import Data.Maybe (
  listToMaybe,
 )
import Data.String
import ExportUtils
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import Other.FixedSupplyPolicy
import Plutarch.Lift (plift)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClass,
 )
import PlutusLedgerApi.V2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import Prelude

poolIdentifier :: forall (t :: PoolType). Pool t => BuiltinByteString
poolIdentifier = BuiltinByteString (plift (ppoolIdentifier @t))

type DexUniqueRef = TxOutRef

data PeripheryInfo = PeripheryInfo
  { treasuryHolderParameters :: TreasuryHolderParameters
  }
  deriving (Show, Generic, ToJSON)

data PeripheryInfoShared = PeripheryInfoShared
  { stakingAgentToken :: AssetClass
  , treasuryVoteToken :: AssetClass
  }
  deriving (Show, Generic, ToJSON)

data ExportInfo = ExportInfo
  { contractParameters :: DexParameters
  , constants :: DexConstants
  , validators :: [ValidatorMeasurement]
  , policies :: [PolicyMeasurement]
  , periphery :: PeripheryInfo
  }
  deriving (Show, Generic, ToJSON)

data ExportInfoShared = ExportInfoShared
  { policies :: [PolicyMeasurement]
  , periphery :: PeripheryInfoShared
  }
  deriving (Show, Generic, ToJSON)

data DexConstants = DexConstants
  { initialFeeBasis :: !Integer
  , initialSwapFeeInBasisConstantProduct :: !Integer
  , initialSwapFeeInBasisStableswap :: !Integer
  , initialProtocolFeeInBasisConstantProduct
    , initialProtocolFeeInBasisStableswap ::
      !Integer
  , initialProjectFeeInBasis :: !Integer
  , initialReserveFeeInBasis :: !Integer
  , maxShareTokens :: !String
  -- ^ Too large to fit into the JS Number
  , burnedShareTokens :: !Integer
  , poolOilAda :: !Integer
  , minRequestOilAda :: !Integer
  , initialAgentFeeAda :: !Integer
  , maxTxValidityRangeSize :: !Integer
  , minTimeToConsiderPoolAbandoned :: !Integer
  }
  deriving (Show, Generic, ToJSON)

data DexParameters = DexParameters
  { agentToken :: AssetClass
  , factoryToken :: AssetClass
  , lpValidityToken :: AssetClass
  , agentFeeAuthorityToken :: AssetClass
  , feeAuthorityToken :: AssetClass
  }
  deriving (Generic, Show, ToJSON)

utxoHashForFactoryToken :: Network -> TxId
utxoHashForFactoryToken PrivateTest = "6be25f8ec58e435e5ba43fdb9a432083f856819b7f4c39ba37804121d16b79d6"
utxoHashForFactoryToken Test = "b5a329d84f8f01830e63929eca6f347336b0d1091fc350bf94098170f8a6ca88"
utxoHashForFactoryToken Main = "914a0f1e615f41b3f462855f6052a6b37d1d291aab53918ad4c9d5f4245204ce"

utxoIdxForFactoryToken :: Network -> Integer
utxoIdxForFactoryToken PrivateTest = 1
utxoIdxForFactoryToken Test = 1
utxoIdxForFactoryToken Main = 1

utxoForFactoryToken :: Network -> TxOutRef
utxoForFactoryToken net = TxOutRef (utxoHashForFactoryToken net) (utxoIdxForFactoryToken net)

utxoHashForAgentTokens :: Network -> TxId
utxoHashForAgentTokens PrivateTest = "6db74944d4a9d0fc362d903df38c21adadfb30403758a28bd61c5b5a37b29333"
utxoHashForAgentTokens Test = "47f4f3e0e0b7b266698d20632e13d57acbcf90f68cb15077bf1f4214627cbd53"
utxoHashForAgentTokens Main = "cca94dea66c58215a15c7e05b4a1f674684ae7a49fc66a6ac814f13727a31ef5"

utxoIdxForAgentTokens :: Network -> Integer
utxoIdxForAgentTokens PrivateTest = 1
utxoIdxForAgentTokens Test = 1
utxoIdxForAgentTokens Main = 0

utxoForAgentTokens :: Network -> TxOutRef
utxoForAgentTokens net = TxOutRef (utxoHashForAgentTokens net) (utxoIdxForAgentTokens net)

utxoHashForAgentFeeAuthorityTokens :: Network -> TxId
utxoHashForAgentFeeAuthorityTokens PrivateTest = "0000000000000000000000000000000000000000000000000000000000000000"
utxoHashForAgentFeeAuthorityTokens Test = "3a5ad1cdfae214d811b2810b3c00ddfe14086c118a26a1cbce631077115276a4"
utxoHashForAgentFeeAuthorityTokens Main = "f7f726e1248fd4825bf01e228d9974e75a56dac2bec53ea348b0ccda274e7641"

utxoIdxForAgentFeeAuthorityTokens :: Network -> Integer
utxoIdxForAgentFeeAuthorityTokens PrivateTest = 0
utxoIdxForAgentFeeAuthorityTokens Test = 1
utxoIdxForAgentFeeAuthorityTokens Main = 1

utxoForAgentFeeAuthorityTokens :: Network -> TxOutRef
utxoForAgentFeeAuthorityTokens net = TxOutRef (utxoHashForAgentFeeAuthorityTokens net) (utxoIdxForAgentFeeAuthorityTokens net)

utxoHashForFeeAuthorityTokens :: Network -> TxId
utxoHashForFeeAuthorityTokens PrivateTest = "0000000000000000000000000000000000000000000000000000000000000000"
utxoHashForFeeAuthorityTokens Test = "acd53f5d300144cf31edd55e8ceaced17ce506aebab69dc7f813ee6a114cbbd4"
utxoHashForFeeAuthorityTokens Main = "bc14fd532ad50367be2a0ed20542b56640163ae674b2afbeb5e895c0fd984234"

utxoIdxForFeeAuthorityTokens :: Network -> Integer
utxoIdxForFeeAuthorityTokens PrivateTest = 0
utxoIdxForFeeAuthorityTokens Test = 1
utxoIdxForFeeAuthorityTokens Main = 1

utxoForFeeAuthorityTokens :: Network -> TxOutRef
utxoForFeeAuthorityTokens net = TxOutRef (utxoHashForFeeAuthorityTokens net) (utxoIdxForFeeAuthorityTokens net)

utxoHashForStakingAgentTokens :: Network -> TxId
utxoHashForStakingAgentTokens PrivateTest = "357f1f252eeaadcc2e38b78f9bf2ad98aa5008dedfc7ea4f0d79d97aeb89a077"
utxoHashForStakingAgentTokens Test = "e396c06ad0628f10114f14b0526c93c9fbedd1183e95ab010b52f0feabd38c5b"
utxoHashForStakingAgentTokens Main = "a985a6317ac03747de30fdda5a447d5507a170a965d56d3bfb286c0b6cb45bd1"

utxoIdxForStakingAgentTokens :: Network -> Integer
utxoIdxForStakingAgentTokens PrivateTest = 1
utxoIdxForStakingAgentTokens Test = 0
utxoIdxForStakingAgentTokens Main = 0

utxoForStakingAgentTokens :: Network -> TxOutRef
utxoForStakingAgentTokens net = TxOutRef (utxoHashForStakingAgentTokens net) (utxoIdxForStakingAgentTokens net)

utxoHashForTreasuryVoteTokens :: Network -> TxId
utxoHashForTreasuryVoteTokens PrivateTest = "420370ab7ad788784f42c83a53b043bea35acb0ea99d6fa791fe082b0d9a3915"
utxoHashForTreasuryVoteTokens Test = "be0f42214e25527758b5b15390275050baf015abdde2acc0757f65b57dd4422e"
utxoHashForTreasuryVoteTokens Main = "ae16cbd0616251fd2290707f891255eeb202bcb527eabb173dda0565c9d57424"

utxoIdxForTreasuryVoteTokens :: Network -> Integer
utxoIdxForTreasuryVoteTokens PrivateTest = 1
utxoIdxForTreasuryVoteTokens Test = 0
utxoIdxForTreasuryVoteTokens Main = 0

utxoForTreasuryVoteTokens :: Network -> TxOutRef
utxoForTreasuryVoteTokens net = TxOutRef (utxoHashForTreasuryVoteTokens net) (utxoIdxForTreasuryVoteTokens net)

treasuryTotalVoteCount :: Network -> Integer
treasuryTotalVoteCount PrivateTest = 100_000
treasuryTotalVoteCount Test = 100_000
treasuryTotalVoteCount Main = 100_000

treasuryMinVoteCount :: Network -> Integer
treasuryMinVoteCount PrivateTest = 51_000
treasuryMinVoteCount Test = 51_000
treasuryMinVoteCount Main = 50_000

utxoHashForAddFarmingRewardsTokens :: Network -> TxId
utxoHashForAddFarmingRewardsTokens PrivateTest = "bc86f10db2916b021ee2a4d370d005975dc9d18894f5b03b6306f5f3659b9b13"
utxoHashForAddFarmingRewardsTokens Test = "37d095e0a0627c50598768d7a504d0688508b2f1203128e9b57ddc81497f531d"
utxoHashForAddFarmingRewardsTokens Main = "2e74782515d9921b2d84e89cf8b45d1df8cfff629313617043ce235a97fdf50c"

utxoIdxForAddFarmingRewardsTokens :: Network -> Integer
utxoIdxForAddFarmingRewardsTokens PrivateTest = 1
utxoIdxForAddFarmingRewardsTokens Test = 0
utxoIdxForAddFarmingRewardsTokens Main = 0

utxoForAddFarmingRewardsTokens :: Network -> TxOutRef
utxoForAddFarmingRewardsTokens net =
  TxOutRef (utxoHashForAddFarmingRewardsTokens net) (utxoIdxForAddFarmingRewardsTokens net)

dexUniqueRef :: Network -> DexUniqueRef
dexUniqueRef = utxoForFactoryToken

outDirV2 :: Network -> String
outDirV2 PrivateTest = "artifacts/dex-v2/private-testnet/"
outDirV2 Test = "artifacts/dex-v2/testnet/"
outDirV2 Main = "artifacts/dex-v2/mainnet/"

outDirShared :: Network -> String
outDirShared PrivateTest = "artifacts/dex-shared/private-testnet/"
outDirShared Test = "artifacts/dex-shared/testnet/"
outDirShared Main = "artifacts/dex-shared/mainnet/"

hardcodedAddFarmingRewardsTokenMeasurement :: Network -> IO PolicyMeasurement
hardcodedAddFarmingRewardsTokenMeasurement network = do
  case network of
    PrivateTest -> return $ PolicyMeasurement "add-farming-rewards-token" "ec05a96b48af6a59d9b84856e066f837120e4687ef55d3cfa7af845e" 3099
    Test -> return $ PolicyMeasurement "add-farming-rewards-token" "ec05a96b48af6a59d9b84856e066f837120e4687ef55d3cfa7af845e" 3099
    Main -> return $ PolicyMeasurement "add-farming-rewards-token" "1c0d57fdad384c5192735d38e467629316ad06650dcd038d54aa15ed" 3099

hardcodedStakingAgentTokenMeasurement :: Network -> IO PolicyMeasurement
hardcodedStakingAgentTokenMeasurement network = do
  case network of
    PrivateTest -> return $ PolicyMeasurement "staking-agent-token" "5e76d032be4de463e7adf1746f1c9bab274239e486cb280e4a56904e" 3099
    Test -> return $ PolicyMeasurement "staking-agent-token" "5e76d032be4de463e7adf1746f1c9bab274239e486cb280e4a56904e" 3099
    Main -> return $ PolicyMeasurement "staking-agent-token" "06782c3e4eba019f64c34635735f1c307555e40490d67197c4ca4428" 3099

hardcodedStakingRewardsTokenMeasurement :: Network -> IO PolicyMeasurement
hardcodedStakingRewardsTokenMeasurement network = do
  case network of
    PrivateTest -> return $ PolicyMeasurement "staking-rewards-token" "eab730739ac93ab7de17fb1e8919456b2c407ff89a133de5fbd1bf14" 2951
    Test -> return $ PolicyMeasurement "staking-rewards-token" "eab730739ac93ab7de17fb1e8919456b2c407ff89a133de5fbd1bf14" 2951
    Main -> return $ PolicyMeasurement "staking-rewards-token" "0ceaab7c2cec8f93c38b7c3598fa239f50bc14fcc6da9f016adb035c" 2951

hardcodedTreasuryTokenMeasurement :: Network -> IO PolicyMeasurement
hardcodedTreasuryTokenMeasurement network = do
  case network of
    PrivateTest -> return $ PolicyMeasurement "treasury-vote-token" "67e5f959b6e3700559f1c448d63bed7c365d2d3f6536fd21708aaf51" 3099
    Test -> return $ PolicyMeasurement "treasury-vote-token" "67e5f959b6e3700559f1c448d63bed7c365d2d3f6536fd21708aaf51" 3099
    Main -> return $ PolicyMeasurement "treasury-vote-token" "5f22f7b88b92355b3adee44aaca2ac0a689aac7f77671eb42e5338cf" 3099

main :: IO ()
main = do
  args <- getArgs
  let net = if listToMaybe args == Just "mainnet" then Main else if listToMaybe args == Just "private-testnet" then PrivateTest else Test

  putStrLn ""

  let bootstrapMode = maybe False (\s -> if map toLower s == "true" then True else False) bootstrapModeEnv
      bootstrapModeEnv = unsafePerformIO $ lookupEnv "HARDCODED_BOOTSTRAP"

  -- determine agent token symbol
  putStrLn $ "Creating agent token policy using utxo: " ++ show (utxoForAgentTokens net)
  agentTokenMeasurement <-
    exportPolicy
      (fixedSupplyPolicy (utxoForAgentTokens net), outDirV2 net, "agent-token", True, V2)
  let agentCurrencySymbol = fromString @CurrencySymbol agentTokenMeasurement.symbol
      agentToken = assetClass agentCurrencySymbol C.agentTokenName
  putStrLn $ "agent token currency symbol is " ++ show agentToken

  -- determine agent fee authority token symbol
  putStrLn $ "Creating agent fee authority token policy using utxo: " ++ show (utxoForAgentFeeAuthorityTokens net)
  agentFeeAuthorityTokenMeasurement <-
    exportPolicy
      (fixedSupplyPolicy (utxoForAgentFeeAuthorityTokens net), outDirV2 net, "agent-fee-authority-token", True, V2)
  let agentFeeAuthorityCurrencySymbol = fromString @CurrencySymbol agentFeeAuthorityTokenMeasurement.symbol
      agentFeeAuthorityToken = assetClass agentFeeAuthorityCurrencySymbol C.agentFeeAuthorityTokenName
  putStrLn $ "agent fee authority token currency symbol is " ++ show agentFeeAuthorityToken

  -- determine fee authority token symbol
  putStrLn $ "Creating fee authority token policy using utxo: " ++ show (utxoForFeeAuthorityTokens net)
  feeAuthorityTokenMeasurement <-
    exportPolicy
      (fixedSupplyPolicy (utxoForFeeAuthorityTokens net), outDirV2 net, "fee-authority-token", True, V2)
  let feeAuthorityCurrencySymbol = fromString @CurrencySymbol feeAuthorityTokenMeasurement.symbol
      feeAuthorityToken = assetClass feeAuthorityCurrencySymbol C.feeAuthorityTokenName
  putStrLn $ "fee authority token currency symbol is " ++ show feeAuthorityToken

  -- determine staking agent token symbol
  putStrLn $ "Creating staking agent token policy using utxo: " ++ show (utxoForStakingAgentTokens net)
  stakingAgentTokenMeasurement <- case bootstrapMode of
    False ->
      exportPolicy
        (fixedSupplyPolicy (utxoForStakingAgentTokens net), outDirShared net, "staking-agent-token", True, V2)
    True -> hardcodedStakingAgentTokenMeasurement net
  let stakingAgentCurrencySymbol = fromString @CurrencySymbol stakingAgentTokenMeasurement.symbol
      stakingAgentToken = assetClass stakingAgentCurrencySymbol C.stakingAgentTokenName
  putStrLn $ "staking agent token currency symbol is " ++ show stakingAgentToken

  -- determine treasury vote symbol
  putStrLn $ "Creating treasury vote token policy using utxo: " ++ show (utxoForTreasuryVoteTokens net)
  treasuryTokenMeasurement <- case bootstrapMode of
    False ->
      exportPolicy
        (fixedSupplyPolicy (utxoForTreasuryVoteTokens net), outDirShared net, "treasury-vote-token", True, V2)
    True -> hardcodedTreasuryTokenMeasurement net
  let treasuryCurrencySymbol = fromString @CurrencySymbol treasuryTokenMeasurement.symbol
      treasuryToken = assetClass treasuryCurrencySymbol C.treasuryVoteTokenName
  putStrLn $ "treasury vote token currency symbol is " ++ show treasuryToken

  -- freeze staking reward policy
  putStrLn "Freezing staking rewards policy based on given staking agent token"
  stakingRewardsTokenMeasurement <- case bootstrapMode of
    False ->
      exportPolicy
        (stakingRewardsTokenPolicy stakingAgentToken, outDirShared net, "staking-rewards-token", True, V2)
    True -> hardcodedStakingRewardsTokenMeasurement net
  let stakingRewardsCurrencySymbol = fromString @CurrencySymbol stakingRewardsTokenMeasurement.symbol
  putStrLn $ "staking rewards currency symbol is " ++ show stakingRewardsCurrencySymbol
  print stakingRewardsTokenMeasurement

  let validityConfig =
        ValidityConfig
          { starter = dexUniqueRef net
          }

  -- freeze lp tokens currency symbol
  putStrLn "Freezing lp tokens policy"
  poolTokensMeasurement <- exportPolicy (validityMintingPolicy validityConfig, outDirV2 net, "pool-tokens", True, V2)
  let poolTokensCurrencySymbol = fromString @CurrencySymbol poolTokensMeasurement.symbol
  putStrLn $ "lp tokens currency symbol is " ++ show poolTokensCurrencySymbol
  putStrLn ""

  let dexConstants =
        DexConstants
          { initialAgentFeeAda = C.agentFeeAda
          , initialSwapFeeInBasisConstantProduct = C.swapFeeInBasis
          , initialSwapFeeInBasisStableswap = C.stableswapSwapFeeInBasis
          , initialProtocolFeeInBasisConstantProduct = C.protocolFeeInBasis
          , initialProtocolFeeInBasisStableswap = C.stableswapProtocolFeeInBasis
          , initialProjectFeeInBasis = C.projectFeeInBasis
          , initialReserveFeeInBasis = C.reserveFeeInBasis
          , initialFeeBasis = C.feeBasis
          , maxShareTokens = show @Integer C.maxShareTokens
          , burnedShareTokens = C.burnedShareTokens
          , poolOilAda = C.poolOilAda
          , minRequestOilAda = C.requestOilAda
          , maxTxValidityRangeSize = C.maxTxValidityRangeSize
          , minTimeToConsiderPoolAbandoned = C.minTimeToConsiderPoolAbandoned
          }
      params =
        DexParameters
          { agentToken = agentToken
          , factoryToken = assetClass poolTokensCurrencySymbol C.factoryTokenName
          , lpValidityToken = assetClass poolTokensCurrencySymbol C.lpValidityTokenName
          , agentFeeAuthorityToken = agentFeeAuthorityToken
          , feeAuthorityToken = feeAuthorityToken
          }
      treasuryHolderParams =
        TreasuryHolderParameters
          { voteTokenClass = treasuryToken
          , totalVoteCount = treasuryTotalVoteCount net
          , minVoteCount = treasuryMinVoteCount net
          }

  let poolConfig =
        PoolConfig
          { dexSymbol = poolTokensCurrencySymbol
          , agentToken = C.agentTokenName
          , agentSymbol = agentCurrencySymbol
          , treasuryHolderValidatorHash = treasuryHolderScriptValidatorHash treasuryHolderParams
          , stakingRewardsSymbol = stakingRewardsCurrencySymbol
          , feeAuthoritySymbol = feeAuthorityCurrencySymbol
          , feeAuthorityToken = C.feeAuthorityTokenName
          , agentFeeAuthoritySymbol = agentFeeAuthorityCurrencySymbol
          , agentFeeAuthorityToken = C.agentFeeAuthorityTokenName
          }

  -- freeze treasury script
  putStrLn "Freezing & exporting Factory validator script"
  treasuryMeasurement <-
    exportValidator
      (treasuryHolderValidator treasuryHolderParams, outDirV2 net, "treasury", True, V2)
  print treasuryMeasurement
  putStrLn ""

  -- freeze constant product pool script
  putStrLn "Freezing & exporting Constant Product Pool validator script"
  constantProductPoolMeasurement <-
    exportValidator
      (poolScriptValidator @'ConstantProduct poolConfig, outDirV2 net, "pool-constant-product", True, V2)
  print constantProductPoolMeasurement
  putStrLn ""

  -- freeze stableswap pool script
  putStrLn "Freezing & exporting Stableswap Pool validator script"
  stableswapPoolMeasurement <-
    exportValidator
      (poolScriptValidator @'Stableswap poolConfig, outDirV2 net, "pool-stableswap", True, V2)
  print stableswapPoolMeasurement
  putStrLn ""

  -- freeze constant product request script
  putStrLn "Freezing & exporting Constant Product Request validator script"
  requestConstantProductMeasurement <-
    exportValidator (requestScriptValidator (poolScriptValidatorHash @'ConstantProduct poolConfig), outDirV2 net, "request-constant-product", True, V2)
  print requestConstantProductMeasurement
  putStrLn ""

  -- freeze stableswap request script
  putStrLn "Freezing & exporting Stableswap Request validator script"
  requestStableswapMeasurement <-
    exportValidator (requestScriptValidator (poolScriptValidatorHash @'Stableswap poolConfig), outDirV2 net, "request-stableswap", True, V2)
  print requestStableswapMeasurement
  putStrLn ""

  let factoryConfig =
        FactoryConfig
          { poolValidatorHashConstantProduct = poolScriptValidatorHash @'ConstantProduct poolConfig
          , requestValidatorHashConstantProduct = requestScriptValidatorHash (poolScriptValidatorHash @'ConstantProduct poolConfig)
          , poolValidatorHashStableswap = poolScriptValidatorHash @'Stableswap poolConfig
          , requestValidatorHashStableswap = requestScriptValidatorHash (poolScriptValidatorHash @'Stableswap poolConfig)
          , dexSymbol = poolTokensCurrencySymbol
          }

  -- freeze factory script
  putStrLn "Freezing & exporting Factory validator script"
  factoryMeasurement <- exportValidator (factoryValidator factoryConfig, outDirV2 net, "factory", True, V2)
  print factoryMeasurement
  putStrLn ""

  let exportInfo =
        ExportInfo
          { contractParameters = params
          , constants = dexConstants
          , periphery =
              PeripheryInfo
                { treasuryHolderParameters = treasuryHolderParams
                }
          , validators =
              [ constantProductPoolMeasurement
              , stableswapPoolMeasurement
              , requestConstantProductMeasurement
              , requestStableswapMeasurement
              , factoryMeasurement
              , treasuryMeasurement
              ]
          , policies =
              [ agentTokenMeasurement
              , agentFeeAuthorityTokenMeasurement
              , feeAuthorityTokenMeasurement
              , poolTokensMeasurement
              ]
          }
      exportInfoShared =
        ExportInfoShared
          { periphery =
              PeripheryInfoShared
                { stakingAgentToken = stakingAgentToken
                , treasuryVoteToken = treasuryToken
                }
          , policies =
              [ treasuryTokenMeasurement
              , stakingAgentTokenMeasurement
              , stakingRewardsTokenMeasurement
              ]
          }
  createDirectoryIfMissing True (outDirShared net)
  _ <- writeJSON (outDirV2 net <> "export-info.json") exportInfo
  _ <- writeJSON (outDirShared net <> "export-info.json") exportInfoShared
  putStrLn "that's all, enjoy! export info can be found in export-info.json"
  return ()
