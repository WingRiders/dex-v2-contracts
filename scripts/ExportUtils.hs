{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
  Utility functions for Export scripts
-}
module ExportUtils where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (
  catchE,
  runExceptT,
 )
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.Aeson.TH (
  Options (..),
  defaultOptions,
  deriveJSON,
 )
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as ByteString.Base16
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.ByteString.Short qualified as ByteString.Short
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text (
  Text,
  unpack,
 )
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Text.IO (writeFile)
import GHC.Generics (Generic)
import Plutarch.Api.V1 qualified
import Plutarch.Api.V2 qualified
import Plutarch.Script (
  Script,
  serialiseScript,
  unScript,
 )
import PlutusLedgerApi.V2
import Ply.Core.Serialize.Script (serializeScriptCborHex)
import System.Directory (createDirectoryIfMissing)
import System.IO (
  IOMode (..),
  hClose,
  hPutStrLn,
  openFile,
 )
import Prelude hiding (writeFile)

-- "address" is returned by the scriptAddress function
-- Docs definition: The address that should be used by a transaction output locked by the given validator script.
data ValidatorMeasurement = ValidatorMeasurement
  { name :: String
  , hash :: String
  , size :: Integer
  }
  deriving (Show, Generic, ToJSON)

-- "policy_symbol" is returned by the mintingPolicySymbol function
data PolicyMeasurement = PolicyMeasurement
  { name :: String
  , symbol :: String
  , size :: Integer
  }
  deriving (Show, Generic, ToJSON)

data Version = V1 | V2
data Network = PrivateTest | Test | Main

scriptToShortBs :: Script -> ByteString.Short.ShortByteString
scriptToShortBs = serialiseUPLC . unScript

writeToJson :: ToJSON a => a -> String -> IO ()
writeToJson val fpath = do
  myFile <- openFile fpath WriteMode
  hPutStrLn myFile (unpack . decodeUtf8 . ByteString.Lazy.toStrict . encodePretty' defConfig {confIndent = Spaces 2} $ val)
  hClose myFile

writeToText :: String -> String -> IO ()
writeToText val fpath = do
  myFile <- openFile fpath WriteMode
  hPutStrLn myFile val
  hClose myFile

scriptAddressV1 :: Script -> Address
scriptAddressV1 = flip Address Nothing . ScriptCredential . Plutarch.Api.V1.scriptHash

scriptAddressV2 :: Script -> Address
scriptAddressV2 = flip Address Nothing . ScriptCredential . Plutarch.Api.V2.scriptHash

data PlutusScriptJSON = PlutusScriptJSON
  { plsj_type :: String
  , plsj_description :: String
  , plsj_cborHex :: String
  }
  deriving stock (Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = \f -> fromMaybe f (stripPrefix "plsj_" f)} ''PlutusScriptJSON)

base16 :: ByteString -> Text
base16 = TE.decodeUtf8 . ByteString.Base16.encode

toScriptJson :: Script -> Version -> PlutusScriptJSON
toScriptJson script ver =
  PlutusScriptJSON
    { plsj_type = case ver of
        V1 -> "PlutusScriptV1"
        V2 -> "PlutusScriptV2"
    , plsj_description = ""
    , plsj_cborHex = unpack (serializeScriptCborHex (serialiseScript script))
    }

writeJSON :: ToJSON a => FilePath -> a -> IO (Either IOError ())
writeJSON path a = runExceptT $ flip catchE id $ lift (writeFile path content)
  where
    content = decodeUtf8 $ ByteString.Lazy.toStrict $ encodePretty' defConfig {confIndent = Spaces 2, confTrailingNewline = True} a

measurePlutusValidator :: (Script, String, String, Bool, Version) -> IO ValidatorMeasurement
measurePlutusValidator (val, outputDir, name, write, ver) = do
  let hash = case ver of
        V1 -> splitOn " " (show $ addressCredential (scriptAddressV1 val)) !! 1
        V2 -> splitOn " " (show $ addressCredential (scriptAddressV2 val)) !! 1
      scriptSerial = toScriptJson val ver
      size = toInteger (ByteString.Short.length (scriptToShortBs val))

  case write of
    True -> do
      createDirectoryIfMissing True outputDir
      result <- writeJSON (outputDir <> name <> ".plutus.json") scriptSerial
      case result of
        Left _ -> error "writeJSON failed"
        Right () -> pure ()
    False -> pure ()

  return $ ValidatorMeasurement name hash size

measurePlutusPolicy :: (Script, String, String, Bool, Version) -> IO PolicyMeasurement
measurePlutusPolicy (pol, outputDir, name, write, ver) = do
  let currencySymbol = case ver of
        V1 -> CurrencySymbol $ getScriptHash $ Plutarch.Api.V1.scriptHash pol
        V2 -> CurrencySymbol $ getScriptHash $ Plutarch.Api.V2.scriptHash pol
      scriptSerial = toScriptJson pol ver
      size = toInteger (ByteString.Short.length (scriptToShortBs pol))

  case write of
    True -> do
      createDirectoryIfMissing True outputDir
      result <- writeJSON (outputDir <> name <> ".plutus.json") scriptSerial
      case result of
        Left _ -> error "writeFileTextEnvelope failed"
        Right () -> pure ()
    False -> pure ()

  return $ PolicyMeasurement name (show currencySymbol) size

exportAllValidators :: [(Script, String, String, Bool, Version)] -> IO [ValidatorMeasurement]
exportAllValidators [] = pure []
exportAllValidators (val : xs) = do
  measurement <- measurePlutusValidator val
  other <- exportAllValidators xs
  return $ measurement : other

exportValidator :: (Script, String, String, Bool, Version) -> IO ValidatorMeasurement
exportValidator val = measurePlutusValidator val

exportAllPolicies :: [(Script, String, String, Bool, Version)] -> IO [PolicyMeasurement]
exportAllPolicies [] = pure []
exportAllPolicies (val : xs) = do
  measurement <- measurePlutusPolicy val
  other <- exportAllPolicies xs
  return $ measurement : other

exportPolicy :: (Script, String, String, Bool, Version) -> IO PolicyMeasurement
exportPolicy val = measurePlutusPolicy val
