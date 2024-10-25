module Plutarch.PlutusScript where

import Data.Char (toLower)
import Data.Text (unpack)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Plutarch (
  ClosedTerm,
  Config (..),
  TracingMode (..),
  compile,
 )
import Plutarch.Script (Script)
import System.Environment (lookupEnv)

defaultConfig :: Config
defaultConfig = Config tracingMode
  where
    tracingMode = maybe NoTracing (\s -> if map toLower s == "true" then DoTracing else NoTracing) tracingModeEnv
    tracingModeEnv = unsafePerformIO $ lookupEnv "CONTRACTS_TRACING"

-- | Converts a Plutarch script to Plutus script
toScript :: HasCallStack => ClosedTerm a -> Script
toScript script = either (error . unpack) id $ compile defaultConfig script
