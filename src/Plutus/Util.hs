module Plutus.Util where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Plutarch ((#))
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Lift (
  pconstant,
  plift,
 )
import PlutusLedgerApi.V1.Value (
  AssetClass,
  adaSymbol,
  adaToken,
  assetClass,
 )
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  DatumHash (..),
  ScriptHash,
 )
import PlutusTx qualified as PlutusTx
import PlutusTx.Builtins qualified as PlutusTx (
  toBuiltin,
 )

toDatumHash :: PlutusTx.ToData a => a -> DatumHash
toDatumHash datum =
  DatumHash $
    PlutusTx.toBuiltin $
      plift $
        pblake2b_256
          # pconstant
            (ByteString.Lazy.toStrict $ serialise $ PlutusTx.toData datum)

scriptHashToAddress :: ScriptHash -> Address
scriptHashToAddress vh = Address (ScriptCredential vh) Nothing

-- | NOTE: doesn't work for negative numbers because of the mod
divideCeil :: Integer -> Integer -> Integer
divideCeil a b = div a b + if (mod a b > 0) then 1 else 0

adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken
