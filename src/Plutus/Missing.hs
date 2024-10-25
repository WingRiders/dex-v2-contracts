{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plutus.Missing where

import Data.Aeson as JSON (
  ToJSON (..),
  Value (..),
  object,
  (.=),
 )
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as ByteString.Base16
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as T
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  CurrencySymbol (..),
  DatumHash (..),
  Map,
  ScriptHash (..),
  TokenName (..),
 )
import PlutusTx (Data (..))
import PlutusTx.Prelude qualified as PlutusTx

-- orphan instances

fromTokenName :: (ByteString -> r) -> (Text -> r) -> TokenName -> r
fromTokenName handleBytestring handleText (TokenName bs) =
  either (\_ -> handleBytestring $ PlutusTx.fromBuiltin bs) handleText $ T.decodeUtf8' (PlutusTx.fromBuiltin bs)

asBase16 :: ByteString -> Text
asBase16 bs = Text.concat ["0x", T.decodeUtf8 $ ByteString.Base16.encode bs]

instance ToJSON TokenName where
  toJSON tn =
    JSON.object
      [
        ( "unTokenName"
        , JSON.toJSON $
            fromTokenName
              (\bs -> Text.cons '\NUL' (asBase16 bs))
              ( \t -> case Text.take 1 t of
                  "\NUL" -> Text.concat ["\NUL\NUL", t]
                  _ -> t
              )
              tn
        )
      ]

instance ToJSON CurrencySymbol where
  toJSON c =
    JSON.object
      [("unCurrencySymbol", JSON.String . T.decodeUtf8 . ByteString.Base16.encode . PlutusTx.fromBuiltin . unCurrencySymbol $ c)]

instance ToJSON BuiltinByteString where
  toJSON b = JSON.String . T.decodeUtf8 . ByteString.Base16.encode . PlutusTx.fromBuiltin $ b

deriving anyclass instance ToJSON AssetClass
deriving anyclass instance ToJSON DatumHash
deriving anyclass instance ToJSON ScriptHash
deriving anyclass instance (ToJSON k, ToJSON v) => ToJSON (Map k v)

-- https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md#detailed-schema
instance ToJSON Data where
  toJSON = \case
    Constr i data' -> object ["constructor" .= i, "fields" .= data']
    Map pairs -> object ["map" .= map (\(k, v) -> object ["k" .= k, "v" .= v]) pairs]
    List datas -> object ["list" .= datas]
    I i -> object ["int" .= i]
    B bs -> object ["bytes" .= (T.decodeUtf8 . ByteString.Base16.encode) bs]
