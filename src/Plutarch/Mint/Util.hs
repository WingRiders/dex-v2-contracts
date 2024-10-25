module Plutarch.Mint.Util where

import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (
  AmountGuarantees (NoGuarantees),
  KeyGuarantees (..),
  PCurrencySymbol,
  PMap (..),
  PScriptHash (..),
  PTokenName,
  PTxInInfo,
  PValue (..),
 )
import Plutarch.Maybe (pfromJust)
import Plutarch.Prelude
import Plutarch.Types.Classes
import Plutarch.Util

-- | Checks that there is minted exact amount of token and no other for that currency symbol
pisMintingExactAmountForPolicy ::
  Term s (PInteger :--> PCurrencySymbol :--> PTokenName :--> (PValue 'Sorted 'NoGuarantees) :--> PBool)
pisMintingExactAmountForPolicy = phoistAcyclic $ plam $ \amount currencySymbol tokenName mintedValue -> unTermCont $ do
  let matchAmountTokenName = pisKeyValueEqualPair # tokenName # amount
      mapToList = pto
      matchList = pisMatchingSingleton matchAmountTokenName
      matchMap = matchList . mapToList
  return $ psatisfiesForMintingPolicy matchMap currencySymbol mintedValue

pisMatchingSingleton :: PIsListLike list a => Term s (a :--> PBool) -> Term s (list a) -> Term s PBool
pisMatchingSingleton matchValue list = pelimList (pisMatchingHeadAndEmptyTail matchValue) (pconstant False) list

pisMatchingDoubleton ::
  PIsListLike list a => Term s (a :--> PBool) -> Term s (a :--> PBool) -> Term s (list a) -> Term s PBool
pisMatchingDoubleton matchValue1 matchValue2 list =
  pelimList (pisMatchingHeadAndSingletonTail matchValue1 matchValue2) (pconstant False) list

pisMatchingHeadAndEmptyTail ::
  PIsListLike list a => Term s (a :--> PBool) -> Term s a -> Term s (list a) -> Term s PBool
pisMatchingHeadAndEmptyTail matchValue matchingHead emptyTail = (pnull # emptyTail) #&& (matchValue # matchingHead)

pisMatchingHeadAndSingletonTail ::
  PIsListLike list a => Term s (a :--> PBool) -> Term s (a :--> PBool) -> Term s a -> Term s (list a) -> Term s PBool
pisMatchingHeadAndSingletonTail matchValue1 matchValue2 matchingHead singletonTail =
  (pisMatchingSingleton matchValue2 singletonTail) #&& (matchValue1 # matchingHead)

pisKeyValueEqualPair :: (PIsData k, PEq k, PIsData v, PEq v) => Term s (k :--> v :--> PBuiltinDataPair k v :--> PBool)
pisKeyValueEqualPair = phoistAcyclic $ plam $ \key value pair -> unTermCont $ do
  let pairKey = pfromData $ pfstBuiltin # pair
      pairValue = pfromData $ psndBuiltin # pair
  return $ (pairKey #== key) #&& (pairValue #== value)

-- | Extract the part of the value associated with the given currency symbol
pvalueOfCurrency :: Term s (PValue sort guarantees) -> Term s PCurrencySymbol -> Term s (PMap sort PTokenName PInteger)
pvalueOfCurrency mintedValue currencySymbol = pfromJust #$ plookup # currencySymbol # pto mintedValue

-- | Extract the part of the value associated with the given currency symbol or Nothing
plookupCurrency :: Term s (PValue sort guarantees) -> Term s PCurrencySymbol -> Term s (PMaybe (PMap sort PTokenName PInteger))
plookupCurrency mintedValue currencySymbol = plookup # currencySymbol # pto mintedValue

{- | Extract the token name and the amount of the given currency symbol.
Throws when the token name is not found or more than one token name is involved
-}
pvalueOfSingleton :: Term s (PValue 'Sorted 'NoGuarantees) -> Term s PCurrencySymbol -> Term s (PPair PTokenName PInteger)
pvalueOfSingleton mintedValue currencySymbol =
  toScott $ passertSingleton "the minted token doesn't have only one token name involved" # pto (pto (pvalueOfCurrency mintedValue currencySymbol))

{- | Extract the token name and the amount of the given currency symbol.
Throws when the token name is not found or more than one token name is involved
Plutarch level function.
-}
pvalueOfSingleton' ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> PValue keys amounts :--> PPair PTokenName PInteger)
pvalueOfSingleton' = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      ( pfix #$ plam $ \recur ys ->
          pelimList
            ( \x xs ->
                pif
                  (pfromData (pfstBuiltin # x) #== policyId)
                  ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                      plet (passertSingleton "the minted token doesn't have only one token name involved" # tokens) $ \tkPair ->
                        pcon (PPair (pfromData (pfstBuiltin # tkPair)) (pfromData (psndBuiltin # tkPair)))
                  )
                  (recur # xs)
            )
            perror
            ys
      )
        # pto val'

ptokenNameAsScriptHash :: Term s PTokenName -> Term s PScriptHash
ptokenNameAsScriptHash tn = pcon (PScriptHash (pto tn))

pdelegateToAssociatedValidator :: Term s PTokenName -> Term s (PBuiltinList PTxInInfo) -> Term s PBool
pdelegateToAssociatedValidator tn inputs =
  pany # plam (\input -> ppaysToCredential # ptokenNameAsScriptHash tn # (ptxInInfoResolved # input)) # inputs

psatisfiesForMintingPolicy ::
  (Term s (PMap 'Sorted PTokenName PInteger) -> Term s PBool) ->
  Term s PCurrencySymbol ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PBool
psatisfiesForMintingPolicy predicate currencySymbol mintedValue =
  AssocMap.pfoldAt # currencySymbol # pconstant False # (plam $ predicate . pfromData) # pto mintedValue
