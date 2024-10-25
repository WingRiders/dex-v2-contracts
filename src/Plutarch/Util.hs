{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plutarch.Util (
  (#.),
  (#/=),
  (#>),
  (#>=),
  PBuiltinDataPair,
  minutes,
  p2elemsAt,
  paddressPubKeyCredential,
  pand'List,
  passertDoubleton,
  passertPositive,
  passertSingleSpecificInput,
  passertSingleton,
  pbetween,
  pbuiltinPair,
  pbuiltinPairLess,
  pcond,
  pcountAllScriptInputs,
  pcountOfUniqueTokens,
  pcountOfUniqueTokensWithOverlap,
  pcountScriptInputs,
  pcountScriptOutputs,
  pdivideCeil,
  pdjust,
  pdnothing,
  pdrop',
  pelemAtOptimized,
  perrorIfFalse,
  pextractNthInputValidatorHash,
  pfalse,
  pfindScriptOutputs,
  pfindScriptOutputsWithAddress,
  pfiniteTxValidityRangeTimestamps,
  pfoldInputsWithTwoOutputs,
  pfromDJust,
  pfst,
  pgetValidatorHashFromScriptAddress,
  phashDatum,
  phaveSameStakingCredentials,
  pid,
  pisAda,
  pisDJust,
  pisDNothing,
  pisEqualScriptHashAddress,
  pisJust,
  pisNothing,
  pisPubKeyAddress,
  pisSignedByPubKeyAddress,
  pisStakePtrAddress,
  pisTokenSpent,
  pisTxValidityRangeShortEnough,
  pjust,
  plowerBoundCurrentTimeApproximation,
  pmapToPList,
  pmin,
  pmustFindOwnInput,
  pnothing,
  pownCurrencySymbol,
  pownInput,
  pownRef,
  ppair,
  ppaysToCredential,
  pquartet,
  psnd,
  ptriplet,
  ptrue,
  ptryLookup,
  ptryTxOutRefRedeemer,
  ptryUniqueScriptTxInInfo,
  ptryUniqueScriptTxOut,
  ptxInInfoResolved,
  ptxOutAddress,
  ptxOutDatum,
  ptxOutHasAssociatedToken,
  ptxOutValue,
  ptxSignedByPkh,
  punit,
  pvalueContainsToken,
  pvalueIsSubsetOf,
  pvalueOfInputs,
  pvalueOfWithOilCheck,
  pfoldl2AllowMore,
) where

import DEX.Constants (maxTxValidityRangeSize)
import Data.Text (Text)
import Plutarch
import Plutarch.Api.V1 (PCredential (..), PRedeemer)
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pvalueOf)
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr
import Plutarch.DataRepr.Internal.FromData (PFromDataable)
import Plutarch.Extra.ScriptContext (pscriptHashFromAddress, pscriptHashToTokenName)
import Plutarch.Extra.TermCont
import Plutarch.Lift
import Plutarch.List
import Plutarch.Maybe
import Plutarch.Prelude (
  PByteString,
  PInteger,
  PIntegral (pdiv, pmod),
  PPair (..),
  PUnit (..),
  ptraceError,
 )
import Plutarch.Types.Base
import Plutarch.Types.Classes

type PBuiltinDataPair a b = PBuiltinPair (PAsData a) (PAsData b)

(#>) :: PPartialOrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a

infix 4 #>

(#>=) :: (PPartialOrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=

{- | Plutarch function composition.
 Note that it introduces an additional lambda and is not free.
-}
(#.) :: Term s (b :--> c) -> Term s (a :--> b) -> Term s (a :--> c)
f #. g = plam (\x -> f # (g # x))

infixr 7 #.

(#/=) :: PEq t => Term s t -> Term s t -> Term s PBool
a #/= b = pnot # (a #== b)

infix 4 #/=

{- | Expand the given list of conditions with pand'.
Evaluates the arguments strictly.
-}
pand'List :: [Term s PBool] -> Term s PBool
pand'List = foldr1 (\res x -> pand' # res # x)

{- | Build nested conditions. It takes an association list of conditions and
and results. It evaluates the conditions in order: whenever a condition
is satisfied, its associated result is returned.
Expands to a nested `pif` structure.
-}
pcond ::
  [(Term s PBool, Term s a)] ->
  Term s a ->
  Term s a
pcond [] def = def
pcond ((cond, x) : conds) def = pif cond x $ pcond conds def

pdnothing :: Term s (PMaybeData a)
pdnothing = pcon (PDNothing pdnil)

pdjust :: PIsData a => Term s a -> Term s (PMaybeData a)
pdjust a = pcon (PDJust (pdcons # pdata a # pdnil))

pnothing :: Term s (PMaybe a)
pnothing = pcon PNothing

pjust :: Term s a -> Term s (PMaybe a)
pjust a = pcon (PJust a)

punit :: Term s PUnit
punit = pcon PUnit

ppair :: Term s a -> Term s b -> Term s (PPair a b)
ppair a b = pcon (PPair a b)

ptriplet :: Term s a -> Term s b -> Term s c -> Term s (PTriplet a b c)
ptriplet a b c = pcon (PTriplet a b c)

pquartet :: Term s a -> Term s b -> Term s c -> Term s d -> Term s (PQuartet a b c d)
pquartet a b c d = pcon (PQuartet a b c d)

pmapToPList ::
  (PElemConstraint PBuiltinList a, PElemConstraint PList b) =>
  Term s ((a :--> b) :--> PBuiltinList a :--> PList b)
pmapToPList = phoistAcyclic $ plam \f l ->
  precList (\recur x xs -> pcons # (f # x) # (recur # xs)) (const pnil) # l

pbuiltinPair :: (PIsData a, PIsData b) => Term s a -> Term s b -> Term s (PBuiltinPair (PAsData a) (PAsData b))
pbuiltinPair a b = ppairDataBuiltin # pdata a # pdata b

pbuiltinPairLess ::
  (POrd a, POrd b, PIsData a, PIsData b) =>
  Term s (PBuiltinPair (PAsData a) (PAsData b) :--> PBuiltinPair (PAsData a) (PAsData b) :--> PBool)
pbuiltinPairLess = phoistAcyclic $ plam $ \a b ->
  plet (pfromData (pfstBuiltin # a)) \a1 -> plet (pfromData (pfstBuiltin # b)) \b1 ->
    let a2 = pfromData (psndBuiltin # a)
        b2 = pfromData (psndBuiltin # b)
     in pcond
          [ (a1 #< b1, ptrue)
          , (a1 #> b1, pfalse)
          ]
          (a2 #< b2)

ptrue :: Term s PBool
ptrue = pconstant True

pfalse :: Term s PBool
pfalse = pconstant False

-- | Identity function
pid :: Term s (a :--> a)
pid = plam $ \x -> x

-- | NOTE: errors out on equal indices
p2elemsAt :: PIsListLike l a => Term s (PInteger :--> PInteger :--> l a :--> PPair a a)
p2elemsAt = phoistAcyclic $ plam \i j l ->
  pif (por' # (i #< 0) # (j #< 0)) perror (p2elemsAt' # i # j # l)

p2elemsAt' :: PIsListLike l a => Term s (PInteger :--> PInteger :--> l a :--> PPair a a)
p2elemsAt' = phoistAcyclic $ (pfix # plam go) # ppair pnothing pnothing # 0
  where
    go recur accPair index i j l =
      pmatch accPair $ \(PPair a b) -> unTermCont $ do
        pure
          ( pif
              (pisJust # a #&& pisJust # b)
              (ppair (pfromJust # a) (pfromJust # b))
              ( pelimList
                  ( \h t ->
                      ( pcond
                          [ (index #== i, recur # ppair (pjust h) b # (index + 1) # i # j # t)
                          , (index #== j, recur # ppair a (pjust h) # (index + 1) # i # j # t)
                          ]
                          (recur # accPair # (index + 1) # i # j # t)
                      )
                  )
                  perror
                  l
              )
          )

perrorIfFalse :: Term s (PBool :--> PUnit)
perrorIfFalse = phoistAcyclic $ plam $ \b -> pif b (pconstant ()) perror

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

ptxSignedByPkh ::
  (PIsListLike list (PAsData PPubKeyHash)) => Term s (PAsData PPubKeyHash :--> list (PAsData PPubKeyHash) :--> PBool)
ptxSignedByPkh = pelem

passertPositive :: Term s (PInteger :--> PInteger)
passertPositive = phoistAcyclic $ plam $ \number -> pif (number #<= 0) (ptraceError "U1") number

-- | NOTE: doesn't work for negative numbers because of the mod
pdivideCeil :: Term s (PInteger :--> PInteger :--> PInteger)
pdivideCeil = phoistAcyclic $ plam $ \a b -> (pdiv # a # b) + pif ((pmod # a # b) #> 0) 1 0

-- | Unpack the head of the list if it's the only element
passertSingleton :: PIsListLike list a => Text -> Term s (list a :--> a)
passertSingleton e = phoistAcyclic $ plam $ \l -> pelimList (pelimList (\_ _ -> (ptraceError (pconstant e)))) (ptraceError (pconstant e)) l

-- | Extract the only two list elements, throw if the length of the list isn't 2.
passertDoubleton :: (PElemConstraint list a, PListLike list) => Term s (list a) -> Term s (PPair a a)
passertDoubleton =
  pelimList
    ( \first ->
        pelimList
          (\second -> pelimList (const . const $ ptraceError "U2") (pcon $ PPair first second))
          (ptraceError "U3")
    )
    (ptraceError "U4")

passertSingleSpecificInput ::
  PIsListLike list a =>
  Text ->
  Term s ((a :--> PTxOut) :--> PScriptHash :--> PCurrencySymbol :--> PTokenName :--> list a :--> PTxOut)
passertSingleSpecificInput e = phoistAcyclic $ plam $ \toTxOut vh cs tn l ->
  toTxOut
    #$ passertSingleton e
    #$ pfilter
    # ( plam
          ( \o' -> plet (toTxOut # o') $ \o ->
              (ppaysToCredential # vh # o) #&& (pvalueOf # (pfield @"value" # o) # cs # tn #== 1)
          )
      )
    # l

pvalueOfInputs :: Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinList PTxInInfo :--> PInteger)
pvalueOfInputs = phoistAcyclic $ plam $ \cs tn inputs ->
  let acc +-+ input = acc + pvalueOf # (pfield @"value" # (pfield @"resolved" # input)) # cs # tn
   in pfoldl # plam (+-+) # 0 # inputs

phashDatum ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  Term s (a :--> PDatumHash)
phashDatum =
  phoistAcyclic $
    plam $
      pcon
        . PDatumHash
        . (pblake2b_256 #)
        . (pserialiseData #)
        . pforgetData
        . pdata

pgetInput :: (PFromDataable PTxOut b, PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> b)
pgetInput = phoistAcyclic $ plam $ \txInputs oref ->
  precList
    ( \recur txIn txIns -> unTermCont $ do
        PTxInInfo txIn' <- pmatchC txIn
        txIn'' <- pletFieldsC @'["outRef", "resolved"] txIn'
        let oref' = txIn''.outRef
        let txOut = txIn''.resolved
        return $ pif (oref #== oref') txOut (recur # txIns)
    )
    (const $ ptraceError "U5")
    # txInputs

ptxOutAddress :: PFromDataable PAddress b => Term s (PTxOut :--> b)
ptxOutAddress = phoistAcyclic $ plam $ \txOut -> pfield @"address" # txOut

pisEqualScriptHashAddress :: Term s (PScriptHash :--> PAddress :--> PBool)
pisEqualScriptHashAddress = phoistAcyclic $ plam $ \sHash addr -> pmatch (pfield @"credential" # addr) $ \case
  PScriptCredential vh -> pfield @"_0" # vh #== sHash
  PPubKeyCredential _ -> pfalse

pgetValidatorHashFromScriptAddress :: PFromDataable PScriptHash b => Term s (PAddress :--> b)
pgetValidatorHashFromScriptAddress = phoistAcyclic $ plam $ \addr -> pmatch (pfield @"credential" # addr) $ \case
  PScriptCredential vh -> pfield @"_0" # vh
  PPubKeyCredential _ -> ptraceError "U6"

-- Inlined as it's called once
pownRef :: Term s PScriptPurpose -> Term s PTxOutRef
pownRef purpose = pmatch purpose $ \case
  PSpending oref -> pfield @"_0" # oref
  _ -> ptraceError "U7"

-- Inlined as it's called once
pownCurrencySymbol :: Term s PScriptPurpose -> Term s PCurrencySymbol
pownCurrencySymbol purpose = pmatch purpose $ \case
  PMinting cs -> pfield @"_0" # cs
  _ -> ptraceError "U8"

pownInput :: Term s (PScriptPurpose :--> PBuiltinList PTxInInfo :--> PTxOut)
pownInput = phoistAcyclic $ plam $ \purpose txInputs -> pmatch purpose $ \case
  PSpending oref -> pgetInput # txInputs # (pfield @"_0" # oref)
  _ -> ptraceError "U9"

ptxInInfoResolved :: PFromDataable PTxOut b => Term s (PTxInInfo :--> b)
ptxInInfoResolved = phoistAcyclic $ plam $ \txInInfo -> pfield @"resolved" # txInInfo

-- There are no tokens with the same currency symbol as ada, that means we can skip checking the token name
pisAda :: Term s (PCurrencySymbol :--> PBool)
pisAda = phoistAcyclic $ plam $ (#== padaSymbol)

-- Check number of unique tokens, taking into account that ada may overlap with provided token
pcountOfUniqueTokensWithOverlap :: Term s PCurrencySymbol -> Term s (PValue 'Sorted 'Positive) -> Term s PInteger
pcountOfUniqueTokensWithOverlap overlapSymbol value =
  (pcountOfUniqueTokens # value) + pif (pisAda # overlapSymbol) 1 0

pcountOfUniqueTokens ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens = phoistAcyclic $
  plam $ \val ->
    let tokensLength = plam (\pair -> pmatch (pfromData $ psndBuiltin # pair) $ \(PMap tokens) -> plength # tokens)
     in pmatch val $ \(PValue val') ->
          pmatch val' $ \(PMap csPairs) -> pfoldl # plam (\acc x -> acc + (tokensLength # x)) # 0 # csPairs

pcountAllScriptInputs :: Term s (PBuiltinList PTxInInfo :--> PInteger)
pcountAllScriptInputs = phoistAcyclic $ plam $ \inputs ->
  pfoldl
    # plam
      ( \acc input ->
          pmatch (pfield @"credential" # (ptxOutAddress # (ptxInInfoResolved # input))) $ \case
            PScriptCredential _ -> acc + 1
            _ -> acc
      )
    # 0
    # inputs

pcountScriptInputs :: Term s (PScriptHash :--> PBuiltinList PTxInInfo :--> PInteger)
pcountScriptInputs = phoistAcyclic $ plam \sHash inputs ->
  pfoldl
    # plam
      ( \acc input ->
          pif
            (pscriptHashFromAddress # (ptxOutAddress # (ptxInInfoResolved # input)) #== pjust sHash)
            (acc + 1)
            acc
      )
    # 0
    # inputs

pcountScriptOutputs :: Term s (PScriptHash :--> PBuiltinList PTxOut :--> PInteger)
pcountScriptOutputs = phoistAcyclic $ plam \sHash outputs ->
  pfoldl
    # plam
      ( \acc output ->
          pif
            (pscriptHashFromAddress # (ptxOutAddress # output) #== pjust sHash)
            (acc + 1)
            acc
      )
    # 0
    # outputs

ptryUniqueScriptTxOut :: Term s PScriptHash -> Term s (PBuiltinList PTxOut) -> Term s PTxOut
ptryUniqueScriptTxOut hash txOuts =
  passertSingleton "U0"
    #$ pfilter
    # plam
      ( \o ->
          ppaysToCredential # hash # o
      )
    # txOuts

ptryUniqueScriptTxInInfo :: Term s PScriptHash -> Term s (PBuiltinList PTxInInfo) -> Term s PTxInInfo
ptryUniqueScriptTxInInfo hash txIns =
  passertSingleton "V1"
    #$ pfilter
    # plam (\i -> ppaysToCredential # hash # (ptxInInfoResolved # i))
    # txIns

-- | Checks whether the passed TxOut has one token of the given minting policy with the token name equal to the TxOut validator hash.
ptxOutHasAssociatedToken :: Term s PCurrencySymbol -> Term s PTxOut -> Term s PBool
ptxOutHasAssociatedToken cs o = pletFields @'["value", "address"] o \out ->
  pvalueOf # out.value # cs # pscriptHashToTokenName (pgetValidatorHashFromScriptAddress # out.address) #== 1

pfindScriptOutputs :: Term s (PScriptHash :--> PBuiltinList PTxOut :--> PList (PPair (PValue 'Sorted 'Positive) POutputDatum))
pfindScriptOutputs = phoistAcyclic $ plam $ \hash txOuts -> pfoldl # (reduction # hash) # pnil # txOuts
  where
    reduction ::
      Term
        s
        ( PScriptHash
            :--> PList (PPair (PValue 'Sorted 'Positive) POutputDatum)
            :--> PTxOut
            :--> PList (PPair (PValue 'Sorted 'Positive) POutputDatum)
        )
    reduction = plam $ \hash' acc txOut -> unTermCont $ do
      o <- pletFieldsC @'["address", "value", "datum"] txOut
      let hashesEqual = pmatch (pfield @"credential" # o.address) \case
            PScriptCredential credential' -> (pfield @"_0" # credential') #== hash'
            PPubKeyCredential _ -> pfalse
      pure $ pif hashesEqual (pcons # ppair o.value o.datum # acc) acc

pfindScriptOutputsWithAddress :: Term s (PScriptHash :--> PBuiltinList PTxOut :--> PList (PTriplet PAddress (PValue 'Sorted 'Positive) POutputDatum))
pfindScriptOutputsWithAddress = phoistAcyclic $ plam $ \hash txOuts -> pfoldl # (reduction # hash) # pnil # txOuts
  where
    reduction ::
      Term
        s
        ( PScriptHash
            :--> PList (PTriplet PAddress (PValue 'Sorted 'Positive) POutputDatum)
            :--> PTxOut
            :--> PList (PTriplet PAddress (PValue 'Sorted 'Positive) POutputDatum)
        )
    reduction = plam $ \hash' acc txOut -> unTermCont $ do
      o <- pletFieldsC @'["address", "value", "datum"] txOut
      let hashesEqual = pmatch (pfield @"credential" # o.address) \case
            PScriptCredential credential' -> (pfield @"_0" # credential') #== hash'
            PPubKeyCredential _ -> pfalse
      pure $ pif hashesEqual (pcons # ptriplet o.address o.value o.datum # acc) acc

{- |
  Based on the transaction validity range, this returns the current time lower bound approximation.
  We know that for any transaction to be valid, cardano nodes check that the current time is between the
  transaction validity start and transaction validity end. That means that to lower bound the current time,
  we need to take the transaction validity start timestamp.
-}
plowerBoundCurrentTimeApproximation :: Term s (PPOSIXTimeRange :--> PPOSIXTime)
plowerBoundCurrentTimeApproximation = phoistAcyclic $ plam $ \interval -> unTermCont $ do
  PInterval i <- pmatchC interval
  PLowerBound b <- pmatchC $ pfield @"from" # i
  let startTimestamp = pmatch
        (pfield @"_0" # b)
        \case
          PFinite s -> s
          _ -> ptraceError "V2"
  pure $ pfield @"_0" # startTimestamp

-- | Look up the given key data in a 'PMap', return the found value in the key-value pair.
ptryLookup :: (PIsData k, PIsData v, PEq k) => Term s (k :--> PMap any k v :--> v)
ptryLookup = phoistAcyclic $ plam $ \k m ->
  precList
    (\recur x xs -> pif (pfromData (pfstBuiltin # x) #== k) (pfromData (psndBuiltin # x)) (recur # xs))
    (\_recur -> ptraceError "V3")
    # pto m

instance ScottConvertible PInteger where
  type ScottOf PInteger = PInteger
  toScott i = i
  fromScott i = i

instance ScottConvertible PPOSIXTime where
  type ScottOf PPOSIXTime = PPOSIXTime
  toScott i = i
  fromScott i = i

instance ScottConvertible PTokenName where
  type ScottOf PTokenName = PTokenName
  toScott i = i
  fromScott i = i

instance ScottConvertible PCurrencySymbol where
  type ScottOf PCurrencySymbol = PCurrencySymbol
  toScott i = i
  fromScott i = i

instance ScottConvertible PByteString where
  type ScottOf PByteString = PByteString
  toScott i = i
  fromScott i = i

instance
  (PIsData a, PIsData b, ScottConvertible a, ScottConvertible b) =>
  ScottConvertible (PBuiltinPair (PAsData a) (PAsData b))
  where
  type ScottOf (PBuiltinPair (PAsData a) (PAsData b)) = PPair (ScottOf a) (ScottOf b)
  toScott p = pcon (PPair (toScott (pfromData (pfstBuiltin # p))) (toScott (pfromData (psndBuiltin # p))))
  fromScott p = pmatch p $ \(PPair a b) -> ppairDataBuiltin # pdata (fromScott a) # pdata (fromScott b)

instance (PIsData a, ScottConvertible a) => ScottConvertible (PBuiltinList (PAsData a)) where
  type ScottOf (PBuiltinList (PAsData a)) = PList (ScottOf a)
  toScott l = precList (\recur x xs -> pcons # toScott (pfromData x) # (recur # xs)) (const pnil) # l
  fromScott l = precList (\recur x xs -> pcons # pdata (fromScott x) # (recur # xs)) (const pnil) # l

instance (PIsData a, ScottConvertible a) => ScottConvertible (PMaybeData (PAsData a)) where
  type ScottOf (PMaybeData (PAsData a)) = PMaybe (ScottOf a)
  toScott m = pmatch m \case
    PDJust r -> pjust (toScott (pfromData (pfield @"_0" # r)))
    PDNothing _ -> pnothing
  fromScott m = pmatch m \case
    PJust r -> pdjust (pdata (fromScott r))
    PNothing -> pdnothing

pforallValue :: Term s ((PCurrencySymbol :--> PTokenName :--> PInteger :--> PBool) :--> PValue 'Sorted 'Positive :--> PBool)
pforallValue = plam $ \f m -> pfoldl # plam (reduction f) # ptrue # pto (pto m)
  where
    reduction f acc p = plet (pfromData $ pfstBuiltin # p) \cs ->
      let m2 = pfromData $ psndBuiltin # p
          subreduction acc' p2 =
            let tn = pfromData $ pfstBuiltin # p2
                am = pfromData $ psndBuiltin # p2
             in acc' #&& (f #cs # tn # am)
       in pfoldl # plam subreduction # acc # pto m2

pvalueIsSubsetOf :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PBool)
pvalueIsSubsetOf = phoistAcyclic $ plam $ \lesser bigger ->
  pforallValue # plam (\cs tn amount -> amount #<= (pvalueOf # bigger # cs # tn)) # lesser

ptxOutValue :: PFromDataable (PValue 'Sorted 'Positive) b => Term s (PTxOut :--> b)
ptxOutValue = phoistAcyclic $ plam $ \txOut -> pfield @"value" # txOut

ptxOutDatum :: PFromDataable POutputDatum b => Term s (PTxOut :--> b)
ptxOutDatum = phoistAcyclic $ plam $ \txOut -> pfield @"datum" # txOut

pfromDJust :: PIsData a => Term s (PMaybeData a :--> a)
pfromDJust = phoistAcyclic $ plam \d -> pmatch d \case
  PDJust v -> pfield @"_0" # v
  PDNothing _ -> ptraceError "pfromDJust"

pextractNthInputValidatorHash :: PFromDataable PScriptHash b => Term s (PScriptContext :--> PInteger :--> b)
pextractNthInputValidatorHash = phoistAcyclic $ plam $ \context index -> unTermCont $ do
  txOutResolved <- pletC $ pextractNthTxInput # context # index
  address <- pletC $ pfield @"address" # (pfield @"resolved" # txOutResolved)
  let vhash = pmatch (pfield @"credential" # address) \case
        PScriptCredential vhashData -> vhashData
        PPubKeyCredential _ -> ptraceError "V4"
  pure $ pfield @"_0" # vhash

pextractNthTxInput :: Term s (PScriptContext :--> PInteger :--> PTxInInfo)
pextractNthTxInput = phoistAcyclic $ plam $ \context index ->
  let info = pfield @"txInfo" # context
      inputs = pfield @"inputs" # info
   in pelemAtOptimized # index # inputs

pvalueContainsToken :: Term s (PCurrencySymbol :--> PTokenName :--> PValue 'Sorted 'Positive :--> PBool)
pvalueContainsToken = phoistAcyclic $ plam $ \cs tn v -> 0 #< pvalueOf # v # cs # tn

-- | Checks if at least 1 given token is present among the inputs.
pisTokenSpent :: Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinList PTxInInfo :--> PBool)
pisTokenSpent = phoistAcyclic $ plam $ \symbol token inputs ->
  precList
    ( \recur input rest ->
        let value = pfield @"value" #$ pfield @"resolved" # input
         in pif
              (pvalueOf # value # symbol # token #> 0)
              ptrue
              (recur # rest)
    )
    (const pfalse)
    # inputs

-- | NOTE: doesn't error out when the source list has fewer items
pdrop' :: PIsListLike list a => Term s (PInteger :--> list a :--> list a)
pdrop' =
  phoistAcyclic $ pfix #$ plam $ \recur n l -> (pif (n #== 0) l (pelimList (\_ xs -> recur # (n - 1) # xs) pnil l))

pmustFindOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
pmustFindOwnInput = phoistAcyclic $ plam $ \txInputs outRef -> matchTxOutRef # outRef # txInputs
  where
    matchTxOutRef :: (PIsListLike list PTxInInfo) => Term s (PTxOutRef :--> list PTxInInfo :--> PTxOut)
    matchTxOutRef = phoistAcyclic $ plam $ \oref -> pfix #$ plam $ \recur xs ->
      pelimList (\y ys -> pif (oref #== (pfield @"outRef" # y)) (pfield @"resolved" # y) (recur # ys)) perror xs

-- | Get the first element of the PPair
pfst :: Term s (PPair a b :--> a)
pfst = phoistAcyclic $ plam $ \p -> pmatch p $ \(PPair a _) -> a

-- | Get the second element of the PPair
psnd :: Term s (PPair a b :--> b)
psnd = phoistAcyclic $ plam $ \p -> pmatch p $ \(PPair _ b) -> b

paddressPubKeyCredential :: PFromDataable PPubKeyHash b => Term s (PAddress :--> b)
paddressPubKeyCredential = phoistAcyclic $ plam $ \address -> pmatch (pfield @"credential" # address) $ \case
  PPubKeyCredential hsd -> pfield @"_0" # hsd
  PScriptCredential _ -> perror

pisPubKeyAddress :: Term s PAddress -> Term s PBool
pisPubKeyAddress addr = pmatch (pfield @"credential" # addr) \case
  PPubKeyCredential _ -> ptrue
  PScriptCredential _ -> pfalse

pisStakePtrAddress :: Term s PAddress -> Term s PBool
pisStakePtrAddress addr = pmatch (pfield @"stakingCredential" # addr) \case
  PDJust sc -> pmatch (pfield @"_0" # sc) \case
    PStakingPtr _ -> ptrue
    PStakingHash _ -> pfalse
  PDNothing _ -> pfalse

phaveSameStakingCredentials :: Term s (PAddress :--> PAddress :--> PBool)
phaveSameStakingCredentials = phoistAcyclic $ plam $ \a1 a2 ->
  pfield @"stakingCredential" # a1 #== pfield @"stakingCredential" # a2

pisSignedByPubKeyAddress :: Term s PAddress -> Term s (PBuiltinList (PAsData PPubKeyHash)) -> Term s PBool
pisSignedByPubKeyAddress addr signatories = pmatch (pfield @"credential" # addr) \case
  PPubKeyCredential cred' -> plet (pfield @"_0" # cred') \pkh -> ptxSignedByPkh # pkh # signatories
  PScriptCredential _ -> pfalse

ppaysToCredential :: Term s (PScriptHash :--> PTxOut :--> PBool)
ppaysToCredential = phoistAcyclic $ plam $ \valHash txOut ->
  let cred = pfield @"credential" #$ pfield @"address" # txOut
   in pmatch cred \case
        PScriptCredential txOutValHash -> (pfield @"_0" # txOutValHash) #== valHash
        PPubKeyCredential _ -> pfalse

pfiniteTxValidityRangeTimestamps :: Term s (PPOSIXTimeRange :--> PTimestamps)
pfiniteTxValidityRangeTimestamps = phoistAcyclic $ plam $ \txInfoValidPeriod -> pmatch txInfoValidPeriod $ \case
  (PInterval validityInterval) -> pletFields @'["from", "to"] validityInterval $ \validityFields ->
    pmatch (pfield @"_0" # validityFields.from) $ \case
      PFinite startTime -> pmatch (pfield @"_0" # validityFields.to) $ \case
        PFinite endTime ->
          pcon $ PTimestamps {lowerBound = pfield @"_0" # startTime, upperBound = pfield @"_0" # endTime}
        _ -> ptraceError "V8"
      _ -> ptraceError "V9"

{- |
Get the quantity of the given asset in the Value while checking
that there is enough oil in the value. If asset is ada, returned quantity is without oil.
Note: This function allows having more oil than is strictly required.
-}
pvalueOfWithOilCheck :: Term s (PValue sort guarantee :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PInteger)
pvalueOfWithOilCheck = phoistAcyclic $ plam $ \val symbol token oilAda -> unTermCont $ do
  adaNoOil <- pletC $ (pvalueOf # val # padaSymbol # padaToken) - oilAda
  pguardC "V0" (adaNoOil #>= 0)
  pure $ pif (pisAda # symbol) adaNoOil (pvalueOf # val # symbol # token)

pisDJust :: Term s (PMaybeData a :--> PBool)
pisDJust = phoistAcyclic $ plam $ \a ->
  pmatch
    a
    ( \case
        PDJust _ -> ptrue
        PDNothing _ -> pfalse
    )

pisDNothing :: Term s (PMaybeData a :--> PBool)
pisDNothing = phoistAcyclic $ plam $ \a ->
  pmatch
    a
    ( \case
        PDNothing _ -> ptrue
        PDJust _ -> pfalse
    )

pisJust :: Term s (PMaybe a :--> PBool)
pisJust = phoistAcyclic $ plam $ \a ->
  pmatch
    a
    ( \case
        PJust _ -> pconstant True
        PNothing -> pconstant False
    )

pisNothing :: Term s (PMaybe a :--> PBool)
pisNothing = phoistAcyclic $ plam $ \a ->
  pmatch
    a
    ( \case
        PNothing -> pconstant True
        PJust _ -> pconstant False
    )

pisTxValidityRangeShortEnough :: Term s (PPOSIXTime :--> PPOSIXTime :--> PBool)
pisTxValidityRangeShortEnough = phoistAcyclic $ plam $ \startTimestamp endTimestamp ->
  (pto endTimestamp - pto startTimestamp) #< maxTxValidityRangeSize

-- | Find the first value with its key matching the predicate.
pfindByKey :: PIsData v => Term s ((PAsData k :--> PBool) :--> PMap any k v :--> PMaybe v)
pfindByKey = phoistAcyclic $
  plam $ \predicate m ->
    precList
      (\recur x xs -> pif (predicate # (pfstBuiltin # x)) (pjust (pfromData (psndBuiltin # x))) (recur # xs))
      (const pnothing)
      # pto m

-- | Extract the redeemer associated with the given TxOutRef, throws if not found.
ptryTxOutRefRedeemer :: Term s (PTxOutRef :--> PMap 'Unsorted PScriptPurpose PRedeemer :--> PRedeemer)
ptryTxOutRefRedeemer = phoistAcyclic $ plam $ \txOutRef redeemers ->
  pfromJust
    #$ pfindByKey
    # plam
      ( \purpose -> pmatch (pfromData purpose) $ \case
          PSpending r -> pfield @"_0" # r #== txOutRef
          _ -> pfalse
      )
    # redeemers

minutes :: Num a => a -> a
minutes m = m * 60 * 1_000

pbetween :: PPartialOrd a => Term s a -> Term s a -> Term s a -> Term s PBool
pbetween l m h = pand' # (l #< m) # (m #< h)

pfoldl2AllowMore ::
  (PListLike list1, PListLike list2, PElemConstraint list1 a, PElemConstraint list2 b) =>
  Term s ((acc :--> a :--> b :--> acc) :--> acc :--> list1 a :--> list2 b :--> acc)
pfoldl2AllowMore =
  pfix #$ plam \recur f acc as bs ->
    pelimList
      ( \a tas ->
          pelimList
            (\b tbs -> recur # f # (f # acc # a # b) # tas # tbs)
            -- We allow more bs, but not more as.
            -- With a specific example of request application
            -- we allow more outputs, but we require that each request is processed.
            perror
            bs
      )
      acc
      as

pfoldl2 ::
  (PListLike list1, PListLike list2, PElemConstraint list1 a, PElemConstraint list2 b) =>
  Term s ((acc :--> a :--> b :--> acc) :--> acc :--> list1 a :--> list2 b :--> acc)
pfoldl2 =
  phoistAcyclic $
    pfix #$ plam \recur f acc as bs ->
      pelimList
        ( \a tas ->
            pelimList
              (\b tbs -> recur # f # (f # acc # a # b) # tas # tbs)
              (ptraceError "pfoldl2: list1.len > list2.len")
              bs
        )
        (pif (pnull # bs) acc (ptraceError "pfoldl2: list2.len > list1.len"))
        as

{- |
  Folding over inputs and outputs of a transaction.
  We also expect indices of the UTxOs being passed as an argument.
  Each output index yields two outputs, the one on the index and on just right after it.
  The second corresponding output can be fetched by forcing the lazy PTxOut passed into the state folding function.
  In case an index is -1, we want to pass PNothing to the state function as the first output
  (e.g. in cases we don't produce outputs to corresponding inputs).
  Note that forcing the lazy PTxOut for -1 index will error out.
-}
pfoldInputsWithTwoOutputs ::
  Term s a ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s (PBuiltinList (PAsData PInteger)) ->
  Term s (a :--> PTxOut :--> PMaybe PTxOut :--> PDelayed PTxOut :--> a) ->
  Term s a
pfoldInputsWithTwoOutputs initialA txIns txOuts inLocations outLocations f =
  pfoldl2
    # plam
      ( \state inI outI ->
          pif
            (pfromData outI #>= 0)
            ( f
                # state
                # (ptxInInfoResolved # (pelemAtOptimized # pfromData inI # txIns))
                # pjust (pelemAtOptimized # pfromData outI # txOuts)
                # pdelay (pelemAtOptimized # (pfromData outI + 1) # txOuts)
            )
            (f # state # (ptxInInfoResolved # (pelemAtOptimized # pfromData inI # txIns)) # pnothing # pdelay perror)
      )
    # initialA
    # inLocations
    # outLocations

pelemAtOptimized :: PIsListLike l a => Term s (PInteger :--> l a :--> a)
pelemAtOptimized = phoistAcyclic $
  pfix #$ plam $ \recur n xs ->
    pif
      (n #>= 5)
      (recur # (n - 5) #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail # xs)
      (pif (n #== 0) (phead # xs) (pelemAt # (n - 1) # (ptail # xs)))
