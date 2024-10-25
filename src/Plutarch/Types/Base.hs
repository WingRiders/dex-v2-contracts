{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Types.Base where

import Plutarch
import Plutarch.Api.V2 (PPOSIXTime)
import Plutarch.Prelude

data PTimestamps (s :: S) = PTimestamps
  { lowerBound :: Term s PPOSIXTime
  , upperBound :: Term s PPOSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (PEq, PlutusType)

instance DerivePlutusType PTimestamps where
  type DPTStrat _ = PlutusTypeScott

data PTriplet (a :: PType) (b :: PType) (c :: PType) (s :: S)
  = PTriplet (Term s a) (Term s b) (Term s c)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PTriplet a b c) where type DPTStrat _ = PlutusTypeScott

data PQuartet (a :: PType) (b :: PType) (c :: PType) (d :: PType) (s :: S)
  = PQuartet (Term s a) (Term s b) (Term s c) (Term s d)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PQuartet a b c d) where type DPTStrat _ = PlutusTypeScott
