module Decosim.Sim.Types
  ( Meters(..)
  , Bar(..)
  , He, N2, O2
  , class ToNumber
  , toNumber
  ) where

import Prelude
import Data.Tagged (Tagged, untag)
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Minutes)


-- Meters

newtype Meters = Meters Number
derive         instance newtypeMeters ∷ Newtype Meters _
derive newtype instance eqMeters      ∷ Eq Meters
derive newtype instance ordMeters     ∷ Ord Meters

instance semigroupMeters ∷ Semigroup Meters where
  append (Meters x) (Meters y) = Meters (x + y)

instance monoidMeters ∷ Monoid Meters where
  mempty = Meters 0.0

instance showMeters ∷ Show Meters where
  show (Meters n) = "(Meters " <> show n <> ")"

-- Bar

newtype Bar = Bar Number
derive         instance newtypeBar ∷ Newtype Bar _
derive newtype instance eqBar      ∷ Eq Bar
derive newtype instance ordBar     ∷ Ord Bar

instance semigroupBar ∷ Semigroup Bar where
  append (Bar x) (Bar y) = Bar (x + y)

instance monoidBar ∷ Monoid Bar where
  mempty = Bar 0.0

instance showBar ∷ Show Bar where
  show (Bar n) = "(Bar " <> show n <> ")"

-- Tags

data He
data N2
data O2


-- toNumber

class ToNumber a where
  toNumber ∷ a → Number

instance toNumberMeters ∷ ToNumber Meters where
  toNumber = unwrap

instance toNumberBar ∷ ToNumber Bar where
  toNumber = unwrap

instance toNumberMinutes ∷ ToNumber Minutes where
  toNumber = unwrap

instance toNumberTaggedNumber ∷ ToNumber (Tagged t Number) where
  toNumber = untag

instance toNumberTaggedBar ∷ ToNumber (Tagged t Bar) where
  toNumber = unwrap <<< untag

instance toNumberTaggedMinutes ∷ ToNumber (Tagged t Minutes) where
  toNumber = unwrap <<< untag
