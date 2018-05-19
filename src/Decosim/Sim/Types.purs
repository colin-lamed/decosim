module Decosim.Sim.Types
  ( Meters(..)
  , Bar(..)
  , He, N2, O2
  , extract
  ) where

import Prelude
import Data.Functor.Tagged (untagged)
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Minutes)

-- Meters

newtype Meters = Meters Number
derive instance newtypeMeters :: Newtype Meters _
derive newtype instance eqMeters       :: Eq Meters
derive newtype instance ordMeters      :: Ord Meters
derive newtype instance semiringMeters :: Semiring Meters
derive newtype instance ringMeters     :: Ring Meters

instance showMeters :: Show Meters where
  show (Meters n) = "(Meters " <> show n <> ")"

-- Bar

newtype Bar = Bar Number
derive instance newtypeBar :: Newtype Bar _
derive newtype instance eqBar            :: Eq Bar
derive newtype instance ordBar           :: Ord Bar
derive newtype instance semiringBar      :: Semiring Bar
derive newtype instance ringBar          :: Ring Bar

instance showBar :: Show Bar where
  show (Bar n) = "(Bar " <> show n <> ")"


data He
data N2
data O2

extract = unwrap <<< untagged
