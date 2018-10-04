module Data.Tagged (Tagged(), tag, untag) where

import Prelude
import Data.Newtype (class Newtype)

newtype Tagged t a = Tagged a

tag ∷ ∀ t a. a → Tagged t a
tag a = Tagged a

untag ∷ ∀ t a. Tagged t a → a
untag (Tagged a) = a

instance newtypeTagged ∷ Newtype (Tagged t a) a where
  wrap = tag
  unwrap = untag

instance showTagged ∷ Show a ⇒ Show (Tagged t a) where
  show (Tagged x) = "(tag " <> show x <> ")"

derive newtype instance ordTagged ∷ Ord a ⇒ Ord (Tagged t a)
derive newtype instance eqTagged ∷ Eq a ⇒ Eq (Tagged t a)
derive newtype instance monoidTagged ∷ Monoid a ⇒ Monoid (Tagged t a)
derive newtype instance semigroupTagged ∷ Semigroup a ⇒ Semigroup (Tagged t a)
