-- | This module defines types and functions for working with undefinable types
-- | using the FFI.

module Node.FS.Internal.Undefinable
  ( Undefinable
  , undefined
  , notUndefined
  , toMaybe
  , toUndefinable
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.Function (on)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)

foreign import data Undefinable :: Type -> Type

type role Undefinable representational

-- | The undefined value.
foreign import undefined :: forall a. Undefinable a

foreign import undefinable :: forall a r. Fn3 (Undefinable a) r (a -> r) r

-- | Wrap a non-undefined value.
foreign import notUndefined :: forall a. a -> Undefinable a

-- | Takes `Nothing` to `undefined`, and `Just a` to `a`.
toUndefinable :: forall a. Maybe a -> Undefinable a
toUndefinable = maybe undefined notUndefined

-- | Represent `undefined` using `Maybe a` as `Nothing`. Note that this function
-- | can violate parametricity, as it inspects the runtime representation of
-- | its argument (see the warning about the pitfall of `Undefinable` above).
toMaybe :: forall a. Undefinable a -> Maybe a
toMaybe n = runFn3 undefinable n Nothing Just

instance showUndefinable :: Show a => Show (Undefinable a) where
  show = maybe "undefined" show <<< toMaybe

instance eqUndefinable :: Eq a => Eq (Undefinable a) where
  eq = eq `on` toMaybe

instance eq1Undefinable :: Eq1 Undefinable where
  eq1 = eq

instance ordUndefinable :: Ord a => Ord (Undefinable a) where
  compare = compare `on` toMaybe

instance ord1Undefinable :: Ord1 Undefinable where
  compare1 = compare
