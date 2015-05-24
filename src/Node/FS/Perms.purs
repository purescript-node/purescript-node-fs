module Node.FS.Perms
  ( permsFromString
  , permsToString
  , permsToInt
  , none
  , r
  , w
  , x
  , all
  , mkPerms
  , Perms()
  , Perm()
  ) where

import Global (readInt)
import Data.Maybe (Maybe(..))
import Data.Char (Char(), charString)
import Data.String (toCharArray)
import Data.Int (Int(), fromNumber, toNumber)

-- | A `Perm` value specifies what is allowed to be done with a particular
-- | file by a particular class of user &mdash; that is, whether it is
-- | readable, writable, and/or executable. It has a semigroup instance, which
-- | allows you to combine permissions; for example, `r <> w` means "readable
-- | and writable".
newtype Perm = Perm { r :: Boolean, w :: Boolean, x :: Boolean }

-- | A `Perms` value includes all the permissions information about a
-- | particular file or directory, by storing a `Perm` value for each of the
-- | file owner, the group, and others.
newtype Perms = Perms { u :: Perm, g :: Perm, o :: Perm }

-- | No permissions. This is the identity of the `Semigroup` operation `(<>)`
-- | for `Perm`.
none :: Perm
none = Perm { r: false, w: false, x: false }

-- | The "readable" permission.
r :: Perm
r = Perm { r: true, w: false, x: false }

-- | The "writable" permission.
w :: Perm
w = Perm { r: false, w: true, x: false }

-- | The "executable" permission.
x :: Perm
x = Perm { r: false, w: false, x: true }

-- | All permissions: readable, writable, and executable.
all :: Perm
all = r <> w <> x

instance semigroupPerm :: Semigroup Perm where
  (<>) (Perm { r = r0, w = w0, x = x0 }) (Perm { r = r1, w = w1, x = x1 }) =
    Perm { r: r0 || r1, w: w0 || w1, x: x0 || x1  }

-- | Attempt to parse a `Perms` value from a `String` containing an octal
-- | integer. For example,
-- | `permsFromString "644" == Just (mkPerms (r <> w) r r)`.
permsFromString :: String -> Maybe Perms
permsFromString = _perms <<< toCharArray
  where
    _perms [u, g, o] =
      mkPerms <$> permFromChar u
              <*> permFromChar g
              <*> permFromChar o
    _perms _ = Nothing

permFromChar :: Char -> Maybe Perm
permFromChar = _perm <<< charString
  where
    _perm "0" = Just $ none
    _perm "1" = Just $ x
    _perm "2" = Just $ w
    _perm "3" = Just $ w <> x
    _perm "4" = Just $ r
    _perm "5" = Just $ r <> x
    _perm "6" = Just $ r <> w
    _perm "7" = Just $ r <> w <> x
    _perm _   = Nothing

-- | Create a `Perm` value. The arguments represent the readable, writable, and
-- | executable permissions, in that order.
mkPerm :: Boolean -> Boolean -> Boolean -> Perm
mkPerm r w x = Perm { r: r, w: w, x: x }

-- | Create a `Perms` value. The arguments represent the user's, group's, and
-- | others' permission sets, respectively.
mkPerms :: Perm -> Perm -> Perm -> Perms
mkPerms u g o = Perms { u: u, g: g, o: o }

-- | Convert a `Perm` to an octal digit. For example:
-- |
-- | * `permToInt r == 4`
-- | * `permToInt w == 2`
-- | * `permToInt (r <> w) == 6`
permToInt :: Perm -> Int
permToInt (Perm { r = r, w = w, x = x }) = fromNumber $
    (if r then 4 else 0)
  + (if w then 2 else 0)
  + (if x then 1 else 0)

-- | Convert a `Perm` to an octal string, via `permToInt`.
permToString :: Perm -> String
permToString = show <<< toNumber <<< permToInt

-- | Convert a `Perms` value to an octal string, in a format similar to that
-- | accepted by `chmod`. For example:
-- |
-- | * `permsToString (mkPerms (r <> w) r r) == "0644"`
permsToString :: Perms -> String
permsToString (Perms { u = u, g = g, o = o }) =
     "0"
  ++ permToString u
  ++ permToString g
  ++ permToString o

permsToInt :: Perms -> Int
permsToInt p = fromNumber $ readInt 8 $ permsToString p

instance showPerm :: Show Perm where
  show (Perm { r = r, w = w, x = x }) =
       (if r then "r" else "-")
    ++ (if w then "w" else "-")
    ++ (if x then "x" else "-")

instance showPerms :: Show Perms where
  show (Perms { u = u, g = g, o = o }) = show u ++ show g ++ show o
