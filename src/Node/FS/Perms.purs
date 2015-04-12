module Node.FS.Perms
  ( permsFromString
  , permsToString
  , permsToNum
  , none
  , r
  , w
  , x
  , mkPerms
  , Perms()
  , Perm()
  ) where

import Global (readInt)
import Data.Maybe (Maybe(..))
import Data.Char (Char(), charString)
import Data.String (toCharArray)
import Data.Function

newtype Perm = Perm { r :: Boolean, w :: Boolean, x :: Boolean }
newtype Perms = Perms { u :: Perm, g :: Perm, o :: Perm }

none = Perm { r: false, w: false, x: false }
r = Perm { r: true, w: false, x: false }
w = Perm { r: false, w: true, x: false }
x = Perm { r: false, w: false, x: true }

instance semigroupPerm :: Semigroup Perm where
  (<>) (Perm { r = r0, w = w0, x = x0 }) (Perm { r = r1, w = w1, x = x1 }) =
    Perm { r: r0 || r1, w: w0 || w1, x: x0 || x1  }

permsFromString :: String -> Maybe Perms
permsFromString = _perms <<< toCharArray
  where
    _perms (u : g : o : []) =
      mkPerms <$> permFromChar u
              <*> permFromChar g
              <*> permFromChar o
    _perms _ = Nothing

permFromChar :: Char -> Maybe Perm
permFromChar = _perm <<< charString
  where
    _perm "0" = Just $ mkPerm false false false
    _perm "1" = Just $ mkPerm false false true
    _perm "2" = Just $ mkPerm false true  false
    _perm "3" = Just $ mkPerm false true  true
    _perm "4" = Just $ mkPerm true  false false
    _perm "5" = Just $ mkPerm true  false true
    _perm "6" = Just $ mkPerm true  true  false
    _perm "7" = Just $ mkPerm true  true  true
    _perm _   = Nothing

mkPerm :: Boolean -> Boolean -> Boolean -> Perm
mkPerm r w x = Perm { r: r, w: w, x: x }

mkPerms :: Perm -> Perm -> Perm -> Perms
mkPerms u g o = Perms { u: u, g: g, o: o }

permToNum :: Perm -> Number
permToNum (Perm { r = r, w = w, x = x }) =
    (if r then 4 else 0)
  + (if w then 2 else 0)
  + (if x then 1 else 0)

permToString :: Perm -> String
permToString = show <<< permToNum

permsToString :: Perms -> String
permsToString (Perms { u = u, g = g, o = o }) =
     "0"
  ++ permToString u
  ++ permToString g
  ++ permToString o

permsToNum :: Perms -> Number
permsToNum p = readInt 8 $ permsToString p

instance showPerm :: Show Perm where
  show (Perm { r = r, w = w, x = x }) =
       (if r then "r" else "-")
    ++ (if w then "w" else "-")
    ++ (if x then "x" else "-")

instance showPerms :: Show Perms where
  show (Perms { u = u, g = g, o = o }) = show u ++ show g ++ show o
