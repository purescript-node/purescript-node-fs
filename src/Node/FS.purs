module Node.FS
  ( FS()
  , FileDescriptor(..)
  , FileFlags(..)
  , FileMode(..)
  , SymlinkType(..)
  , BufferLength(..)
  , BufferOffset(..)
  , ByteCount(..)
  , FilePosition(..)
  , fileFlagsToNode
  ) where

import Prelude

-- |
-- Effect type for file system usage.
--
foreign import data FS :: !

foreign import data FileDescriptor :: *

data FileFlags = R | R_PLUS | RS | RS_PLUS
               | W | WX | W_PLUS | WX_PLUS
               | A | AX | A_PLUS | AX_PLUS

fileFlagsToNode :: FileFlags -> String
fileFlagsToNode ff = case ff of
  R       -> "r"
  R_PLUS  -> "r+"
  RS      -> "rs"
  RS_PLUS -> "rs+"
  W       -> "w"
  WX      -> "wx"
  W_PLUS  -> "w+"
  WX_PLUS -> "wx+"
  A       -> "a"
  AX      -> "ax"
  A_PLUS  -> "a+"
  AX_PLUS -> "ax+"

type FileMode = Int
type FilePosition = Int
type BufferLength = Int
type BufferOffset = Int
type ByteCount = Int

-- |
-- Symlink varieties.
--
data SymlinkType = FileLink | DirLink | JunctionLink

instance showSymlinkType :: Show SymlinkType where
  show FileLink     = "file"
  show DirLink      = "dir"
  show JunctionLink = "junction"

instance eqSymlinkType :: Eq SymlinkType where
  eq FileLink     FileLink     = true
  eq DirLink      DirLink      = true
  eq JunctionLink JunctionLink = true
  eq _ _ = false
