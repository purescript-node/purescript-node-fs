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

instance showFileFlags :: Show FileFlags where
    show R       = "r"
    show R_PLUS  = "r+"
    show RS      = "rs"
    show RS_PLUS = "rs+"
    show W       = "w"
    show WX      = "wx"
    show W_PLUS  = "w+"
    show WX_PLUS = "wx+"
    show A       = "a"
    show AX      = "ax"
    show A_PLUS  = "a+"
    show AX_PLUS = "ax+"

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
