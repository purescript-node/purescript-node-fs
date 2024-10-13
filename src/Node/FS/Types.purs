module Node.FS.Types where

import Prelude

import Data.Nullable (Nullable)
import Data.Nullable as Nullable

foreign import data FileDescriptor :: Type

type FileMode = Int
type FilePosition = Int
type BufferLength = Int
type BufferOffset = Int
type ByteCount = Int
type EncodingString = String

-- newtype FileMode = FileMode Int
-- newtype FilePosition = FilePosition Int
-- newtype BufferLength = BufferLength Int
-- newtype BufferOffset = BufferOffset Int
-- newtype ByteCount = ByteCount Int
-- -- newtype EncodingString = EncodingString String
--
-- derive newtype instance Eq FileMode
-- derive newtype instance Eq FilePosition
-- derive newtype instance Eq BufferLength
-- derive newtype instance Eq BufferOffset
-- derive newtype instance Eq ByteCount
-- -- derive newtype instance Eq EncodingString
--
-- derive newtype instance Show FileMode
-- derive newtype instance Show FilePosition
-- derive newtype instance Show BufferLength
-- derive newtype instance Show BufferOffset
-- derive newtype instance Show ByteCount
-- -- derive newtype instance Show EncodingString
--
-- derive newtype instance Ord FileMode
-- derive newtype instance Ord FilePosition
-- derive newtype instance Ord BufferLength
-- derive newtype instance Ord BufferOffset
-- derive newtype instance Ord ByteCount
-- -- derive newtype instance Ord EncodingString

-- | Symlink varieties.
data SymlinkType = FileLink | DirLink | JunctionLink | AutodetectLink

-- | Convert a `SymlinkType` to a `String` in the format expected by the
-- | Node.js filesystem API.
symlinkTypeToNode :: SymlinkType -> Nullable String
symlinkTypeToNode ty = case ty of
  FileLink -> Nullable.notNull "file"
  DirLink -> Nullable.notNull "dir"
  JunctionLink -> Nullable.notNull "junction"
  AutodetectLink -> Nullable.null

instance Show SymlinkType where
  show FileLink = "FileLink"
  show DirLink = "DirLink"
  show JunctionLink = "JunctionLink"
  show AutodetectLink = "AutodetectLink"

derive instance Eq SymlinkType
