module Node.FS where

import Prelude

-- |
-- Effect type for file system usage.
--
foreign import data FS :: !

-- |
-- Symlink varieties.
--
data SymlinkType = FileLink | DirLink | JunctionLink

-- |
-- Convert a `SymlinkType` to a `String` expected by the Node.js filesystem
-- API.
--
symlinkTypeToNode :: SymlinkType -> String
symlinkTypeToNode ty = case ty of
  FileLink -> "file"
  DirLink -> "dir"
  JunctionLink -> "junction"

instance showSymlinkType :: Show SymlinkType where
  show FileLink     = "FileLink"
  show DirLink      = "DirLink"
  show JunctionLink = "JunctionLink"

instance eqSymlinkType :: Eq SymlinkType where
  eq FileLink     FileLink     = true
  eq DirLink      DirLink      = true
  eq JunctionLink JunctionLink = true
  eq _ _ = false
