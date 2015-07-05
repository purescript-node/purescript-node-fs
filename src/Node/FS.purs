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

instance showSymlinkType :: Show SymlinkType where
  show FileLink     = "file"
  show DirLink      = "dir"
  show JunctionLink = "junction"

instance eqSymlinkType :: Eq SymlinkType where
  eq FileLink     FileLink     = true
  eq DirLink      DirLink      = true
  eq JunctionLink JunctionLink = true
  eq _ _ = false
