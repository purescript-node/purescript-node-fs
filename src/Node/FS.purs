module Node.FS where

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
  (==) FileLink     FileLink     = true
  (==) DirLink      DirLink      = true
  (==) JunctionLink JunctionLink = true
  (==) _ _ = false
  (/=) x y = not (x == y)