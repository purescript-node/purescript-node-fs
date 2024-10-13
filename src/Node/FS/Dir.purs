module Node.FS.Dir where

import Prelude

import Node.Path (FilePath)

-- Foreign imports for the Dir class
foreign import data Dir :: Type

foreign import showDirObj :: Dir -> String

instance Show Dir where
  show s = showDirObj s

-- | Get the path of this directory as was provided to fs.opendir(), fs.opendirSync(), or fsPromises.opendir().
foreign import path :: Dir -> FilePath
