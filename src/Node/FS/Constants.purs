module Node.FS.Constants where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)

-- | the mode parameter passed to `access` and `accessSync`.
foreign import data AccessMode :: Type

-- | the file is visible to the calling process. 
-- | This is useful for determining if a file exists, but says nothing about rwx permissions. Default if no mode is specified.
foreign import f_OK :: AccessMode

-- | the file can be read by the calling process.
foreign import r_OK :: AccessMode

-- | the file can be written by the calling process.
foreign import w_OK :: AccessMode

-- | the file can be executed by the calling process. This has no effect on Windows (will behave like fs.constants.F_OK).
foreign import x_OK :: AccessMode

defaultAccessMode = f_OK :: AccessMode

foreign import data CopyMode :: Type

foreign import copyFile_EXCL :: CopyMode
foreign import copyFile_FICLONE :: CopyMode
foreign import copyFile_FICLONE_FORCE :: CopyMode

defaultCopyMode = copyFile_EXCL :: CopyMode

foreign import appendCopyMode :: Fn2 CopyMode CopyMode CopyMode

instance Semigroup CopyMode where
  append l r = runFn2 appendCopyMode l r
