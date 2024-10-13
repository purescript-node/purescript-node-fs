module Node.FS.Dir.Sync (close, read) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameString)

foreign import closeSyncImpl :: EffectFn1 Dir Unit
foreign import readSyncImpl :: EffectFn1 Dir (Nullable (Dirent DirentNameString))

-- | Synchronously close the directory's underlying resource handle.
close :: Dir -> Effect Unit
close = runEffectFn1 closeSyncImpl

-- | Synchronously read the next directory entry.
read :: Dir -> Effect (Maybe (Dirent DirentNameString))
read dir = toMaybe <$> runEffectFn1 readSyncImpl dir
