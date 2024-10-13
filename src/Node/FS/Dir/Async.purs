module Node.FS.Dir.Async (close, read) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.FS.Dir (Dir)
import Node.FS.Dirent (Dirent, DirentNameString)
import Node.FS.Internal.Callbacks (Callback0, JSCallback0, JSCallback1, Callback1, handleCallback0, handleCallback1)

foreign import closeImpl :: EffectFn2 Dir JSCallback0 Unit
foreign import readImpl :: EffectFn2 Dir (JSCallback1 (Nullable (Dirent DirentNameString))) Unit

-- | Asynchronously close the directory's underlying resource handle.
close :: Dir -> Callback0 -> Effect Unit
close dir cb = runEffectFn2 closeImpl dir (handleCallback0 cb)

-- | Asynchronously read the next directory entry.
read :: Dir -> (Callback1 (Maybe (Dirent DirentNameString))) -> Effect Unit
read dir cb = runEffectFn2 readImpl dir (handleCallback1 (cb <<< map toMaybe))
