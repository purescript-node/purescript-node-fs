module Node.FS.Stream
  ( createWriteStream
  , WriteStreamOptions
  , createWriteStream'
  , fdCreateWriteStream
  , fdCreateWriteStream'
  , createReadStream
  , ReadStreamOptions
  , createReadStream'
  , fdCreateReadStream
  , fdCreateReadStream'
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.FS (FileDescriptor)
import Node.Path (FilePath)
import Node.Stream (Readable, Writable)
import Prim.Row as Row

-- | Create a Writable stream which writes data to the specified file, using
-- | the default options.
createWriteStream :: FilePath -> Effect (Writable ())
createWriteStream f = runEffectFn1 createWriteStreamImpl f

foreign import createWriteStreamImpl :: EffectFn1 (FilePath) (Writable ())

type WriteStreamOptions =
  ( flags :: String
  , encoding :: String
  , mode :: Int
  , autoClose :: Boolean
  , emitClose :: Boolean
  , start :: Int
  )

-- | Create a Writable stream which writes data to the specified file.
-- | Unused options should not be specified. Some options
-- | (e.g. `flags`, `encoding`, and `mode`) should convert their 
-- | PureScript values to the corresponding JavaScript ones:
-- | ```
-- | filePath # createWriteStream'
-- |   { flags: fileFlagsToNode R
-- |   , encoding: encodingToNode UTF8
-- |   , mode: permsToInt Perms.all
-- |   }
-- | ```
createWriteStream'
  :: forall r trash
   . Row.Union r trash WriteStreamOptions
  => FilePath
  -> { | r }
  -> Effect (Writable ())
createWriteStream' f opts = runEffectFn2 createWriteStreamOptsImpl f opts

foreign import createWriteStreamOptsImpl :: forall r. EffectFn2 (FilePath) ({ | r }) ((Writable ()))

-- | Create a Writable stream which writes data to the specified file
-- | descriptor, using the default options.
fdCreateWriteStream :: FileDescriptor -> Effect (Writable ())
fdCreateWriteStream f = runEffectFn1 fdCreateWriteStreamImpl f

foreign import fdCreateWriteStreamImpl :: EffectFn1 (FileDescriptor) (Writable ())

-- | Create a Writable stream which writes data to the specified file descriptor.
-- | Unused options should not be specified. Some options
-- | (e.g. `flags`, `encoding`, and `mode`) should convert their 
-- | PureScript values to the corresponding JavaScript ones:
-- | ```
-- | filePath # fdCreateWriteStream'
-- |   { flags: fileFlagsToNode R
-- |   , encoding: encodingToNode UTF8
-- |   , mode: permsToInt Perms.all
-- |   }
-- | ```
fdCreateWriteStream'
  :: forall r trash
   . Row.Union r trash WriteStreamOptions
  => FileDescriptor
  -> { | r }
  -> Effect (Writable ())
fdCreateWriteStream' f opts = runEffectFn2 fdCreateWriteStreamOptsImpl f opts

foreign import fdCreateWriteStreamOptsImpl :: forall r. EffectFn2 (FileDescriptor) ({ | r }) (Writable ())

-- | Create a Readable stream which reads data to the specified file, using
-- | the default options.
createReadStream :: FilePath -> Effect (Readable ())
createReadStream p = runEffectFn1 createReadStreamImpl p

foreign import createReadStreamImpl :: EffectFn1 (FilePath) (Readable ())

type ReadStreamOptions =
  ( flags :: String
  , encoding :: String
  , mode :: Int
  , autoClose :: Boolean
  , emitClose :: Boolean
  , start :: Int
  , end :: Int
  , highWaterMark :: Int
  )

-- | Create a Readable stream which reads data from the specified file.
-- | Unused options should not be specified. Some options
-- | (e.g. `flags`, `encoding`, and `mode`) should convert their 
-- | PureScript values to the corresponding JavaScript ones:
-- | ```
-- | filePath # createReadStream'
-- |   { flags: fileFlagsToNode R
-- |   , encoding: encodingToNode UTF8
-- |   , mode: permsToInt Perms.all
-- |   }
-- | ```
createReadStream'
  :: forall r trash
   . Row.Union r trash ReadStreamOptions
  => FilePath
  -> { | r }
  -> Effect (Readable ())
createReadStream' path opts = runEffectFn2 createReadStreamOptsImpl path opts

foreign import createReadStreamOptsImpl :: forall r. EffectFn2 (FilePath) ({ | r }) ((Readable ()))

-- | Create a Readable stream which reads data to the specified file
-- | descriptor, using the default options.
fdCreateReadStream :: FileDescriptor -> Effect (Readable ())
fdCreateReadStream f = runEffectFn1 fdCreateReadStreamImpl f

foreign import fdCreateReadStreamImpl :: EffectFn1 (FileDescriptor) (Readable ())

-- | Create a Readable stream which reads data to the specified file descriptor.
-- | Unused options should not be specified. Some options
-- | (e.g. `flags`, `encoding`, and `mode`) should convert their 
-- | PureScript values to the corresponding JavaScript ones:
-- | ```
-- | filePath # fdCreateReadStream'
-- |   { flags: fileFlagsToNode R
-- |   , encoding: encodingToNode UTF8
-- |   , mode: permsToInt Perms.all
-- |   }
-- | ```
fdCreateReadStream'
  :: forall r trash
   . Row.Union r trash ReadStreamOptions
  => FileDescriptor
  -> { | r }
  -> Effect (Readable ())
fdCreateReadStream' f opts = runEffectFn2 fdCreateReadStreamOptsImpl f opts

foreign import fdCreateReadStreamOptsImpl :: forall r. EffectFn2 (FileDescriptor) ({ | r }) ((Readable ()))

