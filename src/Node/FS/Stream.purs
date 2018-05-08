module Node.FS.Stream
  ( createWriteStream
  , fdCreateWriteStream
  , WriteStreamOptions()
  , defaultWriteStreamOptions
  , createWriteStreamWith
  , fdCreateWriteStreamWith
  , createReadStream
  , fdCreateReadStream
  , ReadStreamOptions()
  , defaultReadStreamOptions
  , createReadStreamWith
  , fdCreateReadStreamWith
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Node.Stream (Readable(), Writable())
import Node.Path (FilePath())

import Node.FS (FileDescriptor, FileFlags(..), fileFlagsToNode)
import Node.FS.Perms (Perms())
import Node.FS.Perms as Perms
import Node.FS.Internal (mkEffect, unsafeRequireFS)

fs ::
  { createReadStream  :: forall opts. Fn2 (Nullable FilePath) { | opts } (Readable ())
  , createWriteStream :: forall opts. Fn2 (Nullable FilePath) { | opts } (Writable ())
  }
fs = unsafeRequireFS

readWrite :: Perms
readWrite = Perms.mkPerms rw rw rw
  where
  rw = Perms.read + Perms.write

null :: forall a. Nullable a
null = toNullable Nothing

nonnull :: forall a. a -> Nullable a
nonnull = toNullable <<< Just

-- | Create a Writable stream which writes data to the specified file, using
-- | the default options.
createWriteStream :: FilePath
                  -> Effect (Writable ())
createWriteStream = createWriteStreamWith defaultWriteStreamOptions

-- | Create a Writable stream which writes data to the specified file
-- | descriptor, using the default options.
fdCreateWriteStream :: FileDescriptor
                    -> Effect (Writable ())
fdCreateWriteStream = fdCreateWriteStreamWith defaultWriteStreamOptions

type WriteStreamOptions =
  { flags :: FileFlags
  , perms :: Perms
  }

defaultWriteStreamOptions :: WriteStreamOptions
defaultWriteStreamOptions =
  { flags: W
  , perms: readWrite
  }

-- | Like `createWriteStream`, but allows you to pass options.
createWriteStreamWith :: WriteStreamOptions
                      -> FilePath
                      -> Effect (Writable ())
createWriteStreamWith opts file = mkEffect $ \_ -> runFn2
  fs.createWriteStream (nonnull file)
    { mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    }

-- | Like `fdCreateWriteStream`, but allows you to pass options.
fdCreateWriteStreamWith :: WriteStreamOptions
                        -> FileDescriptor
                        -> Effect (Writable ())
fdCreateWriteStreamWith opts fd = mkEffect $ \_ -> runFn2
  fs.createWriteStream null
    { fd
    , mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    }

-- | Create a Readable stream which reads data to the specified file, using
-- | the default options.
createReadStream :: FilePath
                  -> Effect (Readable ())
createReadStream = createReadStreamWith defaultReadStreamOptions

-- | Create a Readable stream which reads data to the specified file
-- | descriptor, using the default options.
fdCreateReadStream :: FileDescriptor
                   -> Effect (Readable ())
fdCreateReadStream = fdCreateReadStreamWith defaultReadStreamOptions

type ReadStreamOptions =
  { flags     :: FileFlags
  , perms     :: Perms
  , autoClose :: Boolean
  }

defaultReadStreamOptions :: ReadStreamOptions
defaultReadStreamOptions =
  { flags: R
  , perms: readWrite
  , autoClose: true
  }

-- | Create a Readable stream which reads data from the specified file.
createReadStreamWith :: ReadStreamOptions
                     -> FilePath
                     -> Effect (Readable ())
createReadStreamWith opts file = mkEffect $ \_ -> runFn2
  fs.createReadStream (nonnull file)
    { mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , autoClose: opts.autoClose
    }

-- | Create a Readable stream which reads data from the specified file descriptor.
fdCreateReadStreamWith :: ReadStreamOptions
                       -> FileDescriptor
                       -> Effect (Readable ())
fdCreateReadStreamWith opts fd = mkEffect $ \_ -> runFn2
  fs.createReadStream null
    { fd
    , mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , autoClose: opts.autoClose
    }
