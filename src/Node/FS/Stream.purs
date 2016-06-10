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
import Control.Monad.Eff (Eff)
import Node.Stream (Readable(), Writable())
import Node.Path (FilePath())

import Node.FS (FS, FileDescriptor, FileFlags(..), fileFlagsToNode)
import Node.FS.Perms (Perms())
import Node.FS.Perms as Perms
import Node.FS.Internal (mkEff, unsafeRequireFS)

fs ::
  { createReadStream  :: forall eff opts. Fn2 (Nullable FilePath) { | opts } (Readable () (fs :: FS | eff))
  , createWriteStream :: forall eff opts. Fn2 (Nullable FilePath) { | opts } (Writable () (fs :: FS | eff))
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
createWriteStream :: forall eff.
                  FilePath
                  -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
createWriteStream = createWriteStreamWith defaultWriteStreamOptions

-- | Create a Writable stream which writes data to the specified file
-- | descriptor, using the default options.
fdCreateWriteStream :: forall eff.
                    FileDescriptor
                    -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
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
createWriteStreamWith :: forall eff.
                      WriteStreamOptions
                      -> FilePath
                      -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
createWriteStreamWith opts file = mkEff $ \_ -> runFn2
  fs.createWriteStream (nonnull file)
    { mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    }

-- | Like `fdCreateWriteStream`, but allows you to pass options.
fdCreateWriteStreamWith :: forall eff.
                        WriteStreamOptions
                        -> FileDescriptor
                        -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
fdCreateWriteStreamWith opts fd = mkEff $ \_ -> runFn2
  fs.createWriteStream null
    { fd
    , mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    }

-- | Create a Readable stream which reads data to the specified file, using
-- | the default options.
createReadStream :: forall eff.
                  FilePath
                  -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
createReadStream = createReadStreamWith defaultReadStreamOptions

-- | Create a Readable stream which reads data to the specified file
-- | descriptor, using the default options.
fdCreateReadStream :: forall eff.
                   FileDescriptor
                   -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
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
createReadStreamWith :: forall eff.
                     ReadStreamOptions
                     -> FilePath
                     -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
createReadStreamWith opts file = mkEff $ \_ -> runFn2
  fs.createReadStream (nonnull file)
    { mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , autoClose: opts.autoClose
    }

-- | Create a Readable stream which reads data from the specified file descriptor.
fdCreateReadStreamWith :: forall eff.
                       ReadStreamOptions
                       -> FileDescriptor
                       -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
fdCreateReadStreamWith opts fd = mkEff $ \_ -> runFn2
  fs.createReadStream null
    { fd
    , mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , autoClose: opts.autoClose
    }
