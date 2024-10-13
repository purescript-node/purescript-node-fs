module Node.FS.Options where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Foreign (Foreign)
import Foreign as Foreign
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..), encodingToNode)
import Node.FS.Constants (CopyMode, FileFlags(..), copyFile_NO_FLAGS, fileFlagsToNode)
import Node.FS.Dirent (Dirent, DirentNameString)
import Node.FS.Internal.Undefinable (Undefinable)
import Node.FS.Internal.Undefinable as Undefinable
import Node.FS.Perms (Perms, all, mkPerms, permsToString, read, write)
import Node.FS.Types (BufferLength, BufferOffset, EncodingString, FilePosition)
import Node.Path (FilePath)

type RmdirOptions = { maxRetries :: Int, retryDelay :: Int }

rmdirOptionsDefault :: RmdirOptions
rmdirOptionsDefault = { maxRetries: 0, retryDelay: 100 }

type RmOptions = { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int }

rmOptionsDefault :: RmOptions
rmOptionsDefault = { force: false, maxRetries: 100, recursive: false, retryDelay: 1000 }

----------

type MkdirOptionsInternal = { recursive :: Boolean, mode :: String }
type MkdirOptions = { recursive :: Boolean, mode :: Perms }

mkdirOptionsDefault :: MkdirOptions
mkdirOptionsDefault = { recursive: false, mode: mkPerms all all all }

mkdirOptionsToInternal :: MkdirOptions -> MkdirOptionsInternal
mkdirOptionsToInternal { recursive, mode } = { recursive, mode: permsToString mode }

---------

type RealpathOptionsInternal = { encoding :: EncodingString }
type RealpathOptions = { encoding :: Encoding }

realpathOptionsDefault :: RealpathOptions
realpathOptionsDefault = { encoding: UTF8 }

realpathOptionsToInternal :: RealpathOptions -> RealpathOptionsInternal
realpathOptionsToInternal { encoding } = { encoding: encodingToNode encoding }

---------
type ReaddirOptionsInternal =
  { encoding :: Foreign -- encoding string or "buffer"
  , recursive :: Boolean
  , withFileTypes :: Boolean
  }

type ReaddirFilePathOptions = { recursive :: Boolean, encoding :: Encoding }

readdirFilePathOptionsDefault :: ReaddirFilePathOptions
readdirFilePathOptionsDefault = { recursive: false, encoding: UTF8 }

readdirFilePathOptionsToInternal :: ReaddirFilePathOptions -> ReaddirOptionsInternal
readdirFilePathOptionsToInternal { recursive, encoding } = { recursive, encoding: Foreign.unsafeToForeign $ encodingToNode encoding, withFileTypes: false }

type ReaddirBufferOptions = { recursive :: Boolean }

readdirBufferOptionsDefault :: ReaddirBufferOptions
readdirBufferOptionsDefault = { recursive: false }

readdirBufferOptionsToInternal :: ReaddirBufferOptions -> ReaddirOptionsInternal
readdirBufferOptionsToInternal { recursive } = { recursive, encoding: Foreign.unsafeToForeign "buffer", withFileTypes: false }

type ReaddirDirentOptions = { recursive :: Boolean, encoding :: Encoding }

readdirDirentOptionsDefault :: ReaddirDirentOptions
readdirDirentOptionsDefault = { recursive: false, encoding: UTF8 }

readdirDirentOptionsToInternal :: ReaddirDirentOptions -> ReaddirOptionsInternal
readdirDirentOptionsToInternal { recursive, encoding } = { recursive, encoding: Foreign.unsafeToForeign $ encodingToNode encoding, withFileTypes: true }

type ReaddirDirentBufferOptions = { recursive :: Boolean }

readdirDirentBufferOptionsDefault :: ReaddirDirentBufferOptions
readdirDirentBufferOptionsDefault = { recursive: false }

readdirDirentBufferOptionsToInternal :: ReaddirDirentBufferOptions -> ReaddirOptionsInternal
readdirDirentBufferOptionsToInternal { recursive } = { recursive, encoding: Foreign.unsafeToForeign "buffer", withFileTypes: true }

---------
type ReadFileOptionsInternal =
  { encoding :: Nullable String -- if null - returns Buffer, if encoding string - String https://nodejs.org/docs/latest/api/fs.html#fsreadfilepath-options-callback
  , flag :: String
  -- , signal :: Nullable AbortSignal
  }

type ReadFileStringOptions = { flag :: FileFlags, encoding :: Encoding }

readFileStringOptionsDefault :: ReadFileStringOptions
readFileStringOptionsDefault = { flag: R, encoding: UTF8 }

readFileStringOptionsToInternal :: ReadFileStringOptions -> ReadFileOptionsInternal
readFileStringOptionsToInternal { flag, encoding } = { flag: fileFlagsToNode flag, encoding: Nullable.notNull $ encodingToNode encoding }

type ReadFileBufferOptions = { flag :: FileFlags }

readFileBufferOptionsDefault :: ReadFileBufferOptions
readFileBufferOptionsDefault = { flag: R }

readFileBufferOptionsToInternal :: ReadFileBufferOptions -> ReadFileOptionsInternal
readFileBufferOptionsToInternal { flag } = { flag: fileFlagsToNode flag, encoding: Nullable.null }

---------
type WriteFileOptionsInternal =
  { encoding :: Nullable String -- The encoding option is ignored if data is a buffer
  , mode :: String
  , flag :: String -- See support of file system flags. Default: 'w'.
  , flush :: Boolean -- If all data is successfully written to the file, and flush is true, fs.fsync() is used to flush the data. Default: false.
  -- , signal :: Nullable AbortSignal -- allows aborting an in-progress writeFile
  }

type WriteFileStringOptions =
  { encoding :: Encoding
  , mode :: Perms
  , flag :: FileFlags
  , flush :: Boolean
  }

writeFileStringOptionsDefault :: WriteFileStringOptions
writeFileStringOptionsDefault =
  { encoding: UTF8
  , mode: mkPerms (read + write) (read + write) (read + write)
  , flag: W
  , flush: false
  }

writeFileStringOptionsToInternal :: WriteFileStringOptions -> WriteFileOptionsInternal
writeFileStringOptionsToInternal { encoding, mode, flag, flush } = { encoding: Nullable.notNull $ encodingToNode encoding, mode: permsToString mode, flag: fileFlagsToNode flag, flush }

type WriteFileBufferOptions =
  { mode :: Perms
  , flag :: FileFlags
  , flush :: Boolean
  }

writeFileBufferOptionsDefault :: WriteFileBufferOptions
writeFileBufferOptionsDefault =
  { mode: mkPerms (read + write) (read + write) (read + write)
  , flag: W
  , flush: false
  }

writeFileBufferOptionsToInternal :: WriteFileBufferOptions -> WriteFileOptionsInternal
writeFileBufferOptionsToInternal { mode, flag, flush } = { mode: permsToString mode, flag: fileFlagsToNode flag, flush, encoding: Nullable.null }

---------
type AppendFileOptionsInternal =
  { encoding :: Nullable String -- The encoding option is ignored if data is a buffer
  , mode :: String
  , flag :: String -- See support of file system flags. Default: 'w'.
  , flush :: Boolean -- If all data is successfully written to the file, and flush is true, fs.fsync() is used to flush the data. Default: false.
  }

type AppendFileStringOptions =
  { encoding :: Encoding
  , mode :: Perms
  , flag :: FileFlags
  , flush :: Boolean
  }

appendFileStringOptionsDefault :: AppendFileStringOptions
appendFileStringOptionsDefault =
  { encoding: UTF8
  , mode: mkPerms (read + write) (read + write) (read + write)
  , flag: A
  , flush: false
  }

appendFileStringOptionsToInternal :: AppendFileStringOptions -> AppendFileOptionsInternal
appendFileStringOptionsToInternal { encoding, mode, flag, flush } = { encoding: Nullable.notNull $ encodingToNode encoding, mode: permsToString mode, flag: fileFlagsToNode flag, flush }

type AppendFileBufferOptions =
  { mode :: Perms
  , flag :: FileFlags
  , flush :: Boolean
  }

appendFileBufferOptionsDefault :: AppendFileBufferOptions
appendFileBufferOptionsDefault =
  { mode: mkPerms (read + write) (read + write) (read + write)
  , flag: A
  , flush: false
  }

appendFileBufferOptionsToInternal :: AppendFileBufferOptions -> AppendFileOptionsInternal
appendFileBufferOptionsToInternal { mode, flag, flush } = { mode: permsToString mode, flag: fileFlagsToNode flag, flush, encoding: Nullable.null }

---------
type FdReadOptionsInternal =
  { buffer :: Nullable Buffer -- Default: `Buffer.alloc(16384)`
  , offset :: BufferOffset -- Default: `0`
  , length :: Nullable BufferLength -- Default: buffer.byteLength - offset
  , position :: Nullable FilePosition
  }

type FdReadOptions =
  { buffer :: Maybe Buffer
  , offset :: BufferOffset
  , length :: Maybe BufferLength
  , position :: Maybe FilePosition -- If position is null or -1 , data will be read from the current file position, and the file position will be updated. If position is a non-negative integer, the file position will be unchanged.
  }

fdReadOptionsDefault :: FdReadOptions
fdReadOptionsDefault =
  { buffer: Nothing
  , offset: 0
  , length: Nothing
  , position: Nothing
  }

fdReadOptionsToInternal :: FdReadOptions -> FdReadOptionsInternal
fdReadOptionsToInternal { buffer, offset, length, position } = { buffer: Nullable.toNullable buffer, offset, length: Nullable.toNullable length, position: Nullable.toNullable position }

---------
type FdWriteOptionsInternal =
  { offset :: BufferOffset -- Default: `0`
  , length :: Nullable BufferLength -- Default: buffer.byteLength - offset
  , position :: Nullable FilePosition
  }

type FdWriteOptions =
  { offset :: BufferOffset
  , length :: Maybe BufferLength
  , position :: Maybe FilePosition -- If position is null or -1 , data will be write from the current file position, and the file position will be updated. If position is a non-negative integer, the file position will be unchanged.
  }

fdWriteOptionsDefault :: FdWriteOptions
fdWriteOptionsDefault =
  { offset: 0
  , length: Nothing
  , position: Nothing
  }

fdWriteOptionsToInternal :: FdWriteOptions -> FdWriteOptionsInternal
fdWriteOptionsToInternal { offset, length, position } = { offset, length: Nullable.toNullable length, position: Nullable.toNullable position }

-------------------

type CpOptionsInternal =
  { dereference :: Boolean
  , errorOnExist :: Boolean -- Whether to dereference symlinks
  -- if null - will throw "TypeError [ERR_INVALID_ARG_TYPE]: The "options.filter" property must be of type function. Received null"
  , filter :: Undefinable (Fn2 FilePath FilePath Boolean)
  , force :: Boolean
  , mode :: CopyMode -- Modifiers for copy operation
  , preserveTimestamps :: Boolean -- Preserve timestamps from source
  , recursive :: Boolean -- Copy directories recursively
  , verbatimSymlinks :: Boolean -- Skip path resolution for symlinks
  }

data CpForce = CpForce_False | CpForce_TrueWithoutErrorOnExit | CpForce_TrueWithErrorOnExit

type CpDirOptions =
  { dereference :: Boolean
  , filter :: Maybe (FilePath -> FilePath -> Boolean)
  , force :: CpForce
  , mode :: CopyMode
  , preserveTimestamps :: Boolean
  , verbatimSymlinks :: Boolean
  }

type CpFileOptions =
  { dereference :: Boolean
  , force :: CpForce
  , mode :: CopyMode
  , preserveTimestamps :: Boolean
  , verbatimSymlinks :: Boolean
  }

cpDirOptionsDefault :: CpDirOptions
cpDirOptionsDefault =
  { dereference: false
  , filter: Nothing
  , force: CpForce_TrueWithoutErrorOnExit
  , mode: copyFile_NO_FLAGS
  , preserveTimestamps: false
  , verbatimSymlinks: false
  }

cpFileOptionsDefault :: CpFileOptions
cpFileOptionsDefault =
  { dereference: false
  , force: CpForce_TrueWithoutErrorOnExit
  , mode: copyFile_NO_FLAGS
  , preserveTimestamps: false
  , verbatimSymlinks: false
  }

cpDirOptionsToCpOptionsInternal :: CpDirOptions -> CpOptionsInternal
cpDirOptionsToCpOptionsInternal opts =
  { dereference: opts.dereference
  , errorOnExist: case opts.force of
      CpForce_TrueWithErrorOnExit -> true
      _ -> false
  , filter: Undefinable.toUndefinable $ map mkFn2 (opts.filter)
  , force: case opts.force of
      CpForce_False -> false
      _ -> true
  , mode: opts.mode
  , preserveTimestamps: opts.preserveTimestamps
  , recursive: true
  , verbatimSymlinks: opts.verbatimSymlinks
  }

cpFileOptionsToCpOptionsInternal :: CpFileOptions -> CpOptionsInternal
cpFileOptionsToCpOptionsInternal opts =
  { dereference: opts.dereference
  , errorOnExist: case opts.force of
      CpForce_TrueWithErrorOnExit -> true
      _ -> false
  , filter: Undefinable.undefined
  , force: case opts.force of
      CpForce_False -> false
      _ -> true
  , mode: opts.mode
  , preserveTimestamps: opts.preserveTimestamps
  , recursive: false
  , verbatimSymlinks: opts.verbatimSymlinks
  }

------------------

type GlobOptionsInternal filepathOrDirent = { cwd :: Nullable FilePath, exclude :: Nullable (filepathOrDirent -> Boolean), withFileTypes :: Boolean }

type GlobFilePathOptions = { cwd :: Maybe FilePath, exclude :: Maybe (FilePath -> Boolean) }

globFilePathOptionsDefault :: GlobFilePathOptions
globFilePathOptionsDefault = { cwd: Nothing, exclude: Nothing }

globFilePathOptionsToInternal :: GlobFilePathOptions -> GlobOptionsInternal FilePath
globFilePathOptionsToInternal { cwd, exclude } = { cwd: toNullable cwd, exclude: toNullable exclude, withFileTypes: false }

type GlobDirentOptions = { cwd :: Maybe FilePath, exclude :: Maybe (Dirent DirentNameString -> Boolean) }

globDirentOptionsDefault :: GlobDirentOptions
globDirentOptionsDefault = { cwd: Nothing, exclude: Nothing }

globDirentOptionsToInternal :: GlobDirentOptions -> GlobOptionsInternal (Dirent DirentNameString)
globDirentOptionsToInternal { cwd, exclude } = { cwd: toNullable cwd, exclude: toNullable exclude, withFileTypes: true }

------------------

type OpendirOptionsInternal = { encoding :: EncodingString, bufferSize :: Int, recursive :: Boolean }
type OpendirOptions = { encoding :: Encoding, bufferSize :: Int, recursive :: Boolean }

opendirOptionsDefault :: OpendirOptions
opendirOptionsDefault = { bufferSize: 32, recursive: false, encoding: UTF8 }

opendirOptionsToInternal :: OpendirOptions -> OpendirOptionsInternal
opendirOptionsToInternal { encoding, bufferSize, recursive } = { encoding: encodingToNode encoding, bufferSize, recursive }
