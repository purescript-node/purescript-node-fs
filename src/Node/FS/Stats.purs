module Node.FS.Stats
  ( Stats
  , isFile
  , isDirectory
  , isBlockDevice
  , isCharacterDevice
  , isFIFO
  , isSocket
  , isSymbolicLink
  , dev
  , inode
  , mode
  , nlink
  , uid
  , gid
  , rdev
  , size
  , blkSize
  , blocks
  , accessedTimeMs
  , modifiedTimeMs
  , statusChangedTimeMs
  , birthtimeMs
  , accessedTime
  , modifiedTime
  , statusChangedTime
  , birthTime
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.JSDate (JSDate, toDateTime)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Partial.Unsafe (unsafeCrashWith)

foreign import data Stats :: Type

foreign import showStatsObj :: Stats -> String

instance showStats :: Show Stats where
  show s = "Stats " <> showStatsObj s

isBlockDevice :: Stats -> Boolean
isBlockDevice s = runFn1 isBlockDeviceImpl s

foreign import isBlockDeviceImpl :: Fn1 (Stats) (Boolean)

isCharacterDevice :: Stats -> Boolean
isCharacterDevice s = runFn1 isCharacterDeviceImpl s

foreign import isCharacterDeviceImpl :: Fn1 (Stats) (Boolean)

-- | Returns true if the <fs.Stats> object describes a file system directory.
-- | If the `fs.Stats`> object was obtained from `fs.lstat()`, 
-- | this method will always return `false``. This is because `fs.lstat()` 
-- | returns information about a symbolic link itself and not the path to which it resolves.
isDirectory :: Stats -> Boolean
isDirectory s = runFn1 isDirectoryImpl s

foreign import isDirectoryImpl :: Fn1 (Stats) (Boolean)

isFIFO :: Stats -> Boolean
isFIFO s = runFn1 isFIFOImpl s

foreign import isFIFOImpl :: Fn1 (Stats) (Boolean)

isFile :: Stats -> Boolean
isFile s = runFn1 isFileImpl s

foreign import isFileImpl :: Fn1 (Stats) (Boolean)

isSocket :: Stats -> Boolean
isSocket s = runFn1 isSocketImpl s

foreign import isSocketImpl :: Fn1 (Stats) (Boolean)

isSymbolicLink :: Stats -> Boolean
isSymbolicLink s = runFn1 isSymbolicLinkImpl s

foreign import isSymbolicLinkImpl :: Fn1 (Stats) (Boolean)

-- | The numeric identifier of the device containing the file.
dev :: Stats -> Number
dev s = runFn1 devImpl s

foreign import devImpl :: Fn1 (Stats) (Number)

-- | The file system specific "Inode" number for the file.
inode :: Stats -> Number
inode s = runFn1 inodeImpl s

foreign import inodeImpl :: Fn1 (Stats) (Number)

-- | A bit-field describing the file type and mode.
mode :: Stats -> Number
mode s = runFn1 modeImpl s

foreign import modeImpl :: Fn1 (Stats) (Number)

-- | The number of hard-links that exist for the file.
nlink :: Stats -> Number
nlink s = runFn1 nlinkImpl s

foreign import nlinkImpl :: Fn1 (Stats) (Number)

-- | The numeric user identifier of the user that owns the file (POSIX).
uid :: Stats -> Number
uid s = runFn1 uidImpl s

foreign import uidImpl :: Fn1 (Stats) (Number)

-- | The numeric group identifier of the group that owns the file (POSIX).
gid :: Stats -> Number
gid s = runFn1 gidImpl s

foreign import gidImpl :: Fn1 (Stats) (Number)

-- | A numeric device identifier if the file represents a device.
rdev :: Stats -> Number
rdev s = runFn1 rdevImpl s

foreign import rdevImpl :: Fn1 (Stats) (Number)

-- | The size of the file in bytes.
-- | If the underlying file system does not support getting the size of the file, this will be 0.
size :: Stats -> Number
size s = runFn1 sizeImpl s

foreign import sizeImpl :: Fn1 (Stats) (Number)

-- | The file system block size for i/o operations.
blkSize :: Stats -> Number
blkSize s = runFn1 blkSizeImpl s

foreign import blkSizeImpl :: Fn1 (Stats) (Number)

-- | The number of blocks allocated for this file.
blocks :: Stats -> Number
blocks s = runFn1 blocksImpl s

foreign import blocksImpl :: Fn1 (Stats) (Number)

accessedTimeMs :: Stats -> Milliseconds
accessedTimeMs s = runFn1 accessedTimeMsImpl s

foreign import accessedTimeMsImpl :: Fn1 (Stats) (Milliseconds)

modifiedTimeMs :: Stats -> Milliseconds
modifiedTimeMs s = runFn1 modifiedTimeMsImpl s

foreign import modifiedTimeMsImpl :: Fn1 (Stats) (Milliseconds)

statusChangedTimeMs :: Stats -> Milliseconds
statusChangedTimeMs s = runFn1 statusChangedTimeMsImpl s

foreign import statusChangedTimeMsImpl :: Fn1 (Stats) (Milliseconds)

birthtimeMs :: Stats -> Milliseconds
birthtimeMs s = runFn1 birthtimeMsImpl s

foreign import birthtimeMsImpl :: Fn1 (Stats) (Milliseconds)

accessedTime :: Stats -> DateTime
accessedTime s = case toDateTime $ runFn1 accessedTimeImpl s of
  Just d -> d
  Nothing -> unsafeCrashWith $ "Impossible: `accessedTime` returned invalid DateTime value."

foreign import accessedTimeImpl :: Fn1 (Stats) (JSDate)

modifiedTime :: Stats -> DateTime
modifiedTime s = case toDateTime $ runFn1 modifiedTimeImpl s of
  Just d -> d
  Nothing -> unsafeCrashWith $ "Impossible: `modifiedTime` returned invalid DateTime value."

foreign import modifiedTimeImpl :: Fn1 (Stats) (JSDate)

statusChangedTime :: Stats -> DateTime
statusChangedTime s = case toDateTime $ runFn1 statusChangedTimeImpl s of
  Just d -> d
  Nothing -> unsafeCrashWith $ "Impossible: `statusChangedTime` returned invalid DateTime value."

foreign import statusChangedTimeImpl :: Fn1 (Stats) (JSDate)

birthTime :: Stats -> DateTime
birthTime s = case toDateTime $ runFn1 birthTimeImpl s of
  Just d -> d
  Nothing -> unsafeCrashWith $ "Impossible: `birthTime` returned invalid DateTime value."

foreign import birthTimeImpl :: Fn1 (Stats) (JSDate)
