module Node.FS
  ( FS (..)
  , Callback (..)
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
  , Stats (..)
  , stat
  , isFile
  , isDirectory
  , isBlockDevice
  , isCharacterDevice
  , isFIFO
  , isSocket
  ) where

import Control.Monad.Eff
import Data.Either
import Data.Foreign
import Data.Function
import Data.Maybe
import Node.Buffer (Buffer(..))
import Node.Encoding
import Node.Path (FilePath())
import Global (Error(..))

type JSCallback = Fn2 Foreign

type StatsObj =
  { dev :: Number
  , mode :: Number
  , nlink :: Number
  , uid :: Number
  , gid :: Number
  , rdev :: Number
  , ino :: Number
  , size :: Number
  -- , atime :: DateTime
  -- , mtime :: DateTime
  -- , ctime :: DateTime
  , isFile :: Fn0 Boolean
  , isDirectory :: Fn0 Boolean
  , isBlockDevice :: Fn0 Boolean
  , isCharacterDevice :: Fn0 Boolean
  , isFIFO :: Fn0 Boolean
  , isSocket :: Fn0 Boolean
  }

foreign import runCallbackEff
  "function runCallbackEff (f) {\
  \  return f(); \
  \}" :: forall eff a. Eff eff a -> a

handleCallback :: forall eff a b. (Callback eff a) -> JSCallback a Unit
handleCallback f = mkFn2 $ \err x -> runCallbackEff $ f case parseForeign read err of
  Left err -> Left $ "handleCallback failed: " ++ err
  Right (Just err') -> Left $ show (err' :: Error)
  Right Nothing -> Right x

foreign import fs "var fs = require('fs');" :: 
  { readFile :: forall a b opts. Fn3 FilePath { | opts } (JSCallback a b) Unit
  , writeFile :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit Unit) Unit
  , stat :: forall a. Fn2 FilePath (JSCallback StatsObj a) Unit
  }

-- |
-- Effect type for file system usage.
-- 
foreign import data FS :: !

-- |
-- Type synonym for callback functions.
--
type Callback eff a = Either String a -> Eff eff Unit

-- |
-- Reads the entire contents of a file returning the result as a raw buffer.
-- 
readFile :: forall eff. FilePath 
                     -> Callback eff Buffer
                     -> Eff (fs :: FS | eff) Unit

readFile file cb = return $ runFn3
  fs.readFile file {} (handleCallback cb)

-- |
-- Reads the entire contents of a text file with the specified encoding.
-- 
readTextFile :: forall eff. Encoding 
                         -> FilePath 
                         -> Callback eff String 
                         -> Eff (fs :: FS | eff) Unit

readTextFile encoding file cb = return $ runFn3
  fs.readFile file { encoding: show encoding } (handleCallback cb)

-- |
-- Writes a buffer to a file.
-- 
writeFile :: forall eff. FilePath 
                      -> Buffer 
                      -> Callback eff Unit 
                      -> Eff (fs :: FS | eff) Unit

writeFile file buff cb = return $ runFn4 
  fs.writeFile file buff {} (handleCallback cb)

-- |
-- Writes text to a file using the specified encoding.
-- 
writeTextFile :: forall eff. Encoding 
                          -> FilePath 
                          -> String 
                          -> Callback eff Unit 
                          -> Eff (fs :: FS | eff) Unit

writeTextFile encoding file buff cb = return $ runFn4 
  fs.writeFile file buff { encoding: show encoding } (handleCallback cb)

-- |
-- Stats wrapper to provide better interface to the underlying methods.
-- 
data Stats = Stats StatsObj

foreign import showStatsObj
  "function showStatsObj (obj) {\
  \  return require('util').inspect(obj);\
  \}" :: StatsObj -> String

instance showStats :: Show Stats where
  show (Stats o) = "Stats " ++ showStatsObj o

-- |
-- Gets file statistics.
-- 
stat :: forall eff. FilePath 
                 -> Callback eff Stats
                 -> Eff (fs :: FS | eff) Unit

stat file cb = return $ runFn2
  fs.stat file (handleCallback $ cb <<< (<$>) Stats)
  
isFile :: Stats -> Boolean
isFile (Stats s) = runFn0 s.isFile

isDirectory :: Stats -> Boolean
isDirectory (Stats s) = runFn0 s.isDirectory

isBlockDevice :: Stats -> Boolean
isBlockDevice (Stats s) = runFn0 s.isBlockDevice

isCharacterDevice :: Stats -> Boolean
isCharacterDevice (Stats s) = runFn0 s.isCharacterDevice

isFIFO :: Stats -> Boolean
isFIFO (Stats s) = runFn0 s.isFIFO

isSocket :: Stats -> Boolean
isSocket (Stats s) = runFn0 s.isSocket
