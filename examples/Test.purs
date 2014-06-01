module Main where

import Node.FS.Async
import Node.FS.Stats
import Control.Apply ((*>))
import Data.Either
import Debug.Trace
import Node.Encoding

trace' x = trace x *> return unit

main = do

  readFile "examples\\Test.purs" $ \x -> do
    trace "\n\nreadFile result:"
    either trace' (trace' <<< show) x
    
  readTextFile UTF8 "examples\\Test.purs" $ \x -> do
    trace "\n\nreadTextFile result:"
    either trace' trace' x

  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, isFile result:"
    either trace' (trace' <<< show <<< isFile) x

  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, isDirectory result:"
    either trace' (trace' <<< show <<< isDirectory) x

  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, isBlockDevice result:"
    either trace' (trace' <<< show <<< isBlockDevice) x

  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, isCharacterDevice result:"
    either trace' (trace' <<< show <<< isCharacterDevice) x

  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, isFIFO result:"
    either trace' (trace' <<< show <<< isFIFO) x

  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, isSocket result:"
    either trace' (trace' <<< show <<< isSocket) x

  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, isSymbolicLink result:"
    either trace' (trace' <<< show <<< isSymbolicLink) x
    
  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, modifiedTime result:"
    either trace' (trace' <<< show <<< modifiedTime) x
    
  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, accessedTime result:"
    either trace' (trace' <<< show <<< accessedTime) x
    
  stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat, statusChangedTime result:"
    either trace' (trace' <<< show <<< statusChangedTime) x
