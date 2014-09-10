module Main where

import qualified Node.FS.Async as A
import qualified Node.FS.Sync as S
import Node.FS.Stats
import Control.Apply ((*>))
import Control.Monad.Eff.Exception
import Data.Either
import Debug.Trace
import Node.Encoding

main = do

  A.exists "examples\\Test.purs" $ \e ->
    trace $ "Test.purs exists? " ++ show e
  
  file <- S.readTextFile UTF8 "examples\\Test.purs"
  trace "\n\nreadTextFile sync result:"
  trace $ file

  catchException (\err -> do
    trace $ "Caught readTextFile error:\n" ++ show err
    return "") $ S.readTextFile UTF8 "examples\\does not exist"

  S.rename "tmp\\Test.js" "tmp\\Test1.js"

  S.truncate "tmp\\Test1.js" 1000

  stats <- S.stat "tmp\\Test1.js"
  trace "\n\nS.stat:"
  trace "isFile:"
  trace $ show $ isFile stats
  trace "isDirectory:"
  trace $ show $ isDirectory stats
  trace "isBlockDevice:"
  trace $ show $ isBlockDevice stats
  trace "isCharacterDevice:"
  trace $ show $ isCharacterDevice stats
  trace "isFIFO:"
  trace $ show $ isFIFO stats
  trace "isSocket:"
  trace $ show $ isSocket stats
  trace "isSymbolicLink:"
  trace $ show $ isSymbolicLink stats
  trace "modifiedTime:"
  trace $ show $ modifiedTime stats
  trace "accessedTime:"
  trace $ show $ accessedTime stats
  trace "statusChangedTime:"
  trace $ show $ statusChangedTime stats

  A.rename "tmp\\Test1.js" "tmp\\Test.js" $ \x -> do
    trace "\n\nrename result:"
    either (trace <<< show) (trace <<< show) x

    A.truncate "tmp\\Test.js" 10 $ \x -> do
      trace "\n\ntruncate result:"
      either (trace <<< show) (trace <<< show) x

  A.readFile "examples\\Test.purs" $ \x -> do
    trace "\n\nreadFile result:"
    either (trace <<< show) (trace <<< show) x

  A.readTextFile UTF8 "examples\\Test.purs" $ \x -> do
    trace "\n\nreadTextFile result:"
    either (trace <<< show) trace x

  A.stat "examples\\Test.purs" $ \x -> do
    trace "\n\nstat:"
    case x of
      Left err -> trace $ "Error:" ++ show err
      Right x' -> do
        trace "isFile:"
        trace $ show $ isFile x'
        trace "isDirectory:"
        trace $ show $ isDirectory x'
        trace "isBlockDevice:"
        trace $ show $ isBlockDevice x'
        trace "isCharacterDevice:"
        trace $ show $ isCharacterDevice x'
        trace "isFIFO:"
        trace $ show $ isFIFO x'
        trace "isSocket:"
        trace $ show $ isSocket x'
        trace "isSymbolicLink:"
        trace $ show $ isSymbolicLink x'
        trace "modifiedTime:"
        trace $ show $ modifiedTime x'
        trace "accessedTime:"
        trace $ show $ accessedTime x'
        trace "statusChangedTime:"
        trace $ show $ statusChangedTime x'
