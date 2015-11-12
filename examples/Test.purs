module Test where

import Prelude
import Data.Maybe
import Data.Either
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (log)
import Node.Encoding
import qualified Node.Buffer as Buffer
import qualified Node.Path as Path
import Unsafe.Coerce

import Node.FS
import Node.FS.Stats
import qualified Node.FS.Async as A
import qualified Node.FS.Sync as S

-- Cheat to allow `main` to type check. See also issue #5 in
-- purescript-exceptions.
catchException' ::
  forall a eff.
    (Error -> Eff (err :: EXCEPTION | eff) a)
    -> Eff (err :: EXCEPTION | eff) a
    -> Eff (err :: EXCEPTION | eff) a
catchException' = unsafeCoerce catchException

main = do
  let fp = Path.concat

  A.exists (fp ["examples", "Test.purs"]) $ \e ->
    log $ "Test.purs exists? " ++ show e

  file <- S.readTextFile UTF8 (fp ["examples", "Test.purs"])
  log "\n\nreadTextFile sync result:"
  log $ file

  catchException' (\err -> do
    log $ "Caught readTextFile error:\n" ++ show err
    return "") $ S.readTextFile UTF8 (fp ["examples", "does not exist"])

  -- If an error is thrown, it's probably EEXIST, so ignore it. Should
  -- really check this instead.
  catchException' (const (return unit)) (S.mkdir "tmp")

  S.writeTextFile ASCII (fp ["tmp", "Test.js"]) "console.log('hello world')"
  S.rename (fp ["tmp", "Test.js"]) (fp ["tmp", "Test1.js"])

  S.truncate (fp ["tmp", "Test1.js"]) 1000

  stats <- S.stat (fp ["tmp", "Test1.js"])
  log "\n\nS.stat:"
  log "isFile:"
  log $ show $ isFile stats
  log "isDirectory:"
  log $ show $ isDirectory stats
  log "isBlockDevice:"
  log $ show $ isBlockDevice stats
  log "isCharacterDevice:"
  log $ show $ isCharacterDevice stats
  log "isFIFO:"
  log $ show $ isFIFO stats
  log "isSocket:"
  log $ show $ isSocket stats
  log "isSymbolicLink:"
  log $ show $ isSymbolicLink stats
  log "modifiedTime:"
  log $ show $ modifiedTime stats
  log "accessedTime:"
  log $ show $ accessedTime stats
  log "statusChangedTime:"
  log $ show $ statusChangedTime stats

  A.rename (fp ["tmp", "Test1.js"]) (fp ["tmp", "Test.js"]) $ \x -> do
    log "\n\nrename result:"
    either (log <<< show) (log <<< show) x

    A.truncate (fp ["tmp", "Test.js"]) 10 $ \x -> do
      log "\n\ntruncate result:"
      either (log <<< show) (log <<< show) x

  A.readFile (fp ["examples", "Test.purs"]) $ \x -> do
    log "\n\nreadFile result:"
    either (log <<< show) (log <<< show) x

  A.readTextFile UTF8 (fp ["examples", "Test.purs"]) $ \x -> do
    log "\n\nreadTextFile result:"
    either (log <<< show) log x

  A.stat (fp ["examples", "Test.purs"]) $ \x -> do
    log "\n\nstat:"
    case x of
      Left err -> log $ "Error:" ++ show err
      Right x' -> do
        log "isFile:"
        log $ show $ isFile x'
        log "isDirectory:"
        log $ show $ isDirectory x'
        log "isBlockDevice:"
        log $ show $ isBlockDevice x'
        log "isCharacterDevice:"
        log $ show $ isCharacterDevice x'
        log "isFIFO:"
        log $ show $ isFIFO x'
        log "isSocket:"
        log $ show $ isSocket x'
        log "isSymbolicLink:"
        log $ show $ isSymbolicLink x'
        log "modifiedTime:"
        log $ show $ modifiedTime x'
        log "accessedTime:"
        log $ show $ accessedTime x'
        log "statusChangedTime:"
        log $ show $ statusChangedTime x'

  let fdFile = fp ["tmp", "FD.json"]
  fd0 <- S.fdOpen fdFile W (Just 420)
  buf0 <- Buffer.fromString "[ 42 ]" UTF8
  bytes0 <- S.fdAppend fd0 buf0
  S.fdFlush fd0
  S.fdClose fd0
  fd1 <- S.fdOpen fdFile R Nothing
  buf1 <- Buffer.create =<< Buffer.size buf0
  bytes1 <- S.fdNext fd1 buf1
  S.fdClose fd1
