module Main where

import Prelude
import Data.Either
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Console(log,CONSOLE())
import Control.Monad.Eff.Exception

import Node.FS
import qualified Node.FS.Async as A
import qualified Node.Path as FP
import qualified Node.Buffer as B 

-- exercise the file descriptor based async IO functions

main :: Eff _ Unit
main = do
  let path1 = FP.concat( ["examples", "TestAsync.purs"] )
      path2 = FP.concat( ["examples", "TestAsync.purs.partial"] )

  buf <- B.create 1000

  A.fdOpen path1 R Nothing $ \v -> case v of
    (Left err) -> log ("err:" ++ show err)
    (Right fd) -> do
      log ("opened " ++ path1)
      A.fdNext fd buf $ \v -> case v of
        (Left err) -> log ("err:" ++ show err)
        (Right nbytes) -> do
          log ("read " ++ show nbytes)
          A.fdOpen path2 W Nothing $ \v -> case v of
            (Left err) -> log ("err:" ++ show err)
            (Right fd2) -> do
              log ("opened " ++ path2)
              A.fdAppend fd2 buf $ \v -> case v of
                (Left err) -> log ("err:" ++ show err)
                (Right nbytes) ->  do
                  log ("wrote " ++ show nbytes)
                  A.fdClose fd2 $ \v -> case v of
                    (Left err) -> log ("err:" ++ show err)
                    (Right _) ->  do
                      log ("closed " ++ path2)
                      A.fdClose fd $ \v -> case v of
                        (Left err) -> log ("err:" ++ show err)
                        (Right _) ->  do
                          log ("closed " ++ path1)
                          A.unlink path2 $ \v -> case v of
                            (Left err) -> log ("err:" ++ show err)
                            (Right _) ->  log ("unlinked " ++ path2)
