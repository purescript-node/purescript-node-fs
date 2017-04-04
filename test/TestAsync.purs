module TestAsync where

import Prelude (Unit, show, bind, discard, (<>), ($))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console(log,CONSOLE())

import Node.FS (FileFlags(..), FS)
import Node.FS.Async as A
import Node.Path as FP
import Node.Buffer as B

-- exercise the file descriptor based async IO functions

main ::forall e. Eff (fs::FS, console::CONSOLE, buffer::B.BUFFER |e) Unit
main = do
  let path1 = FP.concat( ["test", "TestAsync.purs"] )
      path2 = FP.concat( ["test", "TestAsync.purs.partial"] )

  buf <- B.create 1000

  A.fdOpen path1 R Nothing $ \a -> case a of
    (Left err) -> log ("err:" <> show err)
    (Right fd) -> do
      log ("opened " <> path1)
      A.fdNext fd buf $ \b -> case b of
        (Left err) -> log ("err:" <> show err)
        (Right n8bits) -> do
          log ("read " <> show n8bits)
          A.fdOpen path2 W Nothing $ \c -> case c of
            (Left err) -> log ("err:" <> show err)
            (Right fd2) -> do
              log ("opened " <> path2)
              A.fdAppend fd2 buf $ \d -> case d of
                (Left err) -> log ("err:" <> show err)
                (Right nbytes) ->  do
                  log ("wrote " <> show nbytes)
                  A.fdClose fd2 $ \e -> case e of
                    (Left err) -> log ("err:" <> show err)
                    (Right _) ->  do
                      log ("closed " <> path2)
                      A.fdClose fd $ \f -> case f of
                        (Left err) -> log ("err:" <> show err)
                        (Right _) ->  do
                          log ("closed " <> path1)
                          A.unlink path2 $ \g -> case g of
                            (Left err) -> log ("err:" <> show err)
                            (Right _) ->  log ("unlinked " <> path2)
