module Test.Node.FS.OpendirAndDir where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Options (opendirOptionsDefault, rmOptionsDefault)
import Node.FS.Aff as A
import Node.FS.Dir.Aff (close, entries, read)
import Node.FS.Dirent (Dirent, DirentNameString)
import Node.FS.Perms (permsAll)
import Node.Path (FilePath)
import Node.Path as Path
import Test.Assert (assertEqual)

outerTmpDir :: FilePath
outerTmpDir = Path.concat [ "tmp", "dir-entries-test" ]

prepare :: Aff Unit
prepare = do
  A.rm' outerTmpDir (rmOptionsDefault { recursive = true, force = true })
  A.mkdir' outerTmpDir { recursive: true, mode: permsAll }
  A.writeTextFile UTF8 (Path.concat [ outerTmpDir, "1.txt" ]) "1"
  A.writeTextFile UTF8 (Path.concat [ outerTmpDir, "2.txt" ]) "2"
  A.mkdir $ Path.concat [ outerTmpDir, "dir1" ]
  A.writeTextFile UTF8 (Path.concat [ outerTmpDir, "dir1", "3.txt" ]) "3"
  A.writeTextFile UTF8 (Path.concat [ outerTmpDir, "dir1", "4.txt" ]) "4"

test1 :: Aff Unit
test1 = do
  dir <- A.opendir' outerTmpDir (opendirOptionsDefault { recursive = true })
  liftEffect $ assertEqual
    { actual: show dir
    , expected: "Dir {}"
    }
  files' <- entries dir
  liftEffect $ assertEqual
    { actual: show files'
    , expected:
        """[Dirent {
  name: 'dir1',
  parentPath: 'tmp/dir-entries-test',
  path: 'tmp/dir-entries-test',
  [Symbol(type)]: 2
},Dirent {
  name: '1.txt',
  parentPath: 'tmp/dir-entries-test',
  path: 'tmp/dir-entries-test',
  [Symbol(type)]: 1
},Dirent {
  name: '2.txt',
  parentPath: 'tmp/dir-entries-test',
  path: 'tmp/dir-entries-test',
  [Symbol(type)]: 1
},Dirent {
  name: '3.txt',
  parentPath: 'tmp/dir-entries-test/dir1',
  path: 'tmp/dir-entries-test/dir1',
  [Symbol(type)]: 1
},Dirent {
  name: '4.txt',
  parentPath: 'tmp/dir-entries-test/dir1',
  path: 'tmp/dir-entries-test/dir1',
  [Symbol(type)]: 1
}]"""
    }
  liftEffect $ log $ show files'
  try (entries dir) >>= \(eitherFile :: Either Error (Array (Dirent DirentNameString))) -> liftEffect $ assertEqual
    { actual: String.take 74 $ show eitherFile
    , expected: "(Left Error [ERR_DIR_CLOSED]: Directory handle was closed\n    at #readImpl"
    }

test2 :: Aff Unit
test2 = do
  dir <- A.opendir' outerTmpDir (opendirOptionsDefault { recursive = false })
  read dir >>= \file -> liftEffect $ assertEqual
    { actual: show file
    , expected: "(Just Dirent {\n  name: 'dir1',\n  parentPath: 'tmp/dir-entries-test',\n  path: 'tmp/dir-entries-test',\n  [Symbol(type)]: 2\n})"
    }
  read dir >>= \file -> liftEffect $ assertEqual
    { actual: show file
    , expected: "(Just Dirent {\n  name: '1.txt',\n  parentPath: 'tmp/dir-entries-test',\n  path: 'tmp/dir-entries-test',\n  [Symbol(type)]: 1\n})"
    }
  read dir >>= \file -> liftEffect $ assertEqual
    { actual: show file
    , expected: "(Just Dirent {\n  name: '2.txt',\n  parentPath: 'tmp/dir-entries-test',\n  path: 'tmp/dir-entries-test',\n  [Symbol(type)]: 1\n})"
    }
  read dir >>= \file -> liftEffect $ assertEqual
    { actual: show file
    , expected: "Nothing"
    }
  close dir >>= \file -> liftEffect $ assertEqual
    { actual: show file
    , expected: "unit"
    }
  try (close dir) >>= \(error :: Either Error Unit) -> liftEffect $ assertEqual
    { actual: String.take 74 $ show error
    , expected: "(Left Error [ERR_DIR_CLOSED]: Directory handle was closed\n    at Dir.close"
    }
  try (read dir) >>= \(eitherFile :: Either Error (Maybe (Dirent DirentNameString))) -> liftEffect $ assertEqual
    { actual: String.take 74 $ show eitherFile
    , expected: "(Left Error [ERR_DIR_CLOSED]: Directory handle was closed\n    at #readImpl"
    }

main :: Effect Unit
main = launchAff_ do
  prepare
  test1
  test2
