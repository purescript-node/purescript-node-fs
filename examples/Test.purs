module Main where

import Node.FS
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
    either trace' (trace' <<< show) x -- <<< isFile) x