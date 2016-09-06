module Main where

import Interpreter 
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runSchemeRepl
    else runSchemeProgram args
