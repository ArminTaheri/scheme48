module Main where

import Interpreter 
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runSchemeRepl
    1 -> runSchemeEval . head $ args
    _ -> putStrLn "Please input 0 args for repl or 1 to evaluate."
