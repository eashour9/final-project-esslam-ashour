module Main where

import NFA (compile)
import Parser (parseRegex)
import Simulate (findMatches)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [pat] -> run pat
    _ -> do
      hPutStrLn stderr "usage: hgrep PATTERN"
      exitWith (ExitFailure 1)

run :: String -> IO ()
run pat = case parseRegex pat of
  Left err -> do
    hPutStrLn stderr ("hgrep: " ++ err)
    exitWith (ExitFailure 1)
  Right regex -> do
    let nfa = compile regex
    input <- getContents
    mapM_
      putStrLn
      [ line | line <- lines input, not (null (findMatches nfa line))
      ]
