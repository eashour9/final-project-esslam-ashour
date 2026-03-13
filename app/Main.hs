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
    let processLine line =
          let ms = findMatches nfa line
           in if null ms then Nothing else Just (highlight line ms)
    mapM_ putStrLn [out | Just out <- map processLine (lines input)]

-- Wrap matched spans in bold red ANSI color codes.
-- spans is a sorted, non-overlapping list of (start, end) pairs.
-- "\ESC[1;31m" sets bold+red; "\ESC[0m" resets all attributes.
-- These are standard ANSI/VT100 terminal escape sequences:
--   https://en.wikipedia.org/wiki/ANSI_escape_code
highlight :: String -> [(Int, Int)] -> String
highlight line spans = go 0 line spans
  where
    red s = "\ESC[1;31m" ++ s ++ "\ESC[0m"

    go _ rest [] = rest
    go pos (c : cs) ms@((s, e) : rest)
      | pos < s = c : go (pos + 1) cs ms
      | pos == s =
          let matchLen = e - s
              matched = take matchLen (c : cs)
              after = drop matchLen (c : cs)
           in red matched ++ go e after rest
      | otherwise = c : go (pos + 1) cs ms
    go _ [] _ = []
