module Regex where

-- The AST for a regular expression.
data Regex
  = Lit Char -- a literal character
  | Dot -- any character
  | Seq Regex Regex -- r1 followed by r2
  | Alt Regex Regex -- r1 | r2
  | Star Regex -- r*
  | Plus Regex -- r+  (sync. sugar for r r*)
  | Quest Regex -- r?  (sync .sugar for r | empty)
  | Empty -- matches the empty string
  deriving (Show, Eq)
