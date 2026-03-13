module Regex where

-- A single item inside a character class bracket.
-- Either a literal character or an inclusive range like a-z.
data CharSetItem
  = CSChar Char
  | CSRange Char Char
  deriving (Show, Eq)

-- The AST for a regular expression.
data Regex
  = Lit Char               -- a literal character
  | Dot                    -- any single character
  | Class Bool [CharSetItem] -- character class; Bool = negated (^ prefix)
  | AnchorStart            -- ^ asserts we are at the start of the line
  | AnchorEnd              -- $ asserts we are at the end of the line
  | Seq Regex Regex        -- r1 followed by r2
  | Alt Regex Regex        -- r1 | r2
  | Star Regex             -- r*
  | Plus Regex             -- r+  (sugar for r r*)
  | Quest Regex            -- r?  (sugar for r | empty)
  | Empty                  -- matches the empty string
  deriving (Show, Eq)
