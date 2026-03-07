module Parser (parseRegex) where

import Regex

-- Top-level entry point. Fails if there's leftover input after parsing.
parseRegex :: String -> Either String Regex
parseRegex s = case parseAlt s of
  Right (r, []) -> Right r
  Right (_, rest) -> Left ("unexpected: " ++ show rest)
  Left err -> Left err

-- Grammar (lowest to highest precedence):
--   alt    = seq ('|' seq)*
--   seq    = factor+
--   factor = atom ('*' | '+' | '?')?
--   atom   = '(' alt ')' | '.' | '\' c | <literal char>

parseAlt :: String -> Either String (Regex, String)
parseAlt s = do
  (r, rest) <- parseSeq s
  case rest of
    ('|' : rest') -> do
      (r2, rest'') <- parseAlt rest'
      Right (Alt r r2, rest'')
    _ -> Right (r, rest)

parseSeq :: String -> Either String (Regex, String)
parseSeq s = case parseFactor s of
  Left _ -> Right (Empty, s) -- empty sequence is fine
  Right (r, rest) ->
    if canContinue rest
      then case parseSeq rest of
        Right (r2, rs) -> Right (cat r r2, rs)
        Left err -> Left err
      else Right (r, rest)
  where
    canContinue [] = False
    canContinue (c : _) = c `notElem` ['|', ')']

parseFactor :: String -> Either String (Regex, String)
parseFactor s = do
  (r, rest) <- parseAtom s
  case rest of
    ('*' : rest') -> Right (Star r, rest')
    ('+' : rest') -> Right (Plus r, rest')
    ('?' : rest') -> Right (Quest r, rest')
    _ -> Right (r, rest)

parseAtom :: String -> Either String (Regex, String)
parseAtom [] = Left "unexpected end of pattern"
parseAtom ('(' : rest) = do
  (r, rest') <- parseAlt rest
  case rest' of
    (')' : rest'') -> Right (r, rest'')
    _ -> Left "missing closing ')'"
parseAtom ('.' : rest) = Right (Dot, rest)
parseAtom ('\\' : c : rest) = Right (Lit c, rest)
parseAtom ('\\' : _) = Left "trailing backslash"
parseAtom (c : rest)
  | c `elem` [')', '|', '*', '+', '?', ']'] = Left ("unexpected '" ++ [c] ++ "'")
  | otherwise = Right (Lit c, rest)

-- Smart constructor: don't build Seq nodes around Empty.
cat :: Regex -> Regex -> Regex
cat Empty r = r
cat r Empty = r
cat r1 r2 = Seq r1 r2
