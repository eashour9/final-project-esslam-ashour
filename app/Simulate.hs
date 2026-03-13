-- NFA simulation using Thompson's set-of-states approach.
-- Instead of backtracking, we track the full set of states the NFA could
-- currently be in and advance all of them together on each character.
-- This guarantees linear time in the input length.
--
-- The simulation algorithm (epsilon-closure + step loop) is described in:
--   Russ Cox, "Regular Expression Matching Can Be Simple And Fast"
--   https://swtch.com/~rsc/regexp/regexp1.html
-- The extension for AtStart/AtEnd anchors (treating them as
-- position-conditional epsilon transitions) is my own addition.
module Simulate (findMatches) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NFA (NFA (..), NFAM, StateId, Trans (..))

-- Compute the epsilon-closure of a set of states.
-- pos is the current character position in the line (0-indexed).
-- lineLen is the total length of the line.
-- AtStart transitions only fire when pos == 0.
-- AtEnd transitions only fire when pos == lineLen.
epsClosure :: NFAM -> Int -> Int -> Set.Set StateId -> Set.Set StateId
epsClosure m pos lineLen = go
  where
    go states =
      let new =
            Set.fromList
              [ t
                | q <- Set.toList states,
                  (tr, t) <- Map.findWithDefault [] q m,
                  Set.notMember t states,
                  case tr of
                    Eps -> True
                    AtStart -> pos == 0
                    AtEnd -> pos == lineLen
                    On _ -> False
              ]
       in if Set.null new then states else go (Set.union states new)

-- Move from a set of states on one character (no epsilon closure).
step :: NFAM -> Set.Set StateId -> Char -> Set.Set StateId
step m qs c =
  Set.fromList
    [ t | q <- Set.toList qs, (On p, t) <- Map.findWithDefault [] q m, p c
    ]

-- Try matching starting at position i in s.
-- lineLen is the total length of s (passed in so we don't recompute it).
-- Returns the end position (exclusive) of the longest match, or Nothing.
matchAt :: NFA -> String -> Int -> Int -> Maybe Int
matchAt nfa s lineLen i = go initial (drop i s) i Nothing
  where
    m = nfaTrans nfa
    acc = nfaAccept nfa
    initial = epsClosure m i lineLen (Set.singleton (nfaStart nfa))

    go qs str pos best
      | Set.null qs = best
      | otherwise =
          let best' = if Set.member acc qs then Just pos else best
           in case str of
                [] -> best'
                (c : cs) ->
                  let next = epsClosure m (pos + 1) lineLen (step m qs c)
                   in go next cs (pos + 1) best'

-- Find all non-overlapping leftmost-longest matches in a string.
findMatches :: NFA -> String -> [(Int, Int)]
findMatches nfa s = go 0
  where
    n = length s
    go i
      | i > n = []
      | otherwise = case matchAt nfa s n i of
          Nothing -> go (i + 1)
          Just j
            | j > i -> (i, j) : go j -- advance past the match
            | otherwise -> go (i + 1) -- zero-length match, skip
