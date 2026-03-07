-- NFA simulation using Thompson's approach:
-- instead of backtracking, we track the *set* of all states the NFA
-- could currently be in and advance all of them together on each character.
module Simulate (findMatches) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NFA (NFA (..), NFAM, StateId, Trans (..))

-- Compute the epsilon-closure of a set of states using BFS.
epsClosure :: NFAM -> Set.Set StateId -> Set.Set StateId
epsClosure m = go
  where
    go states =
      let new =
            Set.fromList
              [ t | q <- Set.toList states, (Eps, t) <- Map.findWithDefault [] q m, Set.notMember t states
              ]
       in if Set.null new then states else go (Set.union states new)

-- Move from a set of states on one character (without epsilon closure).
step :: NFAM -> Set.Set StateId -> Char -> Set.Set StateId
step m qs c =
  Set.fromList
    [ t | q <- Set.toList qs, (On p, t) <- Map.findWithDefault [] q m, p c
    ]

-- Try matching starting at position i in s.
-- Returns the end position (exclusive) of the longest match, or Nothing.
matchAt :: NFA -> String -> Int -> Maybe Int
matchAt nfa s i = go initial (drop i s) i Nothing
  where
    m = nfaTrans nfa
    acc = nfaAccept nfa
    initial = epsClosure m (Set.singleton (nfaStart nfa))

    go qs str pos best
      | Set.null qs = best
      | otherwise =
          let best' = if Set.member acc qs then Just pos else best
           in case str of
                [] -> best'
                (c : cs) -> go (epsClosure m (step m qs c)) cs (pos + 1) best'

-- Find all non-overlapping leftmost-longest matches in a string.
findMatches :: NFA -> String -> [(Int, Int)]
findMatches nfa s = go 0
  where
    n = length s
    go i
      | i > n = []
      | otherwise = case matchAt nfa s i of
          Nothing -> go (i + 1)
          Just j
            | j > i -> (i, j) : go j -- advance past the match
            | otherwise -> go (i + 1) -- zero-length match, skip
