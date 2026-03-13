-- Thompson's construction: compile a Regex into an NFA.
-- Each regex operator maps to a small fragment (one start, one accept),
-- and fragments are connected with epsilon transitions.
--
-- The construction and the fragment-wiring approach come from:
--   Russ Cox, "Regular Expression Matching Can Be Simple And Fast"
--   https://swtch.com/~rsc/regexp/regexp1.html
-- That article walks through exactly how to build each fragment and
-- how to compose them.
module NFA (StateId, Trans (..), NFAM, NFA (..), compile) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Regex

type StateId = Int

data Trans
  = Eps -- epsilon: free move, no input consumed
  | On (Char -> Bool) -- consume a character matching the predicate
  | AtStart -- zero-width: only fires at position 0 in the line
  | AtEnd -- zero-width: only fires at end of the line

type NFAM = Map.Map StateId [(Trans, StateId)]

data NFA = NFA
  { nfaStart :: StateId,
    nfaAccept :: StateId,
    nfaTrans :: NFAM
  }

-- An intermediate fragment during construction.
data Frag = Frag
  { fStart :: StateId,
    fAccept :: StateId,
    fTrans :: NFAM
  }

compile :: Regex -> NFA
compile r =
  let (f, _) = runState (build r) 0
   in NFA (fStart f) (fAccept f) (fTrans f)

type Build a = State Int a

fresh :: Build StateId
fresh = do n <- get; modify (+ 1); return n

edge :: NFAM -> StateId -> Trans -> StateId -> NFAM
edge m s t e = Map.insertWith (++) s [(t, e)] m

union2 :: NFAM -> NFAM -> NFAM
union2 = Map.unionWith (++)

build :: Regex -> Build Frag
build Empty = do
  s <- fresh
  t <- fresh
  return $ Frag s t (edge Map.empty s Eps t)
build (Lit c) = do
  s <- fresh
  t <- fresh
  return $ Frag s t (edge Map.empty s (On (== c)) t)
build Dot = do
  s <- fresh
  t <- fresh
  return $ Frag s t (edge Map.empty s (On (const True)) t)
-- character class: one transition that checks membership
build (Class neg items) = do
  s <- fresh
  t <- fresh
  return $ Frag s t (edge Map.empty s (On (matchClass neg items)) t)
-- \^ anchor: zero-width transition that only fires at position 0
build AnchorStart = do
  s <- fresh
  t <- fresh
  return $ Frag s t (edge Map.empty s AtStart t)
-- \$ anchor: zero-width transition that only fires at end of line
build AnchorEnd = do
  s <- fresh
  t <- fresh
  return $ Frag s t (edge Map.empty s AtEnd t)
-- f1.accept --eps--> f2.start
build (Seq r1 r2) = do
  f1 <- build r1
  f2 <- build r2
  let m = union2 (fTrans f1) (edge (fTrans f2) (fAccept f1) Eps (fStart f2))
  return $ Frag (fStart f1) (fAccept f2) m
-- new start fans out to both; both accept fan in to new end
build (Alt r1 r2) = do
  s <- fresh
  f1 <- build r1
  f2 <- build r2
  e <- fresh
  let m = union2 (fTrans f1) (fTrans f2)
      m1 = edge m s Eps (fStart f1)
      m2 = edge m1 s Eps (fStart f2)
      m3 = edge m2 (fAccept f1) Eps e
      m4 = edge m3 (fAccept f2) Eps e
  return $ Frag s e m4
-- s --eps--> f.start (try), s --eps--> e (skip)
-- f.accept --eps--> f.start (loop), f.accept --eps--> e (exit)
build (Star r) = do
  s <- fresh
  f <- build r
  e <- fresh
  let m = fTrans f
      m1 = edge m s Eps (fStart f)
      m2 = edge m1 s Eps e
      m3 = edge m2 (fAccept f) Eps (fStart f)
      m4 = edge m3 (fAccept f) Eps e
  return $ Frag s e m4
build (Plus r) = build (Seq r (Star r))
-- s --eps--> f.start (match), s --eps--> e (skip), f.accept --eps--> e
build (Quest r) = do
  s <- fresh
  f <- build r
  e <- fresh
  let m = fTrans f
      m1 = edge m s Eps (fStart f)
      m2 = edge m1 s Eps e
      m3 = edge m2 (fAccept f) Eps e
  return $ Frag s e m3

-- Check if a character matches a character class.
matchClass :: Bool -> [CharSetItem] -> Char -> Bool
matchClass neg items c =
  let hit = any (itemHit c) items
   in if neg then not hit else hit

-- Check if a character matches a single class item.
itemHit :: Char -> CharSetItem -> Bool
itemHit c (CSChar x) = c == x
itemHit c (CSRange lo hi) = c >= lo && c <= hi
