{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-|
Module      : Symmetric Group
Description : A data type for permutations
-}
module MathPrelude.Extras.SymmetricGroup
  (  module MathPrelude.Classes.Group
  -- * Constructors
  , Perm()
  , fromTable, transp, cyc
  , toTable
  -- * Methods
  , parity, size
  , symm
  )  where

import           BasicPrelude
import qualified Prelude                          as P

import           MathPrelude.Classes.Action
import           MathPrelude.Classes.Group
import           MathPrelude.Classes.Ring
import           MathPrelude.Extras.Combinatorics

------------------------------
--- Data
------------------------------
-- | Typesym representing a cycle in cycle notation.
type Cycle = [Int]
-- | A Permutation sotred as the product of disjoint cycles.
newtype Perm = P [Cycle]

------------------------------
---  Constructors
------------------------------
-- | Construct a permutation from a table. The input is the bottom line of a permutation table.
fromTable ∷ [Int] → Perm
fromTable ls = P . fromTable' [1..(length ls)] $ ls

-- | Aux function to make a permutation by picking out an unused number from ns and constructing its cycle.
fromTable' [] ls = []
fromTable' ns ls
  | length c == 1 = fromTable' (ns \\ c) ls
  | otherwise = c : fromTable' (ns \\ c) ls
  where c = makeCycle (head ns) ls

-- | Given a number and a table, find its cycle.
makeCycle ∷ Int → [Int] -> Cycle
makeCycle x ls =  (x :) . takeWhile (/= x) . tail . iterate f $ x
  where f y = ls !! (y-1)

-- | Make a permutation table from a given Permutation.
toTable ∷ Perm → [Int]
toTable p = map (p$$) ([1..(size p)]::[Int])

-- | The transposition of the two elements.
transp :: Int → Int → Perm
transp m n
  | m == n = P []
  | m < n = P [[m,n]]
  | otherwise = P [[n,m]]

-- | Construct a cyclic permutation.
-- Repeated elements are ignored. eg [1,2,2,3] becomes (1 2 3).
cyc ∷ [Int] → Perm
cyc xs = P [dedupe xs]

------------------------------
--- Methods
------------------------------
-- | Act a cycle on an element.
actCycle ∷ Cycle → Int → Int
actCycle [] y = y
actCycle ls y = actCycle' (ls ++ [head ls]) y
-- | Aux function for actCycle
actCycle' [] y = y
actCycle' [l] y = y
actCycle' (l:l':ls) y
  | l == y = l'
  | otherwise = actCycle' (l':ls) y

-- | Given a cycle, the canonical version is the one shifted to start with the least element.
canonical ∷ Cycle → Cycle
canonical [] = []
canonical xs = zs ++ reverse ys
  where
    m = minimum xs
    (ys,zs) = takeDrop (/=m) xs

-- | Like takeWhile and dropWhile combined.
takeDrop ∷ (a → Bool) → [a] → ([a], [a])
takeDrop p (x:xs)
  | p x = let (ys,zs) = takeDrop p xs in (x:ys,zs)
  | otherwise = ([],x:xs)

-- | Calculate the sign of a cycle. Cycles of even length are odd, and vice versa.
parityC ∷ Cycle → Int
parityC = negate . sign . length

-- | Calculate the sign of a permutation, ie whether it is composed of an even or odd number of transpositions.
parity ∷ Perm → Int
parity (P cs) = product . map parityC $ cs

-- | To which S_n does the permutation belong?
size ∷ Perm → Int
size (P []) = 0
size (P cs) = maximum . map maximum $ cs

-- | The inverse cycle.
invert ∷ Cycle → Cycle
invert [] = []
invert (x:xs) = x : reverse xs

-- | Remove duplicates from a list.
dedupe [] = []
dedupe (x:xs) = x : dedupe (filter (/=x) xs)

-- | Construct all the permutations of a given size.
symm ∷ Int → [Perm]
symm n = map fromTable $ permutations [1..n]
------------------------------
--- Instances
------------------------------
-- | Show a cycle
showC xs = ('(' :) . (++ ")") . concat . intersperse " " . map P.show $ xs

instance Show Perm where
  show (P []) = "()"
  show (P cs) = concatMap showC cs

instance Action Perm Int Int where
  act (P []) y = y
  act (P (c:cs)) y = P cs $$ actCycle c y

instance Monoid Perm where
  mempty = P []
  mappend p q = fromTable $ map ((p$$).(q$$)) [1..m]
    where m = max (size p) (size q)

instance NumEq Perm where
  (=~) (P xs) (P ys) = xs == ys

instance Group Perm where
  negate (P []) = P []
  negate (P cs) = P (map invert cs)
