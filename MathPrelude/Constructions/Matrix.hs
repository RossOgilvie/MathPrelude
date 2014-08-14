{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
module MathPrelude.Constructions.Matrix
  ( Mat()
  , dimMt
  , liftMt, liftMt2
  , transposeMt
  , toListMt, toListsMt
  , fromRows, fromCols, fromListMt, fromListsMt
  , diag, eye
  , rowEsh, reduceRowEsh, rank
  ) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude hiding (Vec)
import qualified Prelude as P

import MathPrelude.Algebraic.Module
import MathPrelude.Algebraic.Field
import MathPrelude.Constructions.Vector
import MathPrelude.Classes.Evaluable
import MathPrelude.Common.Integral

import GHC.TypeLits
import Data.Proxy
-----------------------------------
--- Vec
-----------------------------------
data Mat (n ∷ Nat) (m ∷ Nat) a = M [Vec m a]

-----------------------------------
--- Helpers
-----------------------------------
dimMt ∷ forall n m a. (KnownNat n, KnownNat m) ⇒ Mat n m a → (Integer, Integer)
dimMt _ = (natVal (Proxy ∷ Proxy n), natVal (Proxy ∷ Proxy m))

liftMt ∷ (a→b) → Mat n m a → Mat n m b
liftMt f (M as) = M (map (map f) as)

transposeMt ∷ KnownNat n ⇒ Mat n m a → Mat m n a
transposeMt (M vs) = M . map fromListV . transpose . map toListV $ vs

liftMt2 ∷ (a → b → c) → Mat n m a → Mat n m b → Mat n m c
liftMt2 f (M as) (M bs) = M (zipWith (liftV2 f) as bs)

toListsMt ∷ Mat n m a → [[a]]
toListsMt (M as) = map toListV as

toListMt ∷ Mat n m a → [a]
toListMt (M as) = concatMap toListV as

multMt ∷ (KnownNat q, KnownNat r, Ring a) ⇒ Mat p q a → Mat q r a → Mat p r a
multMt (M rows) m = M . map mkRow $ rows
  where
    M cols = transposeMt m
    mkRow = fromListV . zipWith dotV cols . repeat

actMt ∷ (KnownNat n, Ring a) ⇒ Mat n m a → Vec m a → Vec n a
actMt (M rows) = fromListV . zipWith dotV rows . repeat

-----------------------------------
--- Constructors
-----------------------------------

takeAndCount ∷ Integer → [a] → (Bool, [a])
takeAndCount n [] = (n <= 0, [])
takeAndCount n (x:xs)
  | n <= 0 = (True, [])
  | otherwise = let (b, ys) = takeAndCount (n-1) xs in (b, x:ys)

fromRows ∷ forall n m a. KnownNat n ⇒ [Vec m a] → Mat n m a
fromRows vs = if b then M vs' else error "fromRows: supplied list not long enough."
  where
    n' = natVal (Proxy ∷ Proxy n)
    (b, vs') = takeAndCount n' vs

fromCols ∷ forall n m a. KnownNat m ⇒ [Vec n a] → Mat n m a
fromCols = transposeMt . fromRows

fromListsMt ∷ forall n m a. (KnownNat n, KnownNat m) ⇒ [[a]] → Mat n m a
fromListsMt = fromRows . map fromListV

fromListMt ∷ forall n m a. (KnownNat n, KnownNat m) ⇒ [a] → Mat n m a
fromListMt = fromRows . map fromListV . mkRows
  where
    m' = fromInteger $ natVal (Proxy ∷ Proxy m)
    mkRows [] = []
    mkRows xs = let (h, t) = splitAt m' xs in h : mkRows t

zeroes ∷ Group a ⇒ Int → [a]
zeroes n = take n $ repeat zero

diag' ∷ Ring r ⇒ Int → [r] → [[r]]
diag' n = zipWith (\k d → zeroes k ++ [d] ++ zeroes (n-k-1)) [0..(n-1)]

diag ∷ forall n r. (KnownNat n, Ring r) ⇒ [r] → Mat n n r
diag = fromListsMt . diag' n'
  where n' = fromInteger $ natVal (Proxy ∷ Proxy n)

eye' ∷ Ring r ⇒ Int → [[r]]
eye' = flip diag' (repeat one)
eye ∷ forall n r. (KnownNat n, Ring r) ⇒ Mat n n r
eye = diag (repeat 1)

-----------------------------------
--- Operations
-----------------------------------
elim ∷ (KnownNat m, Field k) ⇒ Vec m k → Vec m k → Vec m k
elim v w
  | null q = w
  | otherwise = w - (c .* v)
  where
    v' = toListV v
    w' = toListV w
    (p,q) = partition nearZero v'
    c = w'!! length p

normalise v
  | null q = v
  | otherwise = head q ./ v
  where
    v' = toListV v
    (p,q) = partition nearZero v'

rowEsh ∷ (KnownNat n, KnownNat m, Field k) ⇒ Mat n m k → Mat n m k
rowEsh (M rows) = M $ rowEsh' rows
rowEsh' ∷ (KnownNat m, Field k) ⇒ [Vec m k] → [Vec m k]
rowEsh' [] = []
rowEsh' (row:rows) = nrow : (rowEsh' . map (elim nrow)) rows
  where nrow = normalise row

reduceRowEsh ∷ (KnownNat n, KnownNat m, Field k) ⇒ Mat n m k → Mat n m k
reduceRowEsh (M rows) = M . reverse . rowEsh' . reverse . rowEsh' $ rows

rank ∷ (KnownNat n, KnownNat m, Field k) ⇒ Mat n m k → Integer
rank m = let M m' = rowEsh m in (fst . dimMt $ m) - (fromIntegral . length . fst . partition nearZero $ m')

-----------------------------------
--- Instances
-----------------------------------
instance Show a ⇒ Show (Mat n m a) where
  show m = "M " ++ P.show (toListsMt m)
--
instance (KnownNat n, KnownNat m, Monoid a) ⇒ Monoid (Mat n m a) where
  mappend = liftMt2 mappend
  mempty = M (take n' $ repeat mempty)
    where n' = fromInteger $ natVal (Proxy ∷ Proxy n)

instance NumEq a ⇒ NumEq (Mat n m a) where
  (=~) (M vs) (M ws) = vs =~ ws

instance (KnownNat n, KnownNat m, Group a) ⇒ Group (Mat n m a) where
  negate = liftMt negate

instance (KnownNat n, KnownNat m, Abelian a) ⇒ Abelian (Mat n m a)

instance (KnownNat n, KnownNat m, Ring a) ⇒ Module (Mat n m a) a where
  scale b = liftMt (b*)

instance (KnownNat n, Ring a) ⇒ Evaluable (Mat n m a) (Vec m a) (Vec n a) where
  eval = actMt

instance (KnownNat n, Ring a) ⇒ Ring (Mat n n a) where
  (*) = multMt
  one = eye
