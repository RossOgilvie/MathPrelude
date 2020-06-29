{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses#-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MathPrelude.Constructions.Matrix
  ( Mat()
  , dimMt
  , liftMt, liftMt2
  , transposeMt
  , toRowsMt, toColsMt, toListMt, toListsMt
  , fromRowsMt, fromColsMt, fromListMt, fromListsMt
  , diag, eye
  , rowEsh, reduceRowEsh, solveSystem, rank
  , rowEshApprox, reduceRowEshApprox, solveSystemApprox, rankApprox
  ) where

-----------------------------------
--- Imports
-----------------------------------
import MathPrelude hiding (Vec)
import qualified Prelude as P

import MathPrelude.Classes.VectorSpace
import MathPrelude.Classes.Field
import MathPrelude.Constructions.Vector
-- import MathPrelude.Classes.Action
import MathPrelude.Classes.Integral

import GHC.TypeLits
import Data.Proxy

import Control.Lens hiding (Action)
-----------------------------------
--- Vec
-----------------------------------
data Mat (n ∷ Nat) (m ∷ Nat) a = Mat [Vec m a]
    deriving (Eq)

-----------------------------------
--- Helpers
-----------------------------------
dimMt ∷ forall n m a. (KnownNat n, KnownNat m) ⇒ Mat n m a → (Integer, Integer)
dimMt _ = (natVal (Proxy ∷ Proxy n), natVal (Proxy ∷ Proxy m))

liftMt ∷ (a→b) → Mat n m a → Mat n m b
liftMt f (Mat as) = Mat (map (map f) as)

transposeMt ∷ KnownNat n ⇒ Mat n m a → Mat m n a
transposeMt (Mat vs) = Mat . map fromListV . transpose . map toListV $ vs

liftMt2 ∷ (a → b → c) → Mat n m a → Mat n m b → Mat n m c
liftMt2 f (Mat as) (Mat bs) = Mat (zipWith (liftV2 f) as bs)

toListsMt ∷ Mat n m a → [[a]]
toListsMt (Mat as) = map toListV as

toListMt ∷ Mat n m a → [a]
toListMt (Mat as) = concatMap toListV as

toRowsMt :: Mat n m a -> [Vec m a]
toRowsMt (Mat vs) = vs

toColsMt :: KnownNat n => Mat n m a -> [Vec n a]
toColsMt = toRowsMt . transposeMt

multMt ∷ (KnownNat q, KnownNat r, Ring a) ⇒ Mat p q a → Mat q r a → Mat p r a
multMt (Mat rows) m = Mat . map mkRow $ rows
  where
    Mat cols = transposeMt m
    mkRow = fromListV . zipWith dotV cols . repeat

actMt ∷ (KnownNat n, KnownNat m, Ring a) ⇒ Mat n m a → Vec m a → Vec n a
actMt (Mat rows) = fromListV . zipWith dotV rows . repeat













-----------------------------------
--- Constructors
-----------------------------------

takeAndCount ∷ Integer → [a] → (Bool, [a])
takeAndCount n xs = let n' = fromIntegral n; xs' = take n' xs in (length xs' == n', xs')

fromRowsMt ∷ forall n m a. KnownNat n ⇒ [Vec m a] → Mat n m a
fromRowsMt vs = if b then Mat vs' else error "fromRows: supplied list not long enough."
  where
    n' = natVal (Proxy ∷ Proxy n)
    (b, vs') = takeAndCount n' vs

fromColsMt ∷ forall n m a. KnownNat m ⇒ [Vec n a] → Mat n m a
fromColsMt = transposeMt . fromRowsMt

fromListsMt ∷ forall n m a. (KnownNat n, KnownNat m) ⇒ [[a]] → Mat n m a
fromListsMt = fromRowsMt . map fromListV

fromListMt ∷ forall n m a. (KnownNat n, KnownNat m) ⇒ [a] → Mat n m a
fromListMt = fromRowsMt . map fromListV . mkRows
  where
    m' = fromInteger $ natVal (Proxy ∷ Proxy m)
    mkRows [] = []
    mkRows xs = let (h, t) = splitAt m' xs in h : mkRows t

zeroes ∷ Group a ⇒ Int → [a]
zeroes n = let n' = fromIntegral n in take n' $ repeat zero

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
isZero ∷ (Monoid k, Eq k) ⇒ k → Bool
isZero = (== mempty)

elim ∷ Field k ⇒ (k → Bool) → [k] → [k] → [k]
elim eq v w
  | null q = w
  | otherwise = zipWith (\wi vi -> wi - c*vi) w v
  where
    (p,q) = partition eq v
    c = w !! length p

normaliseRow :: (Field k, Eq k) => [k] → [k]
normaliseRow = normaliseRow' isZero
normaliseRowApprox :: (Field k, Approx k) => [k] → [k]
normaliseRowApprox = normaliseRow' nearZero
normaliseRow' :: Field k => (k → Bool) → [k] → [k]
normaliseRow' eq v
  | null q = v
  | otherwise = map (/head q) v
  where
    (p,q) = partition eq v

rowEsh ∷ (KnownNat n, KnownNat m, Field k, Eq k) ⇒ Mat n m k → Mat n m k
rowEsh (toListsMt -> rows) = fromListsMt $ rowEsh' isZero rows
rowEshApprox ∷ (KnownNat n, KnownNat m, Field k, Approx k) ⇒ Mat n m k → Mat n m k
rowEshApprox (toListsMt -> rows) = fromListsMt $ rowEsh' nearZero rows
rowEsh' ∷ Field k ⇒ (k → Bool) → [[k]] → [[k]]
rowEsh' eq [] = []
rowEsh' eq (row:rows) = nrow : (rowEsh' eq . map (elim eq nrow)) rows
    where nrow = normaliseRow' eq row


reduceRowEsh ∷ (KnownNat n, KnownNat m, Field k, Eq k) ⇒ Mat n m k → Mat n m k
reduceRowEsh (toListsMt -> rows) = fromListsMt $ reduceRowEsh' isZero rows
reduceRowEshApprox ∷ (KnownNat n, KnownNat m, Field k, Approx k) ⇒ Mat n m k → Mat n m k
reduceRowEshApprox (toListsMt -> rows) = fromListsMt $ reduceRowEsh' nearZero rows
reduceRowEsh' ∷ Field k ⇒ (k → Bool) → [[k]] → [[k]]
reduceRowEsh' eq = reverse . rowEsh' eq . reverse . rowEsh' eq

rank ∷ (KnownNat n, KnownNat m, Field k, Eq k) ⇒ Mat n m k → Integer
rank m = let Mat m' = rowEsh m in (fst . dimMt $ m) - (fromIntegral . length . fst . partition isZero $ m')
rankApprox ∷ (KnownNat n, KnownNat m, Field k, Approx k) ⇒ Mat n m k → Integer
rankApprox m = let Mat m' = rowEshApprox m in (fst . dimMt $ m) - (fromIntegral . length . fst . partition nearZero $ m')

solveSystem :: (KnownNat n, KnownNat m, Field k, Eq k) => Mat n m k -> Vec n k -> Vec n k
solveSystem (map toListV . toColsMt -> cols) (toListV -> v) = fromListV . last . transpose . reduceRowEsh' isZero . transpose $ cols ++ [v]

solveSystemApprox :: (KnownNat n, KnownNat m, Field k, Approx k) => Mat n m k -> Vec n k -> Vec n k
solveSystemApprox (map toListV . toColsMt -> cols) (toListV -> v) = fromListV . last . transpose . reduceRowEsh' nearZero . transpose $ cols ++ [v]







-----------------------------------
--- Instances
-----------------------------------
instance Show a ⇒ Show (Mat n m a) where
  show m = "Mat " ++ P.show (toListsMt m)
--
instance (KnownNat n, KnownNat m, Semigroup a) ⇒ Semigroup (Mat n m a) where
  (<>) = liftMt2 (<>)
instance (KnownNat n, KnownNat m, Monoid a) ⇒ Monoid (Mat n m a) where
  mempty = fromRowsMt $ repeat mempty

instance (KnownNat n, KnownNat m, Approx a) ⇒ Approx (Mat n m a) where
  (=~) (Mat vs) (Mat ws) = vs =~ ws
  epsilon = fromRowsMt $ repeat epsilon

instance (KnownNat n, KnownNat m, Group a) ⇒ Group (Mat n m a) where
  negate = liftMt negate

instance (KnownNat n, KnownNat m, Abelian a) ⇒ Abelian (Mat n m a)

instance (KnownNat n, KnownNat m, Ring a) ⇒ Module (Mat n m a) a where
  scale b = liftMt (b*)

-- instance (KnownNat n, KnownNat m, Ring a) ⇒ Action (Mat n m a) (Vec m a) (Vec n a) where
--   act = actMt

instance (KnownNat n, Ring a) ⇒ Ring (Mat n n a) where
  (*) = multMt
  one = eye

instance Functor (Mat n m) where
  fmap f (Mat rows) = Mat . map (fmap f) $ rows
