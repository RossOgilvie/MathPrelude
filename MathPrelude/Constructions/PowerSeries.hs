{-# LANGUAGE RebindableSyntax, UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module MathPrelude.Constructions.PowerSeries
  ( module MathPrelude.Classes.Field
  , module MathPrelude.Classes.Action
  , PS(..)
  , fromListPS ,toListPS
  , scalarPS, monomialPS, constPS
  , toPoly, fromPoly
  , toGenFunc, fromGenFunc
  , partialSumsPS
  ) where

-----------------------------------
--- Imports
-----------------------------------

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Classes.Module
import MathPrelude.Classes.Field
import MathPrelude.Classes.Derivation
import MathPrelude.Constructions.Polynomial

import MathPrelude.Classes.Integral
import MathPrelude.Classes.Transcendental
import MathPrelude.Extras.Convergence
import MathPrelude.Classes.Action


-----------------------------------
--- PowerSeries
-----------------------------------
-- | A power series
data PS a = PS [a] deriving Eq


-----------------------------------
--- Methods
-----------------------------------
-- | Construct a power series from a list of coefficients.
fromListPS ∷ Monoid a ⇒ [a] → PS a
fromListPS [] = zero
fromListPS ls = PS ls

-- | Destruct a power series to a list of coefficients.
toListPS ∷ Monoid a ⇒ PS a → [a]
toListPS (PS xs) = xs

-- | Construct a scalar polyseries
scalarPS ∷ a → PS a
scalarPS x = PS [x]

-- | Construct a monomial power series in the given degree
monomialPS ∷ Monoid a ⇒ Int → a → PS a
monomialPS d c
  | d < 0 = zero
  | otherwise = PS $ replicate d zero

-- | Extract the constant coefficient of a power series.
constPS ∷ Monoid a ⇒ PS a → a
constPS (PS []) = zero
constPS (PS (x:_)) = x

-- if you allow partial sums which add zero, then it breaks the otherwise useful converge trick. if you don't then you fail on PS with an infinite streak of zeroes. This latter case may be addressed another time by keeping track of degree

-- complication. a small tail element looks the same as a zero. take a pragmatic approach, and don't look at the first 100 terms of the power series
ps_tail_fudge = 50
-- | Evaluate a power series.
evalPS ∷ Ring a ⇒ PS a → a → a
evalPS ps = converge . drop ps_tail_fudge . partialSumsPS ps

partialSumsPS ∷ Ring a ⇒ PS a → a → [a]
partialSumsPS (PS xs) pt = partialSums $ zipWith (*) xs powers
  where powers = iterate (pt*) 1

-- | Truncate a power series to a polynomial of given degree.
toPoly ∷ Monoid a ⇒ Int → PS a → Poly a
toPoly n = fromListP . take n . toListPS

-- | Construct a finite power series from a given polynomial.
fromPoly ∷ Monoid a ⇒ Poly a → PS a
fromPoly = fromListPS . toListP

-- | Use a power series as a generating function.
toGenFunc ∷ Monoid a ⇒ PS a → (Int → a)
toGenFunc (PS y) n
  | null y' = zero
  | otherwise = head y'
  where y' = drop n y

-- | Construct a power series from a generating function.
fromGenFunc ∷ (Int → a) → PS a
fromGenFunc f = PS [f x | x <- [0..]]

-----------------------------------
--- Instances
-----------------------------------

instance Functor PS where
  fmap f (PS xs) = PS (map f xs)

instance (Show a, Ring a) ⇒ Show (PS a) where
  show = refined_show
  -- show = guts

instance NumEq a ⇒ NumEq (PS a) where
  (=~) (PS xs') (PS ys') = (=~) xs' ys'
  -- epsilon = scalarPS epsilon
  -- nearZero (PS xs) = and . map nearZero $ xs
  -- (>>~) (PS os) (PS xs) = and . map (smallL os) $ xs
instance Ring a ⇒ Derivation (PS a) where
  derive (PS p) = PS $ zipWith (*) (tail p) (map fromInteger [1..])
instance Field a ⇒ Integration (PS a) where
  integrate (PS p) = PS $ 0 : zipWith (/) p (map fromInteger [1..])
instance Ring a ⇒ Action (PS a) a a where
  act = evalPS


instance Monoid a ⇒ Monoid (PS a) where
  mempty = scalarPS mempty
  mappend = liftPS2 mappend

instance Group a ⇒ Group (PS a) where
  negate = map negate
  (-) p q = mappend p (negate q)
instance Abelian a ⇒ Abelian (PS a) where

instance Ring a ⇒ Ring (PS a) where
  one = scalarPS one
  (*) = mul

instance CRing a ⇒ CRing (PS a)
instance IntDom a ⇒ IntDom (PS a)

instance Ring r ⇒ Module (PS r) r where
  scale r = map (r*)

instance (Field a, NumEq a) ⇒ Field (PS a) where
  (/) = division

-----------------------------------
--- Routines
-----------------------------------
mul (PS xs) (PS ys) = PS $ mul' xs ys 0

mul' xs ys n = cauchy : mul' xs ys (n+1)
  where
    xs' = take (n+1) xs
    ys' = take (n+1) ys
    lx = length xs'
    ly = length ys'
    ys'' = replicate (n+1 - ly) zero ++ reverse ys'
    cauchy = sum $ zipWith (*) xs' ys''

-- the correct power is calculated here
division ∷ Field a ⇒ PS a → PS a → PS a
division (PS xs) (PS ys) = if deg_xs >= deg_ys then result else error "Power series division impossible: would result in negative powers."
  where
    deg_xs = length . takeWhile nearZero $ xs
    deg_ys = length . takeWhile nearZero $ ys
    xs' = drop deg_xs xs ++ repeat 0
    ys' = drop deg_ys ys ++ repeat 0
    result = shiftPower (deg_xs - deg_ys) $ PS $ division' xs' ys'

-- this builds the resultant list
division' ∷ Field a ⇒ [a] → [a] → [a]
division' xs ys = zs
  where
    y = head ys
    ys' = tail ys
    listTaker ∷ [b] → [[b]]
    listTaker ls = zipWith take [0..] (repeat ls)
    zss = listTaker zs
    yss = map reverse $ listTaker ys'
    cauchys = map sum $ zipWith (zipWith (*)) zss yss
    zs = zipWith (\xn cn → (xn - cn)/y) xs cauchys

liftPS2 ∷ (a → a → a) → PS a → PS a → PS a
liftPS2 f (PS xs) (PS ys) = PS $ liftPS2' f xs ys
liftPS2' f xs [] = xs
liftPS2' f [] ys = ys
liftPS2' f (x:xs) (y:ys) = f x y : liftPS2' f xs ys

-- new_divMod ∷ (Field a, Module (Poly a) a) ⇒ Poly a → Poly a → (Poly a, Poly a)
-- new_divMod p q = (Poly (combineP' d), m)
--   where
--     (d,m) = div' p q
--     -- div' ∷ Field a ⇒ Poly a → Poly a → ([(Int,a)],Poly a)
--     div' p q
--       | deg < 0 = ([(0,mempty)], p)
--       | deg == 0 = ([(0, factor)], r)
--       | otherwise = (p' ++ [(deg, factor)], r')
--         where
--           dp = degreeP p
--           deg = dp - degreeP q
--           factor = leadingP p / leadingP q
--           r = removeTerm dp $ p - shiftPower deg (filterP $ factor .* q)
--           (p',r') = div' r q
refined_show ∷ (Show a, Ring a) ⇒ PS a → String
refined_show p = s --if s /= "" then s else "0"
  where s = refined_show' p
refined_show' ∷ (Show a, Ring a) ⇒ PS a → String
refined_show' (PS xs) = intercalate " + " $ zipWith show_m [0..] xs
show_m ∷ (Show a, Ring a) ⇒ Int → a → String
show_m n x
  | n == 0 = P.show x
  | n == 1 && (x =~ one) = "x"
  | n == 1 = P.show x ++ "x"
  | x =~ one = "x^" ++ P.show n
  | otherwise = P.show x ++ "x^" ++ P.show n

-----------------------------------
--- Internal Stuff
-----------------------------------



guts (PS xs) = P.show xs

-- removeTerm ∷ Monoid a ⇒ Int → PS a → PS a
-- removeTerm d (PS xs) = PS $ front ++ back
--   where
--     front = take d xs
--     back = if length front == d then zero:drop (d+1) xs else []
--
shiftPower ∷ Monoid a ⇒ Int → PS a → PS a
shiftPower d (PS xs)
  | d < 0 = PS $ drop d xs
  | otherwise = PS $ replicate d zero ++ xs
