{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module MathPrelude.Structures.PowerSeries
  ( module MathPrelude.Structures.Field
  , module MathPrelude.Structures.EuclideanDomain
  , module MathPrelude.Structures.Module
  ) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Module
import MathPrelude.Structures.Field
import MathPrelude.Structures.EuclideanDomain
import MathPrelude.Structures.Derivation
import MathPrelude.Structures.Polynomial

import MathPrelude.Common.Integral
import MathPrelude.Common.Transcendental
import MathPrelude.Extras.Sequence


-----------------------------------
--- PowerSeries
-----------------------------------
data PS a = PS [a]

-----------------------------------
--- Instances
-----------------------------------

instance Functor PS where
  fmap f (PS xs) = PS (map f xs)

instance (Show a, Ring a) => Show (PS a) where
  show = refined_show
  -- show = guts

instance Eq a => Eq (PS a) where
  (==) (PS xs) (PS ys) = xs == ys

instance NumEq a => NumEq (PS a) where
  (=~) (PS xs') (PS ys') = (=~) xs' ys'
  epsilon = scalarPS epsilon
  nearZero (PS xs) = and . map nearZero $ xs
  (>>~) (PS os) (PS xs) = and . map (smallL os) $ xs

instance Monoid a => Monoid (PS a) where
  mempty = scalarPS mempty
  mappend = liftPS2 mappend

instance Abelian a => Abelian (PS a) where
  negate = map negate
  (-) p q = mappend p (negate q)

instance Ring a => Ring (PS a) where
  one = scalarPS one
  (*) = mul

instance IntDom a => IntDom (PS a)

-- instance (Ring a, NumEq a) => Module (PS a) a where
--   scale r p = map (r*) p
instance Module m r => Module (PS m) r where
  scale r p = map (scale r) p

instance (Field a, NumEq a) => EuclideanDomain (PS a) where
  stdUnit = scalarPS . constPS
  stdAssociate p = p /. constPS p
  -- div = old_div
  -- p `mod` q = p - (p `div` q)*q
  -- divMod = new_divMod
  divMod = undefined

instance Ring a => Derivation (PS a) where
  derive (PS p) = PS $ zipWith (*) (tail p) (map fromInteger [1..])

instance Field a => Integration (PS a) where
  integrate (PS p) = PS $ 0 : zipWith (/) p (map fromInteger [1..])

-----------------------------------
--- Routines
-----------------------------------
mul (PS xs) (PS ys) = PS $ mul' xs ys 0

mul' xs ys n
  | lx + ly > n+1 = cauchy : mul' xs ys (n+1)
  | otherwise = []
  where
    xs' = take (n+1) xs
    ys' = take (n+1) ys
    lx = length xs'
    ly = length ys'
    ys'' = replicate (n+1 - ly) zero ++ reverse ys'
    cauchy = sum $ zipWith (*) xs' ys''

liftPS2 :: (a -> a -> a) -> PS a -> PS a -> PS a
liftPS2 f (PS xs) (PS ys) = PS $ liftPS2' f xs ys
liftPS2' f xs [] = xs
liftPS2' f [] ys = ys
liftPS2' f (x:xs) (y:ys) = f x y : liftPS2' f xs ys

-- new_divMod :: (Field a, Module (Poly a) a) => Poly a -> Poly a -> (Poly a, Poly a)
-- new_divMod p q = (Poly (combineP' d), m)
--   where
--     (d,m) = div' p q
--     -- div' :: Field a => Poly a -> Poly a -> ([(Int,a)],Poly a)
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
refined_show :: (Show a, Ring a) => PS a -> String
refined_show p = s --if s /= "" then s else "0"
  where s = refined_show' p
refined_show' :: (Show a, Ring a) => PS a -> String
refined_show' (PS xs) = intercalate " + " $ zipWith show_m [0..] xs
show_m :: (Show a, Ring a) => Int -> a -> String
show_m n x
  | n == 0 = P.show x
  | n == 1 && (x =~ one) = "x"
  | n == 1 = P.show x ++ "x"
  | x =~ one = "x^" ++ P.show n
  | otherwise = P.show x ++ "x^" ++ P.show n

-----------------------------------
--- Methods
-----------------------------------
fromListPS :: Monoid a => [a] -> PS a
fromListPS [] = zero
fromListPS ls = PS ls

scalarPS :: a -> PS a
scalarPS x = PS [x]

monomialPS :: Monoid a => Int -> a -> PS a
monomialPS d c
  | d < 0 = zero
  | otherwise = PS $ replicate d zero

constPS :: Monoid a => PS a -> a
constPS (PS []) = zero
constPS (PS (x:_)) = x

-- if you allow partial sums which add zero, then it breaks the otherwise useful converge trick. if you don't then you fail on PS with an infinity streak of zero. this latter case may be address another time by keeping track of degree

-- complication. a small tail element looks the same as a zero. take a pragmatic approach, and don't look at the first 100 terms of the power series
ps_tail_fudge = 50
evalPS :: Ring a => PS a -> a -> a
evalPS ps = converge . drop ps_tail_fudge . partialSumsPS ps

partialSumsPS :: Ring a => PS a -> a -> [a]
partialSumsPS (PS xs) pt = partialSumsPS' 0 xs pt 0
partialSumsPS' _ [] _ total = repeat total
partialSumsPS' n (x:xs) pt total = current : partialSumsPS' (n+1) xs pt current
  where current = total + x*pt^n


-- termwiseP :: (Ring a, NumEq a) => (Int -> a -> a)) -> PS a -> PS a
-- termwiseP f (Poly xs) = Poly . sortSimplifyP' . map (uncurry f) $ xs

toListPS :: Monoid a => PS a -> [a]
toListPS (PS xs) = xs

-- y' = f y
firstODE :: Field a => (PS a -> PS a) -> a -> PS a
firstODE f y0 =
  let
    y = PS $ defIntegrate y0 y'
    PS y' = f y
  in y

secODE :: Field a => (PS a -> PS a -> PS a) -> a -> a -> PS a
secODE f y0 y0' =
  let
    y = defIntegrate y0 y'
    y' = defIntegrate y0' y''
    PS y'' = f (PS y) (PS y')
  in PS y

defIntegrate c p = c : zipWith (/) p (map fromInteger [1..])

toPoly :: Monoid a => Int -> PS a -> Poly a
toPoly n = poly . take n . toListPS

toGenFunc :: Monoid a => PS a -> (Int -> a)
toGenFunc (PS y) n
  | null y' = zero
  | otherwise = head y'
  where y' = drop n y

fromGenFunc :: (Int -> a) -> PS a
fromGenFunc f = PS [f x | x <- [0..]]
-----------------------------------
--- Internal Stuff
-----------------------------------

expPS :: PS Double
expPS = firstODE id 1

sinPS :: PS Double
sinPS = secODE (\y _ -> - y) 0 1

guts (PS xs) = P.show xs

removeTerm :: Monoid a => Int -> PS a -> PS a
removeTerm d (PS xs) = PS $ front ++ back
  where
    front = take d xs
    back = if length front == d then zero:drop (d+1) xs else []

shiftPower :: Monoid a => Int -> PS a -> PS a
shiftPower d (PS xs)
  | d < 0 = PS $ drop d xs
  | otherwise = PS $ replicate d zero ++ xs

ps1, ps2 :: PS Double
ps1 = PS [1,1]
ps2 = PS [4,5,6,1,2,3]
