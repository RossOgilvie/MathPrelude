{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.ContinuedFractions
  (
  ) where

----------------------------------
-- Imports
----------------------------------
import BasicPrelude
import MathPrelude.Algebraic.EuclideanDomain
import MathPrelude.Algebraic.Field
import MathPrelude.Constructions.Ratio
import MathPrelude.Common.Integral
import MathPrelude.Common.Rational

data CF = CF [Integer] deriving Show


makeCF :: Rational -> CF
makeCF q = CF $ makeCF' (numerator q') (denominator q')
  where q' = simplifyQ q
makeCF' a b
  | b =~ 0 = []
  | b =~ 1 = [a]
  | otherwise = d : makeCF' b m
  where (d,m) = divMod a b

liftCF f (CF l) = CF $ f l
liftCF2 f (CF l) (CF l') = CF $ f l l'

leadingCF (CF (x:_)) = x

evalCF (CF ls)= evalCF' ls
evalCF' :: Field a => [Integer] -> a
evalCF' [x] = fromInteger x
evalCF' (x:xs) = fromInteger x + 1 / evalCF' xs

instance NumEq (CF) where
  (=~) (CF ls) (CF ls') = ls =~ ls'
  epsilon = undefined
  nearZero = undefined
  (>>~) = undefined


instance Monoid (CF) where
  mempty = CF [zero]
  mappend = liftCF2 add

instance Abelian (CF) where
  -- negate (x:%y) = negate x :% y
  -- (-) (x:%y) (x':%y') = (x*y' - x'*y) :% (y*y')

instance Ring (CF) where
  one = CF [one]
  (*) = liftCF2 mul

add [x] (y:ys) = (x+y):ys
add (x:xs) [y] = (x+y):xs
add (x:xs) (y:ys) = (x+y): mul (mul xs ys) (recip' $ add xs ys)

mul [x] ys = map (x*) ys
mul xs [y] = map (y*) xs
mul (x:xs) (y:ys) = (x*y): mul (mul xs ys) (recip' $ add (add xs' ys') [1])
  where
    xs' = map (x*) xs
    ys' = map (y*) ys

instance IntDom (CF)

instance Field (CF) where
  recip = liftCF recip'
  -- (/) (x:%y) (x':%y') = (x*y') :% (y*x')

recip' (x:xs)
  | x =~ 0 = xs
  | otherwise = 0:x:xs

instance CharZero CF where
  fromRational' = makeCF

instance Q CF where
  toRational = evalCF
