{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}
module Data.MathPrelude.Complex where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Field
import Data.MathPrelude.Module
import Data.MathPrelude.Ring
import Data.MathPrelude.Abelian
-----------------------------------
--- Complex
-----------------------------------

data Complex a = a :+ a deriving (Show, Eq)

-----------------------------------
--- Instances
-----------------------------------

instance Monoid a => Monoid (Complex a) where
	mempty = mempty :+ mempty
	mappend (x:+y) (x':+y') = mappend x x' :+ mappend y y'

instance Abelian a => Abelian (Complex a) where
	negate (x:+y) = negate x :+ negate y

instance Ring a => Ring (Complex a) where
	one = one :+ zero
	(*) (x:+y) (x':+y') = (x*x' - y*y') :+ (x*y' + x'*y)

instance IntDom a => IntDom (Complex a)

instance Field a => Field (Complex a) where
	recip z@(x:+y) = (x/zz) :+ (negate y/zz)
		where zz = realPart $ normsq z

instance Num a => Num (Complex a) where
	abs = undefined
	signum = undefined

instance Ring s => Module (Complex s) s where
	scale r (x :+ y) = (r*x) :+ (r*y)


instance (Ord a, Field a, Floating a) => Floating (Complex a) where
	pi = fromReal pi
	exp (x:+y) = (exp x) .* (fromArg y)
	log z = (log r) :+ t
		where (r,t) = toPolar z
	--(**) -- use default
	sqrt z
		| nearZero z = zero
		| otherwise = z ** half
			where half = fromReal $ recip $ fromInteger 2
	--logBase -- use default
	sin (x:+y) = ((sin x)*(cosh y)) :+ ((cos x)*(sinh y))
	cos (x:+y) = ((cos x)*(cosh y)) :+ (negate $ (sin x)*(sinh y))
	--tan  -- use default
	--asin = P.asin
	--acos = P.acos
	--atan = P.atan
	--atan2 = P.atan2
	sinh (x:+y) = ((sinh x)*(cos y)) :+ ((cosh x)*(sin y))
	cosh (x:+y) = ((cosh x)*(cos y)) :+ ((sinh x)*(sin y))
	--tanh  -- use default
	--asinh = P.asinh
	--acosh = P.acosh
	--atanh = P.atanh

	epsilon = fromReal epsilon
	nearZero = nearZero . sqrt . normsq'


-----------------------------------
--- Methods
-----------------------------------

iu :: Ring a => Complex a
iu = zero :+ one
realPart :: Complex a -> a
realPart (x:+_) = x
imagPart :: Complex a -> a
imagPart (x:+_) = x

fromReal :: Monoid a => a -> Complex a
fromReal r = r :+ mempty
fromArg :: Floating a => a -> Complex a
fromArg x = (cos x) :+ (sin x)
fromPolar :: (Ring a, Floating a) => a -> a -> Complex a
fromPolar r t = r .* (fromArg t)
toPolar ::  (Ord a, Field a, Floating a) => Complex a -> (a,a)
toPolar z = (sqrt $ normsq' z, arg z)

conjugate :: Abelian a => Complex a -> Complex a
conjugate (x:+y) = (x:+ (negate y))
normsq :: Ring a => Complex a -> Complex a
normsq z = z * conjugate z
normsq' :: Ring a => Complex a -> a
normsq' = realPart . normsq

arg :: (Ord a, Field a, Floating a) => Complex a -> a
arg (x :+ y)
	| x > zero = atan (y/x)
	| nearZero x && y > zero = pi/ fromInteger 2
	| nearZero x && y < zero = negate $ pi/ fromInteger 2
	| nearZero x && nearZero y = zero
	| x < zero && y > zero = atan (y/x) + pi
	| x < zero && y < zero = atan (y/x) - pi
	| x < zero && nearZero y = pi
	| otherwise = zero