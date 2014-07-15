{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, IncoherentInstances, OverloadedStrings #-}
module MathPrelude.Structures.Complex
	( Complex(..)
	, iu, realPart, imagPart
	, fromReal
	, fromArg, arg, primitiveRoot
	, fromPolar, toPolar
	, conjugate, normsq, normsq', norm
	) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Field
import MathPrelude.Structures.Module
import MathPrelude.Common.Transcendental
import MathPrelude.Common.Integral

-----------------------------------
--- Classes
-----------------------------------

data Complex a = a :+ a deriving (Eq)

-----------------------------------
--- Methods
-----------------------------------

iu :: Ring a => Complex a
iu = zero :+ one
realPart :: Complex a -> a
realPart (x:+_) = x
imagPart :: Complex a -> a
imagPart (_:+y) = y

fromReal :: Monoid a => a -> Complex a
fromReal r = r :+ mempty
fromArg :: Transcendental a => a -> Complex a
fromArg x = (cos x) :+ (sin x)
fromPolar :: (Ring a, Transcendental a) => a -> a -> Complex a
fromPolar r t = r .* (fromArg t)
toPolar ::  (Ord a, Field a, Transcendental a) => Complex a -> (a,a)
toPolar z = (sqrt $ normsq' z, arg z)

conjugate :: Abelian a => Complex a -> Complex a
conjugate (x:+y) = (x:+ (negate y))
normsq :: Ring a => Complex a -> Complex a
normsq z = z * conjugate z
normsq' :: Ring a => Complex a -> a
normsq' = realPart . normsq
norm :: (Ring a, Transcendental a) => Complex a -> a
norm = sqrt . realPart . normsq

arg :: (Ord a, Field a, Transcendental a) => Complex a -> a
arg (x :+ y)
	| x > zero = atan (y/x)
	| nearZero x && y > zero = pi * half
	| nearZero x && y < zero = negate $ pi * half
	| nearZero x && nearZero y = zero
	| x < zero && y > zero = atan (y/x) + pi
	| x < zero && y < zero = atan (y/x) - pi
	| x < zero && nearZero y = pi
	| otherwise = zero

primitiveRoot :: Int -> Complex Double
primitiveRoot n = fromArg (2*pi/fromIntegral n)
-----------------------------------
--- Instances
-----------------------------------

instance Functor Complex where
	fmap f (x :+ y) = f x :+ f y

instance (Show a, NumEq a, Monoid a)  => Show (Complex a) where
	show (x :+ y)
		| x >>~ y = P.show x
		| y >>~ x = P.show y ++ "i"
		| otherwise = "(" ++ P.show x ++ "+" ++ P.show y ++ "i)"

instance Monoid a => Monoid (Complex a) where
	mempty = mempty :+ mempty
	mappend (x:+y) (x':+y') = mappend x x' :+ mappend y y'

instance Abelian a => Abelian (Complex a) where
	negate (x:+y) = negate x :+ negate y

instance Ring a => Ring (Complex a) where
	one = one :+ zero
	(*) (x:+y) (x':+y') = (x*x' - y*y') :+ (x*y' + x'*y)
	fromInteger x = fromInteger x :+ zero

instance IntDom a => IntDom (Complex a)

instance Field a => Field (Complex a) where
	recip z@(x:+y) = (x/zz) :+ (negate y/zz)
		where zz = realPart $ normsq z

instance Num a => Num (Complex a) where
	abs = undefined
	signum = undefined

-- instance Ring s => Module (Complex s) s where
-- 	scale r (x :+ y) = (r*x) :+ (r*y)
instance Module m r => Module (Complex m) r where
	scale r z = map (scale r) z


half' :: Field a => Complex a
half' = fromReal $ half

instance (Ord a, Field a, Transcendental a) => Transcendental (Complex a) where
	pi = fromReal pi
	exp (x:+y) = (exp x) .* (fromArg y)
	log z = (log r) :+ t
		where (r,t) = toPolar z
	--(**) -- use default
	sqrt z
		| nearZero z = zero
		| otherwise = z ** half'
	--logBase -- use default
	sin (x:+y) = ((sin x)*(cosh y)) :+ ((cos x)*(sinh y))
	cos (x:+y) = ((cos x)*(cosh y)) :+ (negate $ (sin x)*(sinh y))
	--tan  -- use default
	asin z = (negate iu) * log (iu*z + sqrt(one - z*z))
	acos z = half' * pi - asin z
	atan z = half' * iu * (log (one - iu*z) - log (one + iu*z))
	atan2 x y
		| nearZero x = zero
		| otherwise = atan (y/x)
	sinh (x:+y) = ((sinh x)*(cos y)) :+ ((cosh x)*(sin y))
	cosh (x:+y) = ((cosh x)*(cos y)) :+ ((sinh x)*(sin y))
	--tanh  -- use default
	--ahyps -- use defaults

instance NumEq a => NumEq (Complex a) where
	(x1 :+ y1) =~ (x2 :+ y2) = (x1=~x2) && (y1=~y2)
	epsilon = epsilon :+ epsilon
	nearZero (a:+b)= nearZero a && nearZero b
	(>>~) (x:+y) (a:+b) = m >>~ a && m >>~ b
		where m = leastSmall [x,y]
