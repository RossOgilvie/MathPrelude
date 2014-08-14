{-# LANGUAGE RebindableSyntax, UnicodeSyntax, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, IncoherentInstances, UndecidableInstances #-}
module MathPrelude.Constructions.Complex
	( Complex(..)
	, display
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

import Data.Complex(Complex(..))

import MathPrelude.Algebraic.Field
import MathPrelude.Classes.Derivation
import MathPrelude.Classes.Evaluable
import MathPrelude.Algebraic.Module
import MathPrelude.Common.Transcendental
import MathPrelude.Common.Integral
import MathPrelude.Common.Rational

-----------------------------------
--- Classes
-----------------------------------

-- data LazyComplex a = a :+~ a deriving (Eq)

-----------------------------------
--- Methods
-----------------------------------
-- | Display a complex number bracketed in the common notational form.
display ∷ (Show a, NumEq a, Monoid a) ⇒ Complex a → Text
display (x :+ y)
	| nearZero y = show x
	| nearZero x = show y ++ "i"
	| otherwise = "(" ++ show x ++ "+" ++ show y ++ "i)"

-- | The imaginary unit, ie 0:+1.
iu ∷ Ring a ⇒ Complex a
iu = zero :+ one
-- | The real part of a complex number.
realPart ∷ Complex a → a
realPart (x:+_) = x
-- | The imaginary part of a complex number.
imagPart ∷ Complex a → a
imagPart (_:+y) = y

-- | Contruct a complex number from a real one.
fromReal ∷ Monoid a ⇒ a → Complex a
fromReal r = r :+ mempty
-- | Construct a unit norm complex from its argument.
fromArg ∷ Transcendental a ⇒ a → Complex a
fromArg x = cos x :+ sin x
-- | Construct a complex number from its polar representation (r,θ).
fromPolar ∷ (Ring a, Transcendental a) ⇒ a → a → Complex a
fromPolar r t = r .* fromArg t
-- | Export a complex number to its polar representation (r,θ).
toPolar ∷  (Ord a, Field a, Transcendental a) ⇒ Complex a → (a,a)
toPolar z = (sqrt $ normsq' z, arg z)

-- | Take the conjugate of a complex number, ie negate the imaginary part
conjugate ∷ Abelian a ⇒ Complex a → Complex a
conjugate (x:+y) = x :+ negate y
-- | Take the square of the norm of a complex number, expressed a (purely real) complex number
normsq ∷ Ring a ⇒ Complex a → Complex a
normsq z = z * conjugate z
-- | Take the square of the norm of a complex number, expressed a real number
normsq' ∷ Ring a ⇒ Complex a → a
normsq' = realPart . normsq
-- | Take the norm of a complex number, expressed a real number, ie sqrt 'normsq''
norm ∷ (Ring a, Transcendental a) ⇒ Complex a → a
norm = sqrt . realPart . normsq

-- | Compute the argument of a complex numer. The range is from -pi to pi. Arg 0 = 0. Arg (-x:+0) = pi.
arg ∷ (Ord a, Field a, Transcendental a) ⇒ Complex a → a
arg (x :+ y)
	| x > zero = atan (y/x)
	| nearZero x && y > zero = pi * half
	| nearZero x && y < zero = negate $ pi * half
	| nearZero x && nearZero y = zero
	| x < zero && y > zero = atan (y/x) + pi
	| x < zero && y < zero = atan (y/x) - pi
	| x < zero && nearZero y = pi
	| otherwise = zero

-- | Compute the primitive nth root of unity with least argument.
primitiveRoot ∷ Int → Complex Double
primitiveRoot n = fromArg (2*pi/fromIntegral n)


-----------------------------------
--- Instances
-----------------------------------

instance Functor Complex where
	fmap f (x :+ y) = f x :+ f y

instance Derivation a ⇒ Derivation (Complex a) where
	derive (x:+y) = derive x :+ derive y
instance Evaluable a b c ⇒ Evaluable (Complex a) b (Complex c) where
	eval (x:+y) p = eval x p :+ eval y p
instance NumEq a ⇒ NumEq (Complex a) where
	(x1 :+ y1) =~ (x2 :+ y2) = (x1 =~ x2) && (y1 =~ y2)
	-- epsilon = epsilon :+ epsilon
	-- nearZero (a:+b)= nearZero a && nearZero b
	-- (>>~) (x:+y) (a:+b) = m >>~ a && m >>~ b
	-- 	where m = leastSmall [x,y]


instance Monoid a ⇒ Monoid (Complex a) where
	mempty = mempty :+ mempty
	mappend (x:+y) (x':+y') = mappend x x' :+ mappend y y'
instance Group a ⇒ Group (Complex a) where
	negate (x:+y) = negate x :+ negate y
instance Abelian a ⇒ Abelian (Complex a) where
instance Ring a ⇒ Ring (Complex a) where
	one = one :+ zero
	(*) (x:+y) (x':+y') = (x*x' - y*y') :+ (x*y' + x'*y)
	fromInteger x = fromInteger x :+ zero
instance IntDom a ⇒ IntDom (Complex a)
instance Field a ⇒ Field (Complex a) where
	recip z@(x:+y) = (x/zz) :+ (negate y/zz)
		where zz = normsq' z
instance Module m r ⇒ Module (Complex m) r where
	scale r = map (scale r)

instance Num a ⇒ Num (Complex a) where
	abs = undefined
	signum = undefined

instance (Monoid a, CharZero a) ⇒ CharZero (Complex a) where
	fromRational' = fromReal . fromRational'
-- instance Ring s ⇒ Module (Complex s) s where
-- 	scale r (x :+ y) = (r*x) :+ (r*y)


half' ∷ Field a ⇒ Complex a
half' = fromReal half

instance (Ord a, Field a, Transcendental a) ⇒ Transcendental (Complex a) where
	pi = fromReal pi
	exp (x:+y) = exp x .* fromArg y
	log z = log r :+ t
		where (r,t) = toPolar z
	--(**) -- use default
	sqrt z
		| nearZero z = zero
		| otherwise = z ** half'
	--logBase -- use default
	sin (x:+y) = (sin x * cosh y) :+ (cos x * sinh y)
	cos (x:+y) = (cos x * cosh y) :+ negate (sin x * sinh y)
	--tan  -- use default
	asin z = negate iu * log (iu*z + sqrt (one - z*z))
	acos z = half' * pi - asin z
	atan z = half' * iu * (log (one - iu*z) - log (one + iu*z))
	atan2 x y
		| nearZero x = zero
		| otherwise = atan (y/x)
	sinh (x:+y) = (sinh x * cos y) :+ (cosh x * sin y)
	cosh (x:+y) = (cosh x * cos y) :+ (sinh x * sin y)
	--tanh  -- use default
	--ahyps -- use defaults
