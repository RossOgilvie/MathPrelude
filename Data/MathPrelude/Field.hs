{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.Field where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Ring
import Data.MathPrelude.Abelian
import Data.MathPrelude.EuclideanDomain
import Data.MathPrelude.OverrideEQ


------------------------------
--- Field
------------------------------

class IntDom a => Field a where
	recip :: a -> a
	(/) :: a -> a -> a
	fromRational :: Rational -> a

	recip x = one / x
	(/) x y = x * recip y
	fromRational (a :% b) = (fromInteger a) / (fromInteger b)

instance Field Float where recip = P.recip; (/) = (P./)
instance Field Double where recip = P.recip; (/) = (P./)

class Field a => Fractional a

class Field a => Floating a where
	pi :: a
	exp :: a -> a
	sqrt :: a -> a
	log :: a -> a
	logBase :: a -> a -> a
	(**) :: a -> a -> a
	sin :: a -> a
	cos :: a -> a
	tan :: a -> a
	asin :: a -> a
	acos :: a -> a
	atan :: a -> a
	atan2 :: a -> a -> a
	sinh :: a -> a
	cosh :: a -> a
	tanh :: a -> a
	asinh :: a -> a
	acosh :: a -> a
	atanh :: a -> a

	logBase b x = log x / log b
	x ** y = exp ( y * log x )
	tan x = sin x / cos x
	tanh x = sinh x / cosh x

instance Floating Float where
	pi = P.pi
	exp = P.exp
	sqrt = P.sqrt
	log = P.log
	(**) = (P.**)
	logBase = P.logBase
	sin = P.sin
	cos = P.cos
	tan = P.tan
	asin = P.asin
	acos = P.acos
	atan = P.atan
	atan2 = P.atan2
	sinh = P.sinh
	cosh = P.cosh
	tanh = P.tanh
	asinh = P.asinh
	acosh = P.acosh
	atanh = P.atanh


instance Floating Double where
	pi = P.pi
	exp = P.exp
	sqrt = P.sqrt
	log = P.log
	(**) = (P.**)
	logBase = P.logBase
	sin = P.sin
	cos = P.cos
	tan = P.tan
	asin = P.asin
	acos = P.acos
	atan = P.atan
	atan2 = P.atan2
	sinh = P.sinh
	cosh = P.cosh
	tanh = P.tanh
	asinh = P.asinh
	acosh = P.acosh
	atanh = P.atanh

------------------------------
--- Quotient
------------------------------

data Quotient a = a :% a deriving Show
type Rational = Quotient Integer

------------------------------
--- Quotient - Instances
------------------------------

instance IntDom a => Monoid (Quotient a) where
	mempty = zero :% one
	mappend (x:%y) (x':%y') = (x*y' + x'*y) :% (y*y')

instance IntDom a => Abelian (Quotient a) where
	--zero = zero :% one
	--(+) (x:%y) (x':%y') = (x*y' + x'*y) :% (y*y')
	negate (x:%y) = negate x :% y
	(-) (x:%y) (x':%y') = (x*y' - x'*y) :% (y*y')

instance IntDom a => Ring (Quotient a) where
	one = one :% one
	(*) (x:%y) (x':%y') = (x*x') :% (y*y')
	fromInteger n = fromInteger n :% one

instance IntDom a => IntDom (Quotient a)

instance IntDom a => Field (Quotient a) where
	recip (x :% y) = y :% x
	(/) (x:%y) (x':%y') = (x*y') :% (y*x')

instance (IntDom a, Eq a) => Eq (Quotient a) where
	(==) p@(x:%_) q@(y:%_)
		| x == zero = y == zero
		| y == zero = x == zero
		| otherwise = (p-q) == zero

instance (IntDom a, NumEq a) => NumEq (Quotient a) where
	(=~) p@(x:%_) q@(y:%_)
		| x =~ zero = y =~ zero
		| y =~ zero = x =~ zero
		| otherwise = (p-q) =~ zero

instance (IntDom a, Ord a) => Ord (Quotient a) where
	compare (x:%y) (x':%y') = parity' num yord y'ord
		where
			num = compare (x*y' - x'*y) zero
			yord = compare y zero
			y'ord = compare y' zero

parity' :: Ordering -> Ordering -> Ordering -> Ordering
parity' EQ _ _ = EQ
parity' x y z
	| y == z = x
	| y /= z = opposite x
		where
			opposite LT = GT
			opposite GT = LT

simplifyQ :: EuclideanDomain a => Quotient a -> Quotient a
simplifyQ (p:%q) = (p `div` g) :% (q `div` g)
	where g = gcd p q


