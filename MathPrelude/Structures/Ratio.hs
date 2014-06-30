{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Structures.Ratio
	( module MathPrelude.Structures.Field
	, Ratio(..)
	, numerator
	, denominator
	, simplifyQ
	)  where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Field
import MathPrelude.Structures.EuclideanDomain
import MathPrelude.Structures.Derivation

------------------------------
--- Ratio
------------------------------
data Ratio a = a :% a deriving Show

numerator :: Ratio a -> a
numerator (a :% b) = a
denominator :: Ratio a -> a
denominator (a :% b) = b

------------------------------
---- Instances
------------------------------

instance IntDom a => Monoid (Ratio a) where
	mempty = zero :% one
	mappend (x:%y) (x':%y') = (x*y' + x'*y) :% (y*y')

instance IntDom a => Abelian (Ratio a) where
	--zero = zero :% one
	--(+) (x:%y) (x':%y') = (x*y' + x'*y) :% (y*y')
	negate (x:%y) = negate x :% y
	(-) (x:%y) (x':%y') = (x*y' - x'*y) :% (y*y')

instance IntDom a => Ring (Ratio a) where
	one = one :% one
	(*) (x:%y) (x':%y') = (x*x') :% (y*y')

instance IntDom a => IntDom (Ratio a)

instance IntDom a => Field (Ratio a) where
	recip (x :% y) = y :% x
	(/) (x:%y) (x':%y') = (x*y') :% (y*x')

instance (IntDom a, Eq a) => Eq (Ratio a) where
	(==) p@(x:%_) q@(y:%_)
		| x == zero && y == zero = True
		| otherwise = (p-q) == zero

instance (IntDom a, NumEq a) => NumEq (Ratio a) where
	(=~) p@(x:%_) q@(y:%_)
		| x =~ zero && y =~ zero = True
		| otherwise = (p-q) =~ zero
	epsilon = epsilon :% one
	nearZero q = q =~ zero

instance (IntDom a, Ord a) => Ord (Ratio a) where
	compare (x:%y) (x':%y') = parity' num yord y'ord
		where
			num = compare (x*y' - x'*y) zero
			yord = compare y zero
			y'ord = compare y' zero

instance (Derivation a, Ring a) => Derivation (Ratio a) where
	derive (x:%y) = (derive x * y - x * derive y) :% (y^2)

parity' :: Ordering -> Ordering -> Ordering -> Ordering
parity' EQ _ _ = EQ
parity' x y z
	| y == z = x
	| y /= z = opposite x
		where
			opposite LT = GT
			opposite GT = LT

simplifyQ :: EuclideanDomain a => Ratio a -> Ratio a
simplifyQ (p:%q) = (p `div` g) :% (q `div` g)
	where g = gcd p q
