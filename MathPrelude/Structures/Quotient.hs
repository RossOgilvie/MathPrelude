{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Structures.Quotient
	( module MathPrelude.Structures.Field
	, Quotient(..)
	, numerator
	, denominator
	, simplifyQ
	)  where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Field
import MathPrelude.Structures.EuclideanDomain

------------------------------
--- Quotient
------------------------------
data Quotient a = a :% a deriving Show
type Ratio = Quotient

numerator :: Quotient a -> a
numerator (a :% b) = a
denominator :: Quotient a -> a
denominator (a :% b) = b

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

instance IntDom a => IntDom (Quotient a)

instance IntDom a => Field (Quotient a) where
	recip (x :% y) = y :% x
	(/) (x:%y) (x':%y') = (x*y') :% (y*x')

instance (IntDom a, Eq a) => Eq (Quotient a) where
	(==) p@(x:%_) q@(y:%_)
		| x == zero && y == zero = True
		| otherwise = (p-q) == zero

instance (IntDom a, NumEq a) => NumEq (Quotient a) where
	(=~) p@(x:%_) q@(y:%_)
		| x =~ zero && y =~ zero = True
		| otherwise = (p-q) =~ zero
	epsilon = epsilon :% one
	nearZero q = q =~ zero

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
