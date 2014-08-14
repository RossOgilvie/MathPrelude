{-# LANGUAGE RebindableSyntax, UnicodeSyntax, OverloadedStrings #-}
-- | Represent a ring, a structure that is an abelian group with a unital multiplication operator that distributes over the group operation. Our rings are all commutative.
module MathPrelude.Algebraic.Ring
	( module MathPrelude.Algebraic.Group
	, Ring(..)
	, Num(..)
	, IntDom(..)
	, (^)
	, product
	-- , two
	) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.Group
import MathPrelude.Common.PreludeNumConst
-- import MathPrelude.Common.Integral(even)


-----------------------------------
--- Classes
-----------------------------------
-- | A ring is an abelian group with a multiplication operation that distributes over addition. This operation has an identity element. The is always a homomorphism from the ring of integers to any ring R, given by repeated addition (\n → 1 + .... + 1 or (-1) + ... + (-1)), (alternatively, a ring is just a Z-module).
class Abelian a ⇒ Ring a where
	-- | The identity element
	one ∷ a
	-- | The multiplication operation
	(*) ∷ a → a → a
	-- | Push an integer into the ring.
	fromInteger ∷ Integer → a

	fromInteger n
		| n < zeroInteger = negate (fi (negate n))
		| otherwise = fi n
			where
				fi n
					| n =~ zeroInteger = zero
					| n =~ oneInteger = one
					| P.even n    = fin + fin
					| otherwise = fin + fin + one
						where fin = fi (n `P.div` twoInteger)

	{-# MINIMAL one, (*) #-}

infixl 7 *

-- | A compatibility class to replace the Prelude Num class.
class (Eq a, Show a, Ring a) ⇒ Num a  where
    abs, signum ∷ a → a

-- instance Num a ⇒ P.Num a

-- | An integral domain is a ring with the property that non-zero elements multiply to give non-zero elements. Another way to say this is that there are no zero divisors. This is equivalent to the cancellation law holding.
class Ring a ⇒ IntDom a


-----------------------------------
-- Methods
-----------------------------------
-- | Fold a list together multiplicatively
product ∷ Ring a ⇒ [a] → a
product = foldr (*) one

-- | Take the repeated product of an element of the ring. eg a^3 = a*a*a. Throws an error on negative powers.
(^) ∷ Ring a ⇒ a → Int → a
(^) x n
	| n < 0 = error "negative power"
	| n == 0 = one
	| otherwise = product $ zipWith f (intToBinary n) powers
		where
			powers = iterate (\a → a*a) x
			f b x = if b then x else one
			-- f True x = x
			-- f False _ = one

-- | converts an int to a list of binary digits. True= 1, False = 0
intToBinary ∷ Int → [Bool]
intToBinary n = reverse $ intToBinary' n powers
	where
		powers = reverse $ takeWhile (<= n) twopowers

intToBinary' _ [] = []
intToBinary' n (x:xs)
	| n >= x = True : intToBinary' (n-x) xs
	| otherwise = False : intToBinary' n xs

-- | The powers of 2
twopowers = 1 : map (*2) twopowers

-- two ∷ Ring a ⇒ a
-- two = one + one

-----------------------------------
--- Instances
-----------------------------------

instance Ring Integer where one = oneInteger; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int where one = oneInt; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int32 where one = oneInt32; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Int64 where one = oneInt64; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Float where one = oneFloat; (*) = (P.*); fromInteger = P.fromInteger;
instance Ring Double where one = oneDouble; (*) = (P.*); fromInteger = P.fromInteger;

instance IntDom Integer
instance IntDom Int
instance IntDom Int32
instance IntDom Int64
instance IntDom Float
instance IntDom Double

instance Num Integer where abs = P.abs; signum = P.signum
instance Num Int where abs = P.abs; signum = P.signum
instance Num Int32 where abs = P.abs; signum = P.signum
instance Num Int64 where abs = P.abs; signum = P.signum
instance Num Float where abs = P.abs; signum = P.signum
instance Num Double where abs = P.abs; signum = P.signum

instance Ring a ⇒ Ring (Maybe a) where one = Just one; (*) = liftM2 (*); fromInteger x = Just (fromInteger x)
instance IntDom a ⇒ IntDom (Maybe a)

instance Ring b ⇒ Ring (a → b) where
	one = const one
	(*) f g x = f x * g x
instance IntDom b ⇒ IntDom (a → b)
