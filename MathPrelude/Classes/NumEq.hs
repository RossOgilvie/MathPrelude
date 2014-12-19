{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
-- | A comparison operation that makes sense for numeric types, such as applying approximate equality for floats.
module MathPrelude.Classes.NumEq
	( module MathPrelude.Classes.Logic
	, NumEq(..)
	, nearZero
	-- , smallL, leastSmall
	-- , (<<~), big
	)
where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Classes.Logic
import MathPrelude.PreludeNumConst

-----------------------------------
--- Class
-----------------------------------
-- | A class providing comparison operations for numeric types.
class NumEq a where
	-- | Numerically equal. Reduces to (==) for exact types, but useful for say Doubles
	(=~) ∷ a → a → Bool
	-- | Numerically not equal
	(/=~) ∷ a → a → Bool

	(=~) x y = not $ (/=~) x y
	(/=~) x y = not $ (=~) x y

infixl 4 =~
infixl 4 /=~
-- infixl 4 >>~

-----------------------------------
--- Methods
-----------------------------------
-- | Test whether we are close to zero.
nearZero ∷ (NumEq a , Monoid a) ⇒ a → Bool
nearZero a = a =~ mempty

-----------------------------------
--- Instances
-----------------------------------
instance NumEq Int where
	(=~) = (==)
instance NumEq Int32 where
	(=~) = (==)
instance NumEq Int64 where
	(=~) = (==)
instance NumEq Integer where
	(=~) = (==)

instance NumEq Float where
	(=~) x y = P.abs (x P.- y) <= epsFloat P.* P.maximum [oneFloat, P.abs x, P.abs y]
instance NumEq Double where
	(=~) x y = P.abs (x P.- y) <= epsDouble P.* P.maximum [oneDouble, P.abs x, P.abs y]

instance NumEq a ⇒ NumEq [a] where
	(=~) x y = length x == length y && and (zipWith (=~) x y)

instance (NumEq a, NumEq b) ⇒ NumEq (a,b) where
	(a1,b1) =~ (a2,b2) = a1=~a2 && b1=~b2
instance (NumEq a, NumEq b, NumEq c) ⇒ NumEq (a,b,c) where
	(a1,b1,c1) =~ (a2,b2,c2) = a1=~a2 && b1=~b2 && c1=~c2
instance (NumEq a, NumEq b, NumEq c, NumEq d) ⇒ NumEq (a,b,c,d) where
	(a1,b1,c1,d1) =~ (a2,b2,c2,d2) = a1=~a2 && b1=~b2 && c1=~c2 && d1=~d2
instance (NumEq a, NumEq b, NumEq c, NumEq d, NumEq e) ⇒ NumEq (a,b,c,d,e) where
	(a1,b1,c1,d1,e1) =~ (a2,b2,c2,d2,e2) = a1=~a2 && b1=~b2 && c1=~c2 && d1=~d2 && e1=~e2

instance NumEq a ⇒ NumEq (Maybe a) where
	(=~) (Just x) (Just y) = x =~ y
	(=~) Nothing Nothing = True
	(=~) _ _ = False
