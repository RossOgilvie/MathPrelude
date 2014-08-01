{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Classes.NumEq
	( module MathPrelude.Algebraic.Logic
	, NumEq(..)
	, nearZero
	-- , smallL, leastSmall
	-- , (<<~), big
	)
where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.Logic
import MathPrelude.Common.PreludeNumConst

-----------------------------------
--- Class
-----------------------------------
-- | A class providing comparison operations for numeric types.
class NumEq a where
	-- | Numerically equal. Reduces to (==) for exact types, but useful for say Doubles
	(=~) ∷ a → a → Bool
	-- | Numericall not equal
	(/=~) ∷ a → a → Bool

	-- | The tolerance permitted by '=~'
	-- epsilon ∷ a
	-- -- | Test where a value s approximately zero (to within tolerance)
	-- nearZero ∷ a → Bool
	-- -- | Test whether something is small, compared to another value. Think >> in mathematics. For exact types, the reference magnitude is ignored. 1e20 >>~ 1 = True.
	-- (>>~) ∷ a → a → Bool

	(=~) x y = not $ (/=~) x y
	(/=~) x y = not $ (=~) x y

infixl 4 =~
infixl 4 /=~
-- infixl 4 >>~

-----------------------------------
--- Methods
-----------------------------------
nearZero ∷ (NumEq a , Monoid a) ⇒ a → Bool
nearZero a = a =~ mempty

-- | A function name for '>>~'
-- small ∷ NumEq a ⇒ a → a → Bool
-- small = (>>~)
--
-- -- | Test whether a value is small compared to the values in a list.
-- smallL ∷ NumEq a ⇒ [a] → a → Bool
-- smallL ls x = small (leastSmall ls) x
--
-- -- | A flip of '<<~'
-- (<<~) ∷ NumEq a ⇒ a → a → Bool
-- (<<~) = flip (>>~)
-- infixl 4 <<~
--
-- -- | A flip of 'small'
-- big ∷ NumEq a ⇒ a → a → Bool
-- big = flip (>>~)
--
-- -- | Find the smallest value in a list, in the sense that it is big compared to no other elements.
-- leastSmall ∷ NumEq a ⇒ [a] → a
-- leastSmall [] = epsilon
-- leastSmall (x:xs)
-- 	| or (map (big x) xs) = leastSmall xs
-- 	| otherwise = x

-----------------------------------
--- Instances
-----------------------------------
instance NumEq Int where
	(=~) = (==)
	-- epsilon = zeroInt
	-- nearZero = (==zeroInt)
	-- (>>~) _ = nearZero
instance NumEq Int32 where
	(=~) = (==);
	-- epsilon = zeroInt32;
	-- nearZero = (==zeroInt32)
	-- (>>~) _ = nearZero
instance NumEq Int64 where
	(=~) = (==);
	-- epsilon = zeroInt64;
	-- nearZero = (==zeroInt64)
	-- (>>~) _ = nearZero
instance NumEq Integer where
	(=~) = (==);
	-- epsilon = zeroInteger;
	-- nearZero = (==zeroInteger)
	-- (>>~) _ = nearZero


instance NumEq Float where
	(=~) x y = P.abs (x P.- y) <= epsFloat P.* P.maximum [oneFloat, P.abs x, P.abs y]
	-- (=~) x y = P.abs (x P.- y) <= epsilon P.* P.maximum [oneFloat, P.abs x, P.abs y]
	-- epsilon = epsFloat
	-- nearZero a = P.abs a <= epsilon
	-- (>>~) o a
	-- 	| nearZero o = nearZero a
	-- 	| otherwise = P.abs a P./ P.abs o <= epsilon
instance NumEq Double where
	(=~) x y = P.abs (x P.- y) <= epsDouble P.* P.maximum [oneDouble, P.abs x, P.abs y]
	-- (=~) x y = P.abs (x P.- y) <= epsilon P.* P.maximum [oneDouble, P.abs x, P.abs y]
	-- epsilon = epsDouble
	-- nearZero x = P.abs x <= epsilon
	-- (>>~) o a
	-- 	| nearZero o = nearZero a
	-- 	| otherwise = P.abs a P./ P.abs o <= epsilon


instance NumEq a ⇒ NumEq [a] where
	(=~) x y = length x == length y && (and $ zipWith (=~) x y)
	-- epsilon = [epsilon]
	-- nearZero xs = and . map nearZero $ xs
	-- (>>~) os xs = and . map (smallL os) $ xs

instance (NumEq a, NumEq b) ⇒ NumEq (a,b) where
	(a,b) =~ (c,d) = a =~ c && b =~ d
	-- epsilon = (epsilon,epsilon)
	-- nearZero (a,b) = nearZero a && nearZero b
	-- (>>~) (o1,o2) (a,b) = (>>~) o1 a && (>>~) o2 b

instance NumEq a ⇒ NumEq (Maybe a) where
	(=~) (Just x) (Just y) = x =~ y
	(=~) Nothing Nothing = True
	(=~) _ _ = False
	-- epsilon = Just epsilon
	-- nearZero (Just x) = nearZero x
	-- nearZero Nothing = False
	-- (>>~) Nothing Nothing = False
	-- (>>~) _ Nothing = True
	-- (>>~) (Just o) (Just x) = (>>~) o x
