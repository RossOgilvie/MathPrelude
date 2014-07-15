{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Structures.NumEq
	( module MathPrelude.Structures.Logic
	, NumEq(..)
	, smallL, leastSmall
	, (<<~), big
	)
where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Logic
import MathPrelude.Common.PreludeNumConst

-----------------------------------
--- Class
-----------------------------------
class NumEq a where
	(=~) :: a -> a -> Bool
	(/=~) :: a -> a -> Bool

	epsilon :: a
	nearZero :: a -> Bool
	(>>~) :: a -> a -> Bool

	(=~) x y = not $ (/=~) x y
	(/=~) x y = not $ (=~) x y

infixl 4 =~
infixl 4 /=~
infixl 4 >>~

-----------------------------------
--- Methods
-----------------------------------
small :: NumEq a => a -> a -> Bool
small = (>>~)

smallL :: NumEq a => [a] -> a -> Bool
smallL ls x = small (leastSmall ls) x

(<<~) :: NumEq a => a -> a -> Bool
(<<~) = flip (>>~)
infixl 4 <<~

big :: NumEq a => a -> a -> Bool
big = flip (>>~)

leastSmall :: NumEq a => [a] -> a
leastSmall [] = epsilon
leastSmall (x:xs)
	| or (map (big x) xs) = leastSmall xs
	| otherwise = x

-----------------------------------
--- Instances
-----------------------------------
instance NumEq Int where
	(=~) = (==)
	epsilon = zeroInt
	nearZero = (==zeroInt)
	(>>~) _ = nearZero
instance NumEq Int32 where
	(=~) = (==);
	epsilon = zeroInt32;
	nearZero = (==zeroInt32)
	(>>~) _ = nearZero
instance NumEq Int64 where
	(=~) = (==);
	epsilon = zeroInt64;
	nearZero = (==zeroInt64)
	(>>~) _ = nearZero
instance NumEq Integer where
	(=~) = (==);
	epsilon = zeroInteger;
	nearZero = (==zeroInteger)
	(>>~) _ = nearZero


instance NumEq Float where
	(=~) x y = P.abs (x P.- y) <= epsilon P.* P.maximum [oneFloat, P.abs x, P.abs y]
	epsilon = epsFloat
	nearZero a = P.abs a <= epsilon
	(>>~) o a
		| nearZero o = nearZero a
		| otherwise = P.abs a P./ P.abs o <= epsilon
instance NumEq Double where
	(=~) x y = P.abs (x P.- y) <= epsilon P.* P.maximum [oneDouble, P.abs x, P.abs y]
	epsilon = epsDouble
	nearZero x = P.abs x <= epsilon
	(>>~) o a
		| nearZero o = nearZero a
		| otherwise = P.abs a P./ P.abs o <= epsilon


instance NumEq a => NumEq [a] where
	(=~) x y = length x == length y && (and $ zipWith (=~) x y)
	epsilon = [epsilon]
	nearZero xs = and . map nearZero $ xs
	(>>~) os xs = and . map (smallL os) $ xs

instance (NumEq a, NumEq b) => NumEq (a,b) where
	(a,b) =~ (c,d) = a =~ c && b =~ d
	epsilon = (epsilon,epsilon)
	nearZero (a,b) = nearZero a && nearZero b
	(>>~) (o1,o2) (a,b) = (>>~) o1 a && (>>~) o2 b

instance NumEq a => NumEq (Maybe a) where
	(=~) (Just x) (Just y) = x =~ y
	(=~) Nothing Nothing = True
	(=~) _ _ = False
	epsilon = Just epsilon
	nearZero (Just x) = nearZero x
	nearZero Nothing = False
	(>>~) Nothing Nothing = False
	(>>~) _ Nothing = True
	(>>~) (Just o) (Just x) = (>>~) o x
