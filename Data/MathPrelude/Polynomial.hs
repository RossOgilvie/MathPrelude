{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module Data.MathPrelude.Polynomial where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Ring
import Data.MathPrelude.Abelian
import Data.MathPrelude.Monoid
import Data.MathPrelude.Module
import Data.MathPrelude.Field
import Data.MathPrelude.EuclideanDomain
import Data.MathPrelude.OverrideEQ

import Test.QuickCheck

-----------------------------------
--- Poly
-----------------------------------
data Poly a = Poly [(Int,a)]

-----------------------------------
--- Instances
-----------------------------------

instance Functor Poly where
	fmap f (Poly xs) = Poly (map (fmap f) xs)

instance (Show a, Ring a) => Show (Poly a) where
	show (Poly xs) = s
		where
			show_m (n,x)
				| n == 0 = P.show x
				| n == 1 && (x == one) = "x"
				| n == 1 = P.show x ++ "x"
				| x == one = "x^" ++ P.show n
				| otherwise = P.show x ++ "x^" ++ P.show n
			s' = intercalate " + " . map show_m . filter (\(_,x) -> x /= zero) $ xs
			s = if s' /= "" then s' else "0"

instance Eq a => Eq (Poly a) where
	(==) (Poly xs) (Poly ys) = xs == ys

instance (NumEq a, Monoid a) => NumEq (Poly a) where
	(===) (Poly xs') (Poly ys') = tripEq (sortSimplifyP' xs') (sortSimplifyP' ys')
		where
			tripEq [] [] = True
			tripEq ((n,a):xs) ((m,b):ys)
				| n == m = a === b && tripEq xs ys
				| otherwise = False
			tripEq _ _ = False

instance (Monoid a, NumEq a) => Monoid (Poly a) where
	mempty = monomial 0 mempty
	mappend p q = filterP $ merge mappend p q

instance (Abelian a, NumEq a) => Abelian (Poly a) where
	negate = map negate
	(-) p q = filterP $ merge (-) p q

instance (Ring a, NumEq a) => Ring (Poly a) where
	one = poly [one]
	fromInteger n = poly [fromInteger n]
	(*) (Poly xs) (Poly ys) = sortSimplifyP $ Poly [ (n + m, a*b) | (n,a) <- xs, (m,b) <- ys ]

instance (IntDom a, NumEq a) => IntDom (Poly a)

instance (Ring a, NumEq a) => Module (Poly a) a where
  scale r p = map (r*) p

instance (Field a, NumEq a) => EuclideanDomain (Poly a) where
	stdUnit p = monomial 0 (leadingCoeff p)
	stdAssociate p = leadingCoeff p ./ p
	div p q = Poly $ div' p q
		where
			div' p q
				| d < 0 = [(0,mempty)]
				| otherwise = div' r q ++ [(d, factor)]
					where
						dp = degree p
						d = dp - degree q
						factor = leadingCoeff p / leadingCoeff q
						r = removeTerm dp $ p - shiftPower d (factor .* q)
	p `mod` q = p - (p `div` q)*q

p = poly [1,1] :: Poly Double
q = poly [1,2,1]  :: Poly Double
-----------------------------------
--- Methods
-----------------------------------

poly :: [a] -> Poly a
poly ls = Poly $ zip [0..] ls

merge :: (a -> a-> a) -> Poly a -> Poly a -> Poly a
merge' f p [] = p
merge' f [] q = q
merge' f (x@(n,a):xs) (y@(m,b):ys)
	| n == m = (n, f a b) : merge' f xs ys
	| n < m = x : merge' f xs (y:ys)
	| otherwise = y : merge' f (x:xs) ys
merge f (Poly xs) (Poly ys) = Poly $ merge' f xs ys

constCoeff :: Monoid a => Poly a -> a
constCoeff (Poly ((n,a):xs)) = if n == 0 then a else mempty

leadingCoeff :: Poly a -> a
leadingCoeff (Poly xs) = snd . head . reverse $ xs

degree (Poly xs) = fst . head . reverse $ xs

sortSimplifyP :: (Monoid a, NumEq a) => Poly a -> Poly a
sortSimplifyP (Poly xs) = Poly $ sortSimplifyP' xs
sortSimplifyP' :: (Monoid a, NumEq a) => [(Int,a)] -> [(Int,a)]
sortSimplifyP' = filterP' . combineP' . sortP'

simplifyP :: (Monoid a, NumEq a) => Poly a -> Poly a
simplifyP (Poly xs) = Poly $ simplifyP' xs
simplifyP' :: (Monoid a, NumEq a) => [(Int,a)] -> [(Int,a)]
simplifyP' = filterP' . combineP'

combineP (Poly xs) = Poly $ combineP' xs
combineP' [] = []
combineP' [x] = [x]
combineP' (x@(n,a):y@(m,b):xs)
	| n == m = combineP' $ (n, mappend a b):xs
	| otherwise = x : combineP' (y:xs)

filterP :: (Monoid a, NumEq a) => Poly a -> Poly a
filterP (Poly xs) = Poly $ filterP' xs
filterP' :: (Monoid a, NumEq a) => [(Int,a)] -> [(Int,a)]
filterP' = filter (\(n,a) -> n == 0 || a /== mempty)

sortP :: Poly a -> Poly a
sortP (Poly xs) = Poly $ sortP' xs
sortP' xs = sortBy (\x y -> compare (fst x) (fst y)) xs

monomial d c = Poly [(d,c)]

removeTerm d (Poly xs) = Poly $ filter (\(n,_) -> n /= d) xs

shiftPower d (Poly xs) = Poly $ map (\(n,a) -> (n + d,a)) xs

polyEval :: Ring a => Poly a -> a -> a
polyEval (Poly xs) pt = shift 0 xs
	where
		shift _ [] = zero
		shift k (y@(n,a):ys)
			| k == n = a + (shift (k+1) ys)*pt
			| otherwise = (shift (k+1) (y:ys))*pt
