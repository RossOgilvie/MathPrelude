{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}
module Data.MathPrelude.Polynomial where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Ring
import Data.MathPrelude.Abelian
import Data.MathPrelude.Monoid
import Data.MathPrelude.Module
import Data.MathPrelude.Field
import Data.MathPrelude.EuclideanDomain

-----------------------------------
--- Poly
-----------------------------------
data Poly a = Poly Int [a]

-----------------------------------
--- Instances
-----------------------------------

instance Show a => Show (Poly a) where
	show (Poly n l) = "Poly " ++ P.show l'
		where l' = take (n+1) l

instance Eq a => Eq (Poly a) where
	(==) (Poly n xs) (Poly m ys) = xs' == ys'
		where
			n' = max n m + 1
			xs' = take n' xs
			ys' = take n' ys

instance Functor Poly where
	fmap f (Poly n xs) = Poly n (fmap f xs)

instance (Eq a, Monoid a) => Monoid (Poly a) where
	mempty = poly [mempty]
	mappend (Poly n xs) (Poly m ys) = simplifyP $ Poly (max n m) (zipWith mappend xs ys)

instance Abelian a => Abelian (Poly a) where
	negate = fmap negate
	(-) (Poly n xs) (Poly m ys) = simplifyP $ Poly (max n m) (zipWith (-) xs ys)

instance Ring a => Ring (Poly a) where
	one = poly [one]
	fromInteger n = poly [fromInteger n]
	(*) (Poly n xs) (Poly m ys) = simplifyP $ Poly (n+m) [ sum [ (xs!!i)*(ys!!(k-i)) | i <- [0..k]] | k <- [0..]]

instance IntDom a => IntDom (Poly a)

instance Ring a => Module (Poly a) a where
  scale r p = map (r*) p

instance Field a => EuclideanDomain (Poly a) where
	stdUnit p = poly [leadingTerm p]
	stdAssociate p = leadingTerm p ./ p
	--p `div` q = stdAssociate (div' p q)
	p `div` q = div' p q
		where
			div' p q
				| d < 0 = poly [zero]
				| otherwise = monomial d factor + div' r q
				where
					d = degree p - degree q
					factor = leadingTerm p / leadingTerm q
					r = behead (degree p) $ p - (factor .* q)
					behead d p@(Poly n xs) = if n == d then poly $ take n xs else p
	p `mod` q = p - (p `div` q)*q


-----------------------------------
--- Methods
-----------------------------------

poly :: Monoid a => [a] -> Poly a
poly l = Poly (length l - 1) (l ++ repeat mempty)

safeHead :: a -> [a] -> a
safeHead x [] = x
safeHead _ xs = head xs

constTerm :: Monoid a => Poly a -> a
constTerm (Poly n xs) = safeHead mempty xs
leadingTerm :: (Eq a, Monoid a) => Poly a -> a
leadingTerm (Poly n xs) = safeHead mempty . dropWhile (== mempty) . reverse . take (n+1) $ xs

simplifyP :: (Monoid a, Eq a) => Poly a -> Poly a
simplifyP (Poly n xs) = poly . reverse . dropWhile (== mempty) . reverse . take (n+1) $ xs

degree p = n where Poly n _ = simplifyP p

monomial d c = poly $ pad ++ [c]
	where pad = take d $ repeat zero

polyEval :: Ring a => Poly a -> a -> a
polyEval (Poly n xs) x = blah xs'
	where
		xs' = take (n+1) xs
		blah [] = zero
		blah (y:ys) = y + (blah ys)*x
