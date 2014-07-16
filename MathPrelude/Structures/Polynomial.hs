{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module MathPrelude.Structures.Polynomial
	( module MathPrelude.Structures.Field
	, module MathPrelude.Structures.EuclideanDomain
	, module MathPrelude.Structures.Module
	, module MathPrelude.Extras.Evaluable
	, Poly, poly
	-- , evalP
	, monomialP, xnP, scalarP, fromFactorsP
	, constP, leadingP, degreeP
	, termwiseP, toListP
	) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Module
import MathPrelude.Structures.Field
import MathPrelude.Structures.EuclideanDomain
import MathPrelude.Structures.Derivation
import MathPrelude.Extras.Evaluable
import MathPrelude.Common.Integral
-- import MathPrelude.Common.CharZero

-----------------------------------
--- Poly
-----------------------------------
data Poly a = Poly [(Int,a)]

-----------------------------------
--- Instances
-----------------------------------

instance Functor Poly where
	fmap f (Poly xs) = Poly (map (map f) xs)

instance (Show a, Ring a) => Show (Poly a) where
	show = refined_show
	-- show = guts

instance Eq a => Eq (Poly a) where
	(==) (Poly xs) (Poly ys) = xs == ys

instance NumEq a => NumEq (Poly a) where
	(=~) (Poly xs') (Poly ys') = tripEq xs' ys'
		where
			tripEq [] [] = True
			tripEq ((n,a):xs) ((m,b):ys)
				| n == m = a =~ b && tripEq xs ys
				| otherwise = False
			tripEq _ _ = False
	epsilon = scalarP epsilon
	nearZero (Poly xs) = and . map (nearZero . snd) $ xs
	(>>~) (Poly os) (Poly xs) = and . map (smallL os') $ xs'
		where
			os' = map snd os
			xs' = map snd xs


instance (Monoid a, NumEq a) => Monoid (Poly a) where
	mempty = monomialP 0 mempty
	mappend p q = filterP $ merge mappend p q

instance (Abelian a, NumEq a) => Abelian (Poly a) where
	negate = map negate
	(-) p q = mappend p (negate q)

instance (Ring a, NumEq a) => Ring (Poly a) where
	one = poly [one]
	(*) = mul

instance (IntDom a, NumEq a) => IntDom (Poly a)

-- instance (Ring a, NumEq a) => Module (Poly a) a where
--   scale r p = map (r*) p
instance Module m r => Module (Poly m) r where
  scale r p = map (scale r) p

instance (Field a, NumEq a) => EuclideanDomain (Poly a) where
	stdUnit p = monomialP 0 (leadingP p)
	stdAssociate p = p /. leadingP p
	-- div = old_div
	-- p `mod` q = p - (p `div` q)*q
	divMod = new_divMod

instance (Ring a, Derivation a) => Derivation (Poly a) where
	derive pp@(Poly p) = map derive pp + (Poly $ polyDiff' p)
		where
			polyDiff' [] = []
			polyDiff' ((n, x):ps)
				| n /= 0 = (n-1, n' * x): polyDiff' ps
				| otherwise = polyDiff' ps
				where n' = fromIntegral n

instance Field a => Integration (Poly a) where
	integrate (Poly p) = Poly $ polyInt' p
		where
			polyInt' [] = []
			polyInt' ((n, x):ps) = (n-1, x / n') : polyInt' ps
				where n' = fromIntegral n

instance Ring a => Evaluable (Poly a) a where
	eval = evalP

-----------------------------------
--- Routines
-----------------------------------
mul (Poly xs) (Poly ys) = sortSimplifyP $ Poly [ (n + m, a*b) | (n,a) <- xs, (m,b) <- ys ]

old_div p q = Poly $ div' p q
	where
		div' p q
			| d < 0 = [(0,mempty)]
			| d == 0 = [(0, factor)]
			| otherwise = div' r q ++ [(d, factor)]
				where
					dp = degreeP p
					d = dp - degreeP q
					factor = leadingP p / leadingP q
					r = removeTerm dp $ p - shiftPower d (factor .* q)

-- new_divMod :: (Field a, Module (Poly a) a) => Poly a -> Poly a -> (Poly a, Poly a)
new_divMod p q = (Poly (combineP' d), m)
	where
		(d,m) = div' p q
		-- div' :: Field a => Poly a -> Poly a -> ([(Int,a)],Poly a)
		div' p q
			| deg < 0 = ([(0,mempty)], p)
			| deg == 0 = ([(0, factor)], r)
			| otherwise = (p' ++ [(deg, factor)], r')
				where
					dp = degreeP p
					deg = dp - degreeP q
					factor = leadingP p / leadingP q
					r = removeTerm dp $ p - shiftPower deg (filterP $ factor .* q)
					(p',r') = div' r q

refined_show p = if s /= "" then "(" ++ s ++ ")" else "0"
	where s = refined_show' p
refined_show' (Poly xs) = intercalate " + " . map show_m $ xs
show_m (n,x)
	| n == 0 = P.show x
	| n == 1 && (x =~ one) = "x"
	| n == 1 = P.show x ++ "x"
	| x =~ one = "x^" ++ P.show n
	| otherwise = P.show x ++ "x^" ++ P.show n

-----------------------------------
--- Methods
-----------------------------------

poly :: Monoid a => [a] -> Poly a
poly [] = Poly [(0, mempty)]
poly ls = Poly . zip [0..] $ ls

monomialP :: Int -> a -> Poly a
monomialP d c
	| d >= 0 = Poly [(d,c)]
	| otherwise = Poly [(0,c)]

xnP :: Ring a => Int -> Poly a
xnP n = monomialP n one

scalarP :: a -> Poly a
scalarP c = Poly [(0,c)]

fromFactorsP :: Ring a => [a] -> Poly a
fromFactorsP ls = product . map (\l -> poly [-l,1]) $ ls


constP :: Monoid a => Poly a -> a
constP (Poly ((n,a):xs)) = if n == 0 then a else mempty

leadingP :: Ring a => Poly a -> a
leadingP (Poly []) = one
leadingP (Poly xs) = snd . head . reverse $ xs

degreeP :: Poly a -> Int
degreeP (Poly []) = 0
degreeP (Poly xs) = fst . head . reverse $ xs

evalP :: Ring a => Poly a -> a -> a
evalP (Poly xs) pt = shift 0 xs
	where
		shift _ [] = zero
		shift k (y@(n,a):ys)
			| k == n = a + (shift (k+1) ys)*pt
			| otherwise = (shift (k+1) (y:ys))*pt

termwiseP :: (Ring a, NumEq a) => (Int -> a -> (Int,a)) -> Poly a -> Poly a
termwiseP f (Poly xs) = Poly . sortSimplifyP' . map (uncurry f) $ xs

toListP :: Monoid a => Poly a -> [a]
toListP (Poly xs) = toList' xs 0
toList' :: Monoid a => [(Int, a)] -> Int -> [a]
toList' [] _ = []
toList' l@((n,x):xs) k
	| n == k = x : toList' xs (k+1)
	| otherwise = mempty : toList' l (k+1)

-----------------------------------
--- Internal Stuff
-----------------------------------

guts (Poly xs) = P.show xs

sortSimplifyP :: (Monoid a, NumEq a) => Poly a -> Poly a
sortSimplifyP (Poly xs) = Poly $ sortSimplifyP' xs
sortSimplifyP' :: (Monoid a, NumEq a) => [(Int,a)] -> [(Int,a)]
sortSimplifyP' = filterP' . combineP' . sortP'

simplifyP :: (Monoid a, NumEq a) => Poly a -> Poly a
simplifyP (Poly xs) = Poly $ simplifyP' xs
simplifyP' :: (Monoid a, NumEq a) => [(Int,a)] -> [(Int,a)]
simplifyP' = filterP' . combineP'

combineP :: (Monoid a, NumEq a) => Poly a -> Poly a
combineP (Poly xs) = Poly . combineP' $ xs
combineP' [] = []
combineP' [x] = [x]
combineP' (x@(n,a):y@(m,b):xs)
	| n == m = combineP' $ (n,mappend a b) : xs
	| otherwise = x : combineP' (y:xs)

filterP :: (NumEq a, Monoid a) => Poly a -> Poly a
filterP (Poly xs) = Poly . filterP' $ xs
filterP' :: (Monoid a, NumEq a) => [(Int,a)] -> [(Int,a)]
filterP' ls = catchEmpty' . filter crit $ ls
	where
		crit (n,a) = n >= 0 && not (nearZero a)

catchEmpty' :: Monoid a => [(Int,a)] -> [(Int,a)]
catchEmpty' xs
	| length xs == 0 = [(0,mempty)]
	| otherwise = xs

sortP :: Poly a -> Poly a
sortP (Poly xs) = Poly . sortP' $ xs
sortP' = sortBy (\x y -> compare (fst x) (fst y))

merge :: Monoid a => (a -> a-> a) -> Poly a -> Poly a -> Poly a
merge' f p [] = p
merge' f [] q = q
merge' f (x@(n,a):xs) (y@(m,b):ys)
	| n == m = (n, f a b) : merge' f xs ys
	| n < m = x : merge' f xs (y:ys)
	| otherwise = y : merge' f (x:xs) ys
merge f (Poly xs) (Poly ys) = Poly $ merge' f xs ys

removeTerm :: Int -> Poly a -> Poly a
removeTerm d (Poly xs) = Poly $ filter (\(n,_) -> n /= d) xs

shiftPower :: Int -> Poly a -> Poly a
shiftPower d (Poly xs) = Poly $ map (\(n,a) -> (n + d,a)) xs
