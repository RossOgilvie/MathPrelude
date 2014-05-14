{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
module MathPrelude
	( module MathPrelude
	, module BasicPrelude
	)where

import BasicPrelude
import qualified Prelude as P

instance Monoid Int where mempty = 0; mappend = (P.+)
instance Monoid Integer where mempty = 0; mappend = (P.+)
instance Monoid Int32 where mempty = 0; mappend = (P.+)
instance Monoid Int64 where mempty = 0; mappend = (P.+)
instance Monoid Float where mempty = 0; mappend = (P.+)
instance Monoid Double where mempty = 0; mappend = (P.+)

sum :: Monoid a => [a] -> a
sum = foldr mappend mempty

class (Eq a, Monoid a) => Abelian a where
	zero :: a
	(+) :: a -> a -> a
	negate :: a -> a
	(-) :: a -> a -> a

	zero = mempty
	(+) = (<>)
	negate x = zero - x
	(-) x y = x + negate y
infixl 6  +, -

instance Abelian Int where negate = P.negate; (-) = (P.-)
--instance Abelian Int where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Integer where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Int32 where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Int64 where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Float where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Double where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)

class Abelian a => Ring a where
	one :: a
	(*) :: a -> a -> a
	fromInteger :: Integer -> a

	one = fromInteger 1
	fromInteger n
		| n < 0       =  negate (fi (negate n))
		| otherwise   =  fi n
			where
				fi 0    =  zero
				fi 1    =  one
				fi n
					| even n    = fin + fin
					| otherwise = fin + fin + one
						where fin = fi (n `P.div` 2)
infixl 7  *

product :: Ring a => [a] -> a
product = foldr (*) one

(^) :: Ring a => a -> Int -> a
(^) x n = product $ take n $ repeat x

instance Ring Int where one = 1; (*) = (P.*)
instance Ring Integer where one = 1; (*) = (P.*)
instance Ring Int32 where one = 1; (*) = (P.*)
instance Ring Int64 where one = 1; (*) = (P.*)
instance Ring Float where one = 1; (*) = (P.*)
instance Ring Double where one = 1; (*) = (P.*)

class (Eq a, Show a, Ring a) => Num a  where
    abs, signum :: a -> a

instance Num Int where abs = P.abs; signum = P.signum
instance Num Integer where abs = P.abs; signum = P.signum
instance Num Int32 where abs = P.abs; signum = P.signum
instance Num Int64 where abs = P.abs; signum = P.signum
instance Num Float where abs = P.abs; signum = P.signum
instance Num Double where abs = P.abs; signum = P.signum

class Ring a => IntDom a where;

instance IntDom Int
instance IntDom Integer
instance IntDom Int32
instance IntDom Int64
instance IntDom Float
instance IntDom Double


class IntDom a => Field a where
	recip :: a -> a
	(/) :: a -> a -> a
	fromRational :: Quotient Integer -> a

	recip x = one / x
	(/) x y = x * recip y
	fromRational (a :% b) = (fromInteger a) / (fromInteger b)

instance Field Float where recip = P.recip; (/) = (P./)
instance Field Double where recip = P.recip; (/) = (P./)

class Field a => Fractional a where;

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


	epsilon :: a
	nearZero :: a -> Bool

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

	epsilon = 1e-6
	nearZero a = P.abs a <= epsilon

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

	epsilon = 1e-14
	nearZero a = P.abs a <= epsilon




--------------- Quotient stuff

data Quotient a = a :% a deriving Show
--type Rational = Quotient Integer

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


------------ fun stuff
newtype Z2 = Z2 Integer deriving Show

instance Eq Z2 where
	(==) (Z2 x) (Z2 y) = (x-y) `mod` 2 == 0

instance Monoid Z2 where
	mempty = Z2 0
	mappend (Z2 0) (Z2 0) = Z2 0
	mappend (Z2 1) (Z2 0) = Z2 1
	mappend (Z2 0) (Z2 1) = Z2 1
	mappend (Z2 1) (Z2 1) = Z2 0

instance Abelian Z2 where
	negate = id

instance Ring Z2 where
	one = Z2 1
	(*) (Z2 0) (Z2 0) = Z2 0
	(*) (Z2 1) (Z2 0) = Z2 0
	(*) (Z2 0) (Z2 1) = Z2 0
	(*) (Z2 1) (Z2 1) = Z2 1

----------- polynomial
data Poly a = Poly Int [a]

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

polyEval :: Ring a => Poly a -> a -> a
polyEval (Poly n xs) x = blah xs'
	where
		xs' = take (n+1) xs
		blah [] = zero
		blah (y:ys) = y + (blah ys)*x

--p,q :: Poly Double
--p = poly [1,2,3]
--q = poly [1,2,3,4]
--pq = p :% q


------ Euclidean Domains
class IntDom a => EuclideanDom a where
	stdAssociate    :: a -> a
	stdUnit         :: a -> a
	normalize       :: a -> (a, a)

	div, mod        :: a -> a -> a
	divMod          :: a -> a -> (a,a)

	-- Minimal complete definition:
	-- (stdUnit or normalize) and (divMod or (div and mod))
	stdAssociate x  =  x `div` stdUnit x
	stdUnit x       =  snd (normalize x)
	normalize x     =  (stdAssociate x, stdUnit x)

	n `divMod` d    =  (n `div` d, n `mod` d)
	n `div` d       =  q  where (q,r) = divMod n d
	n `mod` d       =  r  where (q,r) = divMod n d

instance EuclideanDom Integer where
	stdAssociate 	= P.abs
	stdUnit         = P.signum
	divMod          = P.divMod

gcd :: EuclideanDom a => a -> a -> a
gcd a b
	| b == zero = a
	| otherwise = gcd b (a `mod` b)

simplifyQ :: EuclideanDom a => Quotient a -> Quotient a
simplifyQ (p:%q) = (p `div` g) :% (q `div` g)
	where g = gcd p q

instance Field a => EuclideanDom (Poly a) where
	stdUnit p = poly [leadingTerm p]
	stdAssociate p = map (/ leadingTerm p) p
	--p `div` q = stdAssociate (div' p q)
	p `div` q = div' p q
		where
			div' p q
				| d < 0 = poly [zero]
				| otherwise = factor + div' r q
				where
					d = degree p - degree q
					factor = monomial d (leadingTerm p / leadingTerm q)
					r = behead (degree p) $ p - factor * q
					behead d p@(Poly n xs) = if n == d then poly $ take n xs else p
	p `mod` q = p - (p `div` q)*q


-----------------------------------
class (Abelian m, Ring s) => Module m s where
	scale :: s -> m -> m
	(.*) :: s -> m -> m
	(*.) :: m -> s -> m

	(.*) = scale
	(*.) = flip (.*)
	scale = flip (.*)


-----------------------------------

data Complex a = a :+ a deriving (Show, Eq)
instance Monoid a => Monoid (Complex a) where
	mempty = mempty :+ mempty
	mappend (x:+y) (x':+y') = mappend x x' :+ mappend y y'

instance Abelian a => Abelian (Complex a) where
	negate (x:+y) = negate x :+ negate y

instance Ring a => Ring (Complex a) where
	one = one :+ zero
	(*) (x:+y) (x':+y') = (x*x' - y*y') :+ (x*y' + x'*y)

instance IntDom a => IntDom (Complex a)

instance Field a => Field (Complex a) where
	recip z@(x:+y) = (x/zz) :+ (negate y/zz)
		where zz = realPart $ normsq z

instance Num a => Num (Complex a) where
	abs = undefined
	signum = undefined

instance Ring s => Module (Complex s) where
	scale r (x :+ y) = (r*x) :+ (r*y)

iu :: Ring a => Complex a
iu = zero :+ one
realPart :: Complex a -> a
realPart (x:+_) = x
imagPart :: Complex a -> a
imagPart (x:+_) = x

fromReal :: Monoid a => a -> Complex a
fromReal r = r :+ mempty
fromArg :: Floating a => a -> Complex a
fromArg x = (cos x) :+ (sin x)
fromPolar :: (Ring a, Floating a) => a -> a -> Complex a
fromPolar r t = r .* (fromArg t)
toPolar ::  (Ord a, Field a, Floating a) => Complex a -> (a,a)
toPolar z = (sqrt $ normsq' z, arg z)

conjugate :: Abelian a => Complex a -> Complex a
conjugate (x:+y) = (x:+ (negate y))
normsq :: Ring a => Complex a -> Complex a
normsq z = z * conjugate z
normsq' :: Ring a => Complex a -> a
normsq' = realPart . normsq

arg :: (Ord a, Field a, Floating a) => Complex a -> a
arg (x :+ y)
	| x > zero = atan (y/x)
	| nearZero x && y > zero = pi/ fromInteger 2
	| nearZero x && y < zero = negate $ pi/ fromInteger 2
	| nearZero x && nearZero y = zero
	| x < zero && y > zero = atan (y/x) + pi
	| x < zero && y < zero = atan (y/x) - pi
	| x < zero && nearZero y = pi
	| otherwise = zero

instance (Ord a, Field a, Floating a) => Floating (Complex a) where
	pi = fromReal pi
	exp (x:+y) = (exp x) .* (fromArg y)
	log z = (log r) :+ t
		where (r,t) = toPolar z
	--(**) -- use default
	sqrt z
		| nearZero z = zero
		| otherwise = z ** half
			where half = fromReal $ recip $ fromInteger 2
	--logBase -- use default
	sin (x:+y) = ((sin x)*(cosh y)) :+ ((cos x)*(sinh y))
	cos (x:+y) = ((cos x)*(cosh y)) :+ (negate $ (sin x)*(sinh y))
	--tan  -- use default
	--asin = P.asin
	--acos = P.acos
	--atan = P.atan
	--atan2 = P.atan2
	sinh (x:+y) = ((sinh x)*(cos y)) :+ ((cosh x)*(sin y))
	cosh (x:+y) = ((cosh x)*(cos y)) :+ ((sinh x)*(sin y))
	--tanh  -- use default
	--asinh = P.asinh
	--acosh = P.acosh
	--atanh = P.atanh

	epsilon = fromReal epsilon
	nearZero = nearZero . sqrt . normsq'
