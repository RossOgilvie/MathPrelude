{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}

module MathPrelude.Constructions.Polynomial
    ( module Export
    , Poly()
    -- , poly
    , toListP, fromListP
    , (*^), scalarP, fromFactorsP
    , constP, leadingP, degreeP
    , termwiseP
    , evalP
    ) where

-----------------------------------
--- Imports
-----------------------------------

import MathPrelude
import qualified Prelude                             as P

-- import           MathPrelude.Classes.EuclideanDomain as Export
-- import           MathPrelude.Classes.Field     as Export
-- import           MathPrelude.Classes.Integral
import           MathPrelude.Classes.VectorSpace
                                               as Export
-- import           MathPrelude.Classes.Rational

-- import           Control.Lens                        hiding (Action)
-- import qualified Data.Foldable                       as F

-----------------------------------
--- Poly
-----------------------------------
-- | A sparse representation of polynomials.
data Poly a = Poly [(Integer,a)] deriving (Eq)


-----------------------------------
--- Methods
-----------------------------------
-- | The isomorphism between a polynomial and its list of coefficients.
-- poly :: Monoid a ⇒ Iso' [a] (Poly a)
-- poly = iso fromListP toListP
-- {-# INLINE poly #-}

-- | Construct a polynomial from its list of coefficients.
fromListP ∷ Monoid a ⇒ [a] → Poly a
fromListP [] = Poly [(0, mempty)]
fromListP ls = Poly . zip [0..] $ ls

-- | Extract the list of coefficients of the polynomial.
toListP ∷ Monoid a ⇒ Poly a → [a]
toListP (Poly xs) = toList' xs 0
toList' ∷ Monoid a ⇒ [(Integer, a)] → Integer → [a]
toList' [] _ = []
toList' l@((n,x):xs) k
    | n == k = x : toList' xs (k+1)
    | otherwise = mempty : toList' l (k+1)

-- | Contruct a monomial with the specified coefficient and degree. 2*^3 + 3*^1 = 2x^3 + 3x
(*^) ∷ a → Integer → Poly a
(*^) c d
    | d >= 0 = Poly [(d,c)]
    | otherwise = Poly [(0,c)]

infixr 8 *^

-- | Embed a scalar into the ring of polynomials.
scalarP ∷ a → Poly a
scalarP c = Poly [(0,c)]

-- | Construct a monic polynomial with the specified roots (counting multiplicities).
fromFactorsP ∷ Ring a ⇒ [a] → Poly a
fromFactorsP ls = product . map (\l → fromListP [-l,1]) $ ls

-- | Extract the constant coeffiecient of a polynomial.
constP ∷ Monoid a ⇒ Poly a → a
constP (Poly []) = mempty
constP (Poly ((n,a):_)) = if n == 0 then a else mempty

-- | Extract the leading coeffiecient of a polynomial.
leadingP ∷ Ring a ⇒ Poly a → a
leadingP (Poly []) = one
leadingP (Poly xs) = snd . head . reverse $ xs

-- | Extract the degree of the polynomial.
degreeP ∷ Poly a → Integer
degreeP (Poly []) = 0
degreeP (Poly xs) = fst . head . reverse $ xs

-- | Apply a function termwise to a polynomial, acting potentially on its degree and coefficient.
termwiseP ∷ Ring a ⇒ (Integer → a → (Integer,a)) → Poly a → Poly a
termwiseP f (Poly xs) = Poly . catchEmpty' . sortSimplifyP' . map (uncurry f) $ xs

-- | Do not allow the empty polynomial, instead use the zero polynomial.
catchEmpty' ∷ Monoid a ⇒ [(Integer,a)] → [(Integer,a)]
catchEmpty' xs
    | length xs == 0 = [(0,mempty)]
    | otherwise = xs

-----------------------------------
--- Routines
-----------------------------------
-- | Evaluate a polynomial at a point.
evalP ∷ Ring a ⇒ Poly a → a → a
evalP (Poly xs) pt = shift 0 xs
    where
        shift _ [] = zero
        shift k (y@(n,a):ys)
            | k == n = a + shift (k+1) ys * pt
            | otherwise = shift (k+1) (y:ys) * pt

-- | Multiply two polys.
mul ∷ Ring a ⇒ Poly a → Poly a → Poly a
mul (Poly xs) (Poly ys) = sortSimplifyP $ Poly [ (n + m, a*b) | (n,a) <- xs, (m,b) <- ys ]

-- old_div p q = Poly $ div' p q
--     where
--         div' p q
--             | d < 0 = [(0,mempty)]
--             | d == 0 = [(0, factor)]
--             | otherwise = div' r q ++ [(d, factor)]
--                 where
--                     dp = degreeP p
--                     d = dp - degreeP q
--                     factor = leadingP p / leadingP q
--                     r = removeTerm dp $ p - shiftPower d (factor .* q)

-- | calculate the divMod of two polys.
newDivMod p q = (Poly (combineP' d), m)
    where
        (d,m) = div' p q
        -- div' ∷ Field a ⇒ Poly a → Poly a → ([(Int,a)],Poly a)
        div' p q
            | deg < 0 = ([(0,mempty)], p)
            | deg == 0 = ([(0, factor)], r)
            | dq == 0 = (p'', 0)
            | otherwise = (p' ++ [(deg, factor)], r')
                where
                    dp = degreeP p
                    dq = degreeP q
                    deg = dp - dq
                    factor = leadingP p / leadingP q
                    r = removeTerm dp $ p - shiftPower deg (factor .* q)
                    (p',r') = div' r q
                    Poly p'' = p /. constP q

-- | A better show functions, using conventions and a dummy variable "x"
displayP p = if s /= "" then "(" ++ s ++ ")" else "0"
    where s = display' p
display' (Poly xs) = intercalate " + " . map displayM $ xs
displayM (n,x)
    | n == 0 = P.show x
    | n == 1 = P.show x ++ "x"
    | otherwise = P.show x ++ "x^" ++ P.show n

-----------------------------------
--- Instances
-----------------------------------

instance Functor Poly where
    fmap f (Poly xs) = Poly (map (map f) xs)

instance (Show a, Ring a) ⇒ Show (Poly a) where
    show = displayP
    -- show = guts

instance Approx a ⇒ Approx (Poly a) where
    (=~) (Poly xs') (Poly ys') = tripEq xs' ys'
        where
            tripEq [] [] = True
            tripEq ((n,a):xs) ((m,b):ys)
                | n == m = a =~ b && tripEq xs ys
                | otherwise = False
            tripEq _ _ = False


instance Monoid a ⇒ Semigroup (Poly a) where
    (<>) p q = merge (<>) p q
instance Monoid a ⇒ Monoid (Poly a) where
    mempty = mempty *^ 0
instance Group a ⇒ Group (Poly a) where
    negate = map negate
    (-) p q = mappend p (negate q)
instance Abelian a ⇒ Abelian (Poly a)
instance Ring a ⇒ Ring (Poly a) where
    one = fromListP [one]
    (*) = mul
    fromInteger n = fromListP [fromInteger n]
instance CRing a ⇒ CRing (Poly a)
instance IntDom a ⇒ IntDom (Poly a)
instance Field a ⇒ EuclideanDomain (Poly a) where
    stdUnit p = leadingP p *^ 0
    stdAssociate p = p /. leadingP p
    -- div = old_div
    -- p `mod` q = p - (p `div` q)*q
    divMod = newDivMod
instance Ring r ⇒ Module (Poly r) r where
    scale r = map (r*)

instance (Monoid a, CharZero a) ⇒ CharZero (Poly a) where
    fromRational' x = fromListP [fromRational' x]

-----------------------------------
--- Internal Stuff
-----------------------------------
-- | just dump the contents of the poly
guts (Poly xs) = P.show xs

-- | collect like terms, sort terms by degree.
sortSimplifyP ∷ Monoid a ⇒ Poly a → Poly a
sortSimplifyP (Poly xs) = Poly $ sortSimplifyP' xs
sortSimplifyP' ∷ Monoid a ⇒ [(Integer,a)] → [(Integer,a)]
sortSimplifyP' = combineP' . sortP'


-- | collect like terms
combineP ∷ Monoid a ⇒ Poly a → Poly a
combineP (Poly xs) = Poly . combineP' $ xs
combineP' [] = []
combineP' [x] = [x]
combineP' (x@(n,a):y@(m,b):xs)
    | n == m = combineP' $ (n,mappend a b) : xs
    | otherwise = x : combineP' (y:xs)


-- | sort terms by degree
sortP ∷ Poly a → Poly a
sortP (Poly xs) = Poly . sortP' $ xs
sortP' = sortBy (compare `on` fst)


-- | Given two sorted polynomials, merge their terms with the given function.
merge ∷ Monoid a ⇒ (a → a→ a) → Poly a → Poly a → Poly a
merge f (Poly xs) (Poly ys) = Poly $ merge' f xs ys
merge' ∷ Monoid a ⇒ (a → a→ a) → [(Integer,a)] → [(Integer,a)] → [(Integer,a)]
merge' _ p [] = p
merge' _ [] q = q
merge' f (x@(n,a):xs) (y@(m,b):ys)
    | n == m = (n, f a b) : merge' f xs ys
    | n < m = x : merge' f xs (y:ys)
    | otherwise = y : merge' f (x:xs) ys


-- | remove a term of the specified degree.
removeTerm ∷ Integer → Poly a → Poly a
removeTerm d (Poly xs) = Poly $ filter (\(n,_) → n /= d) xs


-- | shift the degree of every term in a polynomial. Warning, no checks for negative degrees.
shiftPower ∷ Integer → Poly a → Poly a
shiftPower d (Poly xs) = Poly $ map (\(n,a) → (n + d,a)) xs
