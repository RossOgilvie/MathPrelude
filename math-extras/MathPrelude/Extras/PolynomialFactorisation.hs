{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}
module MathPrelude.Extras.PolynomialFactorisation
    ( findRoot
    , findRoots
    , findRealRoots
    , isRoot, isRootApprox
    , removeRoot, removeRootApprox
    , rootOrder, rootOrderApprox
    , solveBezout
    ) where

import MathPrelude

import           MathPrelude.Classes.Integral
import           MathPrelude.Classes.Norm
import           MathPrelude.Classes.Transcendental
import           MathPrelude.Constructions.Complex
import           MathPrelude.Constructions.Polynomial
import           MathPrelude.Classes.Action
import           MathPrelude.Calculus.Convergence
import           MathPrelude.Calculus.Derivation
import           MathPrelude.Calculus.NewtonsMethod

-- | All the roots of a poly lie in a circle of this radius
polyRootBound ∷ Poly (Complex Double) → Double
polyRootBound p = min opt1 opt2
    where
        (x:xs) = map norm . reverse $ toListP p
        opt1 = 1 + maximum xs /x
        opt2 = max 1 (sum xs/x)

-- | Use Newton's method to find a root
findRoot ∷ Poly (Complex Double) → Complex Double → Maybe (Complex Double)
findRoot p = newtons 20 (p$$) (p'$$)
    where p' = derive p

-- | A bastard polynomial that is difficult to factorise.
w ∷ Poly (Complex Double)
w = product . map (linearPolyWithRoot . fromReal) $ [1..10]

-- | A type wrapper on 'findRoot'
findRealRoots ∷ Poly Double → [Complex Double]
findRealRoots p = findRoots $ map fromReal p

-- | Find all the roots of a polynomial with multiplicity.
findRoots ∷ Poly (Complex Double) → [Complex Double]
-- findRoots p = concatMap (duplicate . refine) crudeRoots
findRoots p = concatMap duplicate crudeRoots
    where
        repeated = gcd' p (derive p)
        squarefree = if degreeP repeated > 0 then p `div` repeated else p
        crudeRoots = dkMethod squarefree
        -- refine x
        --     | isRoot p x = x
        --     | otherwise = findRoot p x
        duplicate x = replicate ro x where ro = max 1 (rootOrder p x) -- sometimes even the refinement is not enough, in which case root order returns 0, but we 'know' it is a root already

-- | Factor a root from a polynomial and return the factored polynomial and the multiplicity.
removeRoot ∷ (Field a, Eq a) ⇒ Poly a → a → (Poly a, Int)
removeRoot p x
    | not $ isRoot p x = (p,0)
    | otherwise = (p', n+1)
            where (p', n) = removeRoot (p `div` linearPolyWithRoot x) x
-- | Factor a root from a polynomial and return the factored polynomial and the multiplicity.
removeRootApprox ∷ (Field a, Approx a) ⇒ Poly a → a → (Poly a, Int)
removeRootApprox p x
    | not $ isRootApprox p x = (p,0)
    | otherwise = (p', n+1)
            where (p', n) = removeRootApprox (p `div` linearPolyWithRoot x) x

-- | Calculate the order of a root.
rootOrder ∷ (Derivation a, Ring a, Eq a) ⇒ Poly a → a → Integer
rootOrder p x = length . takeWhile (`isRoot` x) . take (degreeP p) . iterate derive $ p
-- | Calculate the order of a root.
rootOrderApprox ∷ (Derivation a, Ring a, Approx a) ⇒ Poly a → a → Integer
rootOrderApprox p x = length . takeWhile (`isRootApprox` x) . take (degreeP p) . iterate derive $ p

-- | Does the polynomial has this point as a root.
isRoot ∷ (Ring a, Eq a) ⇒ Poly a → a → Bool
isRoot p x = (== zero) $ act p x
-- | Does the polynomial has this point as a root.
isRootApprox ∷ (Ring a, Approx a) ⇒ Poly a → a → Bool
isRootApprox p x = nearZero $ act p x

linearPolyWithRoot ∷ Ring a ⇒ a → Poly a
linearPolyWithRoot c = fromListP [negate c, one]

--------------------------
-- Durand–Kerner method
--------------------------
-- | The Durand–Kerner method to factorise a polynomial
dkMethod ∷ Poly (Complex Double) → [Complex Double]
dkMethod p = converge . iterate (dkStep (stdAssociate p) []) $ dkInitPts p

-- | Calculate the initial points to use in the DK method.
-- The initial points for the DK method should be spread throughout the potential disk, and not real
dkInitPts ∷ Poly (Complex Double) → [Complex Double]
dkInitPts p = initPts
    where
        deg = degreeP p
        omega = 2*pi/fromIntegral deg
        args = map (\k → 1 + omega* fromIntegral k) [1..deg] -- roots of unity have 2*pi*k/n args, and arg pi*k/n + 1 is never real
        r = polyRootBound p
        radii = map (\k → fromIntegral k*r/fromIntegral deg) [1..deg]
        initPts = zipWith fromPolar radii args

-- | A single round of the DK method.
-- Set up should be to call dkStep poly [] [est. roots].
-- Step through the  second list, taking from the front , recomputing, and putting onto the front of the first list.
-- Then in the final step, when the second list is exhausted, reverse the first list to preserve the ordering
dkStep ∷ Poly (Complex Double) → [Complex Double] → [Complex Double] → [Complex Double]
dkStep _ before [] = reverse before
dkStep p before (old:after) = dkStep p (new : before) after
    where
        new = old - (p $$ old)/denom
        denom = product $ map (old -) (before++after)

----------------------------------
-- Bezout's method
----------------------------------
-- | Generalised Bezout's method. Given p, q and a right hand side, finds (x,y) s.t. x*p + y*q = rhs, with x and y of minimal degree.
solveBezout ∷ (EuclideanDomain a, Eq a)⇒ a → a → a → (a,a)
solveBezout p q rhs
    | m == mempty = (x', y') -- only a sol if gcd | rhs
    | otherwise = (zero, zero)
        where
            g = gcd' p q -- use the un-normalised gcd
            (f,m) = divMod rhs g -- factor out the gcd from rhs. rhs = f*gcd + m
            (x,y) = extEuclidAlg p q -- solve the related x*p + y*q = g
            (d,x') = divMod (x*f) q -- (xf)p + (yf)q = fg = rhs, but then reduce xf by the 'best possible' multiple of q
            y' = (y*f) + d*p -- reduce yf by the same multiple. The higher degrees will be cancelled by the same factor
