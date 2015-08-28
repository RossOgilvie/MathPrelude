{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE DataKinds #-}
module DD
    ( DD(..)
    , inDisc
    , offDiag
    , projax, projay, projbx, projby

    , Boundary(..)
    , viewBoundary, includeBoundary
    , fixθ, swapBoundary

    , ddPts, boundaryPts, makeDDGrid
    , discPts, s1Pts, cutPts, symPts, conformalPts, singularPts
    ) where

import MathPrelude
import MathPrelude.Constructions.Complex
import MathPrelude.Classes.Module
import MathPrelude.Classes.Norm
import MathPrelude.Constructions.Sphere1

import Control.DeepSeq

-----------------------------------------
-- Parameter Space
-----------------------------------------
data DD = DD !(Complex Double) !(Complex Double) deriving (Show,Read,Eq)


instance NumEq DD where
    (=~) (DD a0 a1) (DD b0 b1) = a0 =~ b0 && a1 =~ b1
    epsilon = DD epsilon epsilon

instance Monoid DD where
    mempty = DD 0 0
    mappend (DD z1 w1) (DD z2 w2) = DD (z1+z2) (w1+w2)

instance Group DD where
    negate (DD z1 w1) = DD (-z1) (-w1)

instance Abelian DD

instance Module DD Double where
    -- scale ∷ Double → DD → DD
    scale s (DD α β) = DD (s.*α) (s .*β)

instance Norm DD Double where
    norm (DD α β) = sqrt $ normsq' α + normsq' β

instance VectorSpace DD Double

instance NFData DD where
  rnf (DD (x:+y) (z:+w)) = rnf x `seq` rnf y `seq` rnf z `seq` rnf w

inDisc (DD a b) = normsq' a < 0.99 && normsq' b < 0.99

offDiag (DD a b) = a /=~ b

projax (DD (ax:+ay) (bx :+ by)) = (ay,bx,by)
projay (DD (ax:+ay) (bx :+ by)) = (ax,bx,by)

projbx (DD (ax:+ay) (bx :+ by)) = (ax,ay,by)
projby (DD (ax:+ay) (bx :+ by)) = (ax,ay,bx)






-----------------------------------------
-- Boundary
-----------------------------------------
data Boundary = DS (Complex Double) S1 | SD S1 (Complex Double) deriving Show

viewBoundary ∷ Boundary → (Double,Double,Double)
viewBoundary (DS α (Angle φ)) = (fixθ θ, fixθ φ, 1 - fixR r)
    where (r,θ) = toPolar α
viewBoundary (SD (Angle θ) β) = (fixθ θ, fixθ φ, fixR r - 1)
    where (r,φ) = toPolar β

includeBoundary ∷ Boundary → DD
includeBoundary (SD α β) = DD (toC α) β
includeBoundary (DS α β) = DD α (toC β)

fixθ θ
    | θ >= (2*pi) = fixθ (θ - 2*pi)
    | θ < 0 = fixθ (θ + 2*pi)
    | otherwise = θ
-- fixR r = r+1
fixR r
    | r > 1 = 1/r
    | otherwise = r

swapBoundary ∷ Boundary → Boundary
swapBoundary (DS a b) = SD b a
swapBoundary (SD a b) = DS b a
















-----------------------------------------
-- Special Points
-----------------------------------------


discPts ∷ Integer → [Complex Double]
discPts n =  map (uncurry (:+)) . filter (\(x,y) → x^2 + y^2 <= r2) $ [(x,y) | x <- pts, y <- pts ]
    where
        -- n = 100
        -- n = fromInteger n'
        r2 = 0.99 -- stay off the edge of the disc
        pts = map (\k → 2*fromInteger k/fromInteger n *(pi/3) - 1) [0..n] -- put a pi/3 here to avoid degenerate situations (pi/3 ~ 1.1)

s1Pts ∷ Integer → [S1]
s1Pts k' = map Angle pts
    where
        -- k = 50
        k = fromInteger k'
        pts = map (*(2*pi/k)) [1..k]



ddPts ∷ Integer → [DD]
ddPts n = makeDDGrid n 1 (DD (0.0001:+0.00001) ((-0.001):+(-0.00003)))

makeDDGrid ∷ Integer → Double → DD → [DD]
makeDDGrid num halfWidth center = filter offDiag . filter inDisc . map (+center) $ δdd
        where
            notOne = pi/3.15 -- avoid integral points, as they tend to be bad
            f k = 2*notOne*fromInteger k / fromInteger num - notOne -- linear interpolation of -1 to 1
            δs = map (\k → f k * halfWidth) [0..num] -- offsets
            δdd = [DD (a:+b) (c:+d) | a<-δs, b<-δs, c<-δs, d<-δs]

boundaryPts ∷ Integer → [Boundary]
boundaryPts k = [DS (toC a) b | a <- s1Pts k, b <- s1Pts k]




stdDiscPts = discPts 100

f1Infinite eps (DD (x:+y) (z:+w)) = abs (((z-1)^2 + w^2)*y - ((x-1)^2 + y^2)*w) < eps
cutPts ∷ [DD]
cutPts = filter (f1Infinite 0.001) $ ddPts 30

symPts ∷ [Boundary]
symPts' = map (SD (Angle 0)) stdDiscPts
symPts = symPts' ++ map swapBoundary symPts'

conformalPts ∷ [DD]
conformalPts = map (flip DD 0) stdDiscPts ++ map (DD 0) stdDiscPts

singularPts ∷ [DD]
singularPts = zipWith DD stdDiscPts stdDiscPts
