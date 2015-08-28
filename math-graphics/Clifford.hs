{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Clifford where

import MathPrelude
import MathPrelude.Classes.Module
import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector
import MathPrelude.Constructions.Sphere1
import MathPrelude.Constructions.Sphere3
import MathPrelude.Calculus.Derivation
import MathPrelude.Calculus.NewtonsMethod

-- import Differentials

import Graphics.EasyPlot
import Data.Maybe


main =  plot' [Interactive] X11 $
    [
    Data3D [Color Blue, Style Dots] [] (map viewS3 cliffPts)
    -- , Data3D [Color Red, Style Dots] [] (map viewS3 meridians)
    , Data3D [Color Red, Style Points] [] (map viewS3 diagonal)
    -- , Data3D [Color Blue] [] (map viewS3 axisPts)
    -- Data3D [Color White, Style Points] [] (map projR3 . filter outlier . concat $ boundaryLink)
    -- , Data3D [Color Blue, Style Dots] [] (map projR3 overlapPts)
    -- , Data3D [Color White, Style Points] [] (map (viewS3.proj) (concat boundaryLink))
    -- , Data3D [Color Green, Style Dots] [] (map (viewS3.proj) exactDiffSlice)
    -- , Data3D [Color White, Style Points                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ] [] (map viewS3 . filter (exactDiffRatioS3 0.05 2) $ cliffPts)
    , Data3D [Color Black, Style Points] [] (map (viewS3.proj) boundaryLink)
    -- , Data3D [Color White, Style Points] [] (map (viewS3.proj) cut1Pts)
    ]

viewS3 ∷ S3 → (Double,Double,Double)
viewS3 (S3Stereo (V3 t)) = t

data Boundary = DS (Complex Double) S1 | SD S1 (Complex Double) deriving Show

proj ∷ Boundary → S3
proj (DS a (S1inC b)) = S3inC2 (V2 (a /. d, b /. d))
    where d = 1 + normsq' a
proj (SD (S1inC a) b) = S3inC2 (V2 (a /. d, b /. d))
    where d = 1 + normsq' b


projR3 ∷ Boundary → (Double, Double, Double)
projR3 (DS (x:+y) (Angle t)) = (x,y,t)
projR3 (SD (Angle t) (x:+y)) = (x/(x^2+y^2), y/(x^2+y^2), t)

outlier ∷ Boundary → Bool
outlier (DS z _) = normsq' z > 0.05 && normsq' z < 20
outlier (SD _ z) = normsq' z > 0.05 && normsq' z < 20













exactDiffRatio' ∷ Double → Double → Complex Double → Complex Double → Bool
exactDiffRatio' ε p α β = abs x < ε
    where x = normsq' (1+α) * normsq' (1+β) / normsq' (1-α) / normsq' (1-β) - p^2
exactDiffRatioB ∷ Double → Double → Boundary → Bool
exactDiffRatioB ε p (DS α (S1inC β)) = exactDiffRatio' ε p α β
exactDiffRatioB ε p (SD (S1inC α) β) = exactDiffRatio' ε p α β
exactDiffRatioS3 ∷ Double → Double → S3 → Bool
exactDiffRatioS3 ε p (S3inC2 (V2 (α,β))) = exactDiffRatio' ε p α β

exactDiffSlice = filter (exactDiffRatioB 0.05 2) boundaryPts













boundaryLink ∷ [Boundary]
boundaryLink = concatMap (\x → [x,swapBoundary x]) . concatMap (innerGoodPts p) $ qs
-- boundaryLink = [ innerGoodPts p q ++ outerGoodPts p q | q <- qs]
-- boundaryLink = [ innerGoodPts p q | q <- qs]
    where
        p = 2
        -- qs = [-2..2]
        qs = [2..6]

innerGoodPts ∷ Double → Double → [Boundary]
innerGoodPts p q = map (\(a,b) → DS (fromJust a) b) . filter (isJust . fst) $ [(betaSol p q t, Angle t) | t <- pts]
    where
        n = 200 :: Double
        -- we have squared our equations, losing some sign info
        -- sin t and q need to have the same sign
        pts = (if q < 0 then map (*(-1)) else id) . map (*(pi/n)) $ [1..(n-1)]

outerGoodPts ∷ Double → Double → [Boundary]
outerGoodPts p q = map (\(a,b) → SD b (fromJust a)) . filter (isJust . fst) $ [(betaSol p q t, Angle t) | t <- pts]
    where
        n = 200 :: Double
        -- we have squared our equations, losing some sign info
        -- sin t and q need to have the same sign
        pts = (if q < 0 then map (*(-1)) else id) . map (*(pi/n)) $ [1..(n-1)]

-- eqn1 ∷ Double → Double → Diff Double → Diff Double → Diff Double
-- eqn1 p t x y = (x+1)^2 + y^2 - ((p * tan (t/2))^2 .* ((x-1)^2 + y^2))
-- eqn2 ∷ Double → Double → Diff Double → Diff Double → Diff Double
-- eqn2 q t x y = (x - sin t')^2 + (y- cos t')^2 - ((q * sec (t/2))^2 .* ((x-1)^2 + y^2))
--     where
--         sec = 1/cos
--         t' = constant t

---- based on revised equations in write up
eqn1 ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn1 p t αx αy = ((αx-1)^2 + αy^2)*((βx-1)^2 + βy^2) - p^2 .* (((αx+1)^2 + αy^2)*((βx+1)^2 + βy^2))
    where
        βx = cos t'
        βy = sin t'
        t' = constant t

eqn2 ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn2 q t αx αy = ((αx-1)^2 + αy^2)*((βx-1)^2 + βy^2) - (q^2) .* (((αx-βx)^2 + (αy-βy)^2)*sin t')
    where
        βx = cos t'
        βy = sin t'
        t' = constant t

eqnV ∷ Double → Double → Double → Vec 2 (Diff Double) → Vec 2 (Diff Double)
eqnV p q t (V2 (x,y)) = V2 (eqn1 p t x y, eqn2 q t x y)

betaSol p q t = map (\(V2 (x,y)) → x:+y) $ genNewtonsAD (eqnV p q t) (V2 (0,0))





















swapBoundary ∷ Boundary → Boundary
swapBoundary (DS a b) = SD b a
swapBoundary (SD a b) = DS b a

discPts ∷ [Complex Double]
discPts =  map (\(x,y) → x:+y) . filter (\(x,y) → x^2 + y^2 <= r2) $ [(x,y) | x <- pts, y <- pts ]
    where
        n = 100
        r2 = 0.99 -- stay off the edge of the disc
        pts = map (\k → 2*fromInteger k/fromInteger n *(pi/3) - 1) [0..n] -- put a pi/3 here to avoid degenerate situations (pi/3 ~ 1.1)


s1Pts ∷ [S1]
s1Pts = map Angle pts
    where
        k = 50
        pts = map (*(2*pi/k)) [1..k]


boundaryPts ∷ [Boundary]
innerboundaryPts = [DS a b | a <- discPts, b <- s1Pts]
outerboundaryPts = map swapBoundary innerboundaryPts

boundaryPts = innerboundaryPts ++ outerboundaryPts

overlapPts ∷ [Boundary]
overlapPts = [DS (toC a) b | a <- s1Pts, b <- s1Pts] ++ [SD a (toC b) | a <- s1Pts, b <- s1Pts]


-- CUT POINTS
f1Infinite ∷ Double → Boundary → Bool
f1Infinite eps (DS z (S1inC w)) = f1Infinite' eps z w
f1Infinite eps (SD (S1inC z) w) = f1Infinite' eps z w
f1Infinite' eps (x:+y) (z:+w) = abs (((z-1)^2 + w^2)*y - ((x-1)^2 + y^2)*w) < eps

cut1Pts = filter (f1Infinite 0.1) innerboundaryPts




cliffPts ∷ [S3]
cliffPts = [S3inR4 ((sqrt 2) ./ V4 (cos a, sin a, cos b, sin b)) | a <- pts, b <- pts ]
  where
    pts = map (*(2*pi/k)) [1..k]
    k = 50

axisPts ∷ [S3]
axisPts = map (S3inR4 . V4) $ concatMap (\f → map f s1) axes
    where
        k = 10
        angs = map (*(2*pi/k)) [1..k]
        s1 = [(cos t, sin t) | t <- angs]
        t12 (x,y) = (x,y,0,0)
        t13 (x,y) = (x,0,y,0)
        t14 (x,y) = (x,0,0,y)
        t23 (x,y) = (0,x,y,0)
        t24 (x,y) = (0,x,0,y)
        t34 (x,y) = (0,0,x,y)
        axes = [t12,t13,t14,t23,t24,t34]

meridians = [S3inR4 ((sqrt 2) ./ V4 (cos a, sin a, cos b, sin b)) | a <- pts, b <- [0,pi] ] ++ [S3inR4 ((sqrt 2) ./ V4 (cos a, sin a, cos b, sin b)) | b <- pts, a <- [0,pi] ]
  where
    pts = map (*(2*pi/k)) [1..k]
    k = 200

diagonal = [S3inR4 ((sqrt 2) ./ V4 (cos a, sin a, cos a, sin a)) | a <- pts]
  where
    pts = map (*(2*pi/k)) [1..k]
    k = 200
