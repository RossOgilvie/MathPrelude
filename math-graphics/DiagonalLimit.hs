{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module DiagonalLimit where

import MathPrelude
import MathPrelude.Classes.Norm
import MathPrelude.Classes.Module
import MathPrelude.Constructions.Complex
import MathPrelude.Calculus.Convergence
import MathPrelude.SpecialFunctions.EllipticFunctions
import ThreeDViewer
import DD
import Differentials

import Control.Parallel.Strategies
--
-- import Graphics.EasyPlot
-- import Data.Maybe


arcPts ∷ [Complex Double]
arcPts =  filter (\(x:+y) → x^2 + y^2 <= r2) . map shiftAndScale $ pts
    where
        r2 = 0.99
        n = 2000
        pts :: [Double]
        pts = map (\k → 2*pi*fromInteger k/fromInteger n) [0..(n-1)]
        shiftAndScale :: Double -> Complex Double
        shiftAndScale = (+(3:+0)) .fromPolar (sqrt 8)

phi a = imagPart $ log ((1+conjugate a)/(1+a)) - 2 *log ((1-conjugate a)/(1-a))

moduliBoundary = zip arcPts $ map phi arcPts

boundaryProj (x:+y, f) = (x,y,f)
boundaryPlot = graph boundaryProj moduliBoundary

embedBlowup ∷ Complex Double → Double → [DD]
embedBlowup α f = [DD (α + fromPolar ε f) (α - fromPolar ε f), DD (α - fromPolar ε f) (α + fromPolar ε f)]
    where ε = 0.05

embeddedPts = concatMap (uncurry embedBlowup) moduliBoundary
embeddedDiag = [DD α α | α <- arcPts]


makePerpDDGrid ∷ Integer → Double → [DD] → [DD]
makePerpDDGrid num halfWidth dds = concatMap (makePerpDDGrid' num halfWidth) $ zip dds (drop 1 dds)
makePerpDDGrid' ∷ Integer → Double → (DD,DD) → [DD]
makePerpDDGrid' num halfWidth (center,next) = filter (not . onCut1) . filter offDiag . filter inDisc . map (+center) $ δdd
        where
            -- an orthonormal basis perpendicular to the approximate tangent to the curve
            (DD α β) = next - center
            perp1' = DD (iu*α) β
            perp1 = normalise perp1'
            perp2' = DD (iu*α) (iu*β)
            perp2 = normalise perp2'
            perp3' = DD α (iu*β)
            perp3 = normalise perp3'
            notOne = pi/3.15 -- avoid integral points, as they tend to be bad
            f k = 2*notOne*fromInteger k / fromInteger num - notOne -- linear interpolation of -1 to 1
            δs = map (\k → f k * halfWidth) [0..num] -- offsets
            δdd = [a.*perp1 + b.*perp2 + c.*perp3 | a<-δs, b<-δs, c<-δs]

nearArcPts = makePerpDDGrid 5 0.05 embeddedDiag

inRange l u x = l < x && x < u

manualSymCond' ∷ DD → Double
manualSymCond' (DD α β) = (norm (1+α) * norm (1+β)) / (norm (1-α) * norm (1-β))
manualSymCond ∷ Double → Double → DD → Bool
manualSymCond val eps = inRange (val-eps) (val+eps) . manualSymCond'


manualSymCondP' ∷ DD → Double
manualSymCondP' pt = -gamma2p*gammaSm + gamma2m
    where
        gammaSm = gamma_minus 1 0 pt
        gamma2p = gamma_plus 0 1 pt
        gamma2m = gamma_minus 0 1 pt
manualSymCondP ∷ Double → Double → DD → Bool
manualSymCondP val eps = inRange (val-eps) (val+eps) . manualSymCondP'

nearArcPts' = filter (manualSymCond 2 0.5) nearArcPts
nearArcPts'' ∷ [(DD, Integer)]
nearArcPts'' = zip nearArcPts' $ map (round . manualSymCondP') nearArcPts'
nearArcPts''' = map (map fst) . groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ nearArcPts''
-- nearArcPts'' = filter (manualSymCondP 4 1) nearArcPts' `using` parListChunk 100 rdeepseq



graphEmbedded = graphs projbx ([embeddedPts, embeddedDiag]++ nearArcPts''')










discPts ∷ Integer → [Complex Double]
discPts n =  map (\(x,y) → x:+y) . filter (\(x,y) → x^2 + y^2 <= r2) $ [(x,y) | x <- pts, y <- pts ]
    where
        -- n = 20
        r2 = 0.99 -- stay off the edge of the disc
        pts = map (\k → 2*fromInteger k/fromInteger n *(pi/3) - 1) [0..n] -- put a pi/3 here to avoid degenerate situations (pi/3 ~ 1.1)

phi' a = imagPart $ (pi*iu*q*r1 - r1*log ((1+conjugate a)/(1+a)) + r2*log ((1-conjugate a)/(1-a)) ) / (r1-r2)
    where
        q = 1
        r1 = normsq (1-a)
        r2 = normsq (1+a)
moduliBoundary' = zip pts $ map phi' pts
    where pts = discPts 20
boundaryPlot' = graph boundaryProj moduliBoundary'



-----------------------------
-- For B_0 line over Δ
-----------------------------

complexPlot (x:+y) = (x,y,0)

mIntDeltaCond :: Double → Double → Complex Double → Bool
mIntDeltaCond eps q alpha = abs (normsq' (1+alpha) / normsq' (1-alpha) - q) < eps

-- intersectionHeights alpha n =
--     where
--         denom = normsq (1-alpha) - normsq (1+alpha)








--
-- cliffPts ∷ [S3]
-- cliffPts = [S3inR4 ((sqrt 2) ./ V4 (cos a, sin a, cos b, sin b)) | a <- pts, b <- pts ]
--   where
--     pts = map (*(2*pi/k)) [1..k]
--     k = 200
--
-- axisPts ∷ [S3]
-- axisPts = map (S3inR4 . V4) $ concatMap (\f → map f s1) axes
--     where
--         k = 10
--         angs = map (*(2*pi/k)) [1..k]
--         s1 = [(cos t, sin t) | t <- angs]
--         t12 (x,y) = (x,y,0,0)
--         t13 (x,y) = (x,0,y,0)
--         t14 (x,y) = (x,0,0,y)
--         t23 (x,y) = (0,x,y,0)
--         t24 (x,y) = (0,x,0,y)
--         t34 (x,y) = (0,0,x,y)
--         axes = [t12,t13,t14,t23,t24,t34]
--
-- meridians = [S3inR4 ((sqrt 2) ./ V4 (cos a, sin a, cos b, sin b)) | a <- pts, b <- [0,pi] ] ++ [S3inR4 ((sqrt 2) ./ V4 (cos a, sin a, cos b, sin b)) | b <- pts, a <- [0,pi] ]
--   where
--     pts = map (*(2*pi/k)) [1..k]
--     k = 200
--
-- viewS3 ∷ S3 → (Double,Double,Double)
-- viewS3 (S3Stereo (V3 t)) = t
--
-- main =  plot' [Interactive] X11 $
--     [ Data3D [Color Blue, Style Dots] [] (map viewS3 cliffPts)
--     , Data3D [Color Red, Style Dots] [] (map viewS3 meridians)
--     -- , Data3D [Color Blue] [] (map viewS3 axisPts)
--     -- , Data3D [Color White, Style Points] [] (map (viewS3.proj) randomPts1)
--     -- , Data3D [Color White, Style Points] [] (map (viewS3.proj) randomPts2)
--     , Data3D [Color White, Style Points] [] (map (viewS3.proj) cut1Pts)
--     ]
--
-- data Boundary = DS (Complex Double) S1 | SD S1 (Complex Double)
--
-- proj ∷ Boundary → S3
-- proj (DS a (S1inC b)) = S3inC2 (V2 (a /. d, b /. d))
--     where d = 1 + normsq' a
-- proj (SD (S1inC a) b) = S3inC2 (V2 (a /. d, b /. d))
--     where d = 1 + normsq' b
--
-- randomPts1 ∷ [Boundary]
-- randomPts1 = map (\(a,b) → DS (fromJust a) b) . filter (isJust . fst) $ [(betaSol t, Angle t) | t <- pts]
--     where
--         k = 500
--         pts = map (*(2*pi/k)) [1..k]
--
-- randomPts2 ∷ [Boundary]
-- randomPts2 = map (\(a,b) → SD b (fromJust a)) . filter (isJust . fst) $ [(betaSol t, Angle t) | t <- pts]
--     where
--         k = 500
--         pts = map (*(2*pi/k)) [1..k]
-- p = 2
-- q = 0.5
-- eqn1 ∷ Double → Diff Double → Diff Double → Diff Double
-- eqn1 t x y = (x+1)^2 + y^2 - ((p * tan (t/2))^2 .* ((x-1)^2 + y^2))
-- eqn2 ∷ Double → Diff Double → Diff Double → Diff Double
-- eqn2 t x y = (x - sin t')^2 + (y- cos t')^2 - ((q * sec (t/2))^2 .* ((x-1)^2 + y^2))
--     where
--         sec a = 1 / cos a
--         t' = constant t
--
-- eqnV ∷ Double → Vec 2 (Diff Double) → Vec 2 (Diff Double)
-- eqnV t (V2 (x,y)) = V2 (eqn1 t x y, eqn2 t x y)
--
-- betaSol t = map (\(V2 (x,y)) → x:+y) $ genNewtonsAD (eqnV t) (V2 (0,0))
--
--
-- swapBoundary ∷ Boundary → Boundary
-- swapBoundary (DS a b) = SD b a
-- swapBoundary (SD a b) = DS b a
--
-- discPts ∷ [Complex Double]
-- discPts =  map (\(x,y) → x:+y) . filter (\(x,y) → x^2 + y^2 <= r2) $ [(x,y) | x <- pts, y <- pts ]
--     where
--         n = 20 -- remember the number of points is ^4 of this
--         r2 = 0.99 -- stay off the edge of the disc
--         pts = map (\k → 2*fromInteger k/fromInteger n *(pi/3) - 1) [0..n] -- put a pi/3 here to avoid degenerate situations (pi/3 ~ 1.1)
--
-- s1Pts ∷ [S1]
-- s1Pts = map Angle pts
--     where
--         k = 50
--         pts = map (*(2*pi/k)) [1..k]
--
--
-- boundaryPts ∷ [Boundary]
-- innerboundaryPts = [DS a b | a <- discPts, b <- s1Pts]
-- outerboundaryPts = map swapBoundary innerboundaryPts
--
-- boundaryPts = innerboundaryPts ++ outerboundaryPts
--
--
--
--
-- -- CUT POINTS
-- f1Infinite ∷ Double → Boundary → Bool
-- f1Infinite eps (DS z (S1inC w)) = f1Infinite' eps z w
-- f1Infinite eps (SD (S1inC z) w) = f1Infinite' eps z w
-- f1Infinite' eps (x:+y) (z:+w) = abs (((z-1)^2 + w^2)*y - ((x-1)^2 + y^2)*w) < eps
--
-- cut1Pts = filter (f1Infinite 0.1) innerboundaryPts






εs ∷ [Double]
εs = map ((2**).negate) [0..]

βs φ α = map ((α+).(flip fromPolar φ)) εs

-- thetaPlimit α φ = converge . map (gamma_minus 0 1) $ zipWith DD (repeat α) (βs φ α)
theta2limit α φ = map (gamma_minus 0 1) $ zipWith DD (repeat α) (βs φ α)
theta2limit' α φ = converge . takeWhile (\(x:+y) → not (isNaN x || isNaN y)) . map gm $ zipWith DD (repeat α) (βs φ α)
    where
        gm pt = 4 * completeE k * ellipticF (mobTrans pt 1) k
            where
                k = modulus pt
        modulus (DD a b) = fromReal $ (-d+f)/(d+f)
            where
                d = norm (a-b)
                f = norm (1 - conjugate a * b)
        mobTrans (DD a b) z = (z*(bb + conjugate a * aa) -(a*bb+aa))/(z*(conjugate a * aa - bb) + (a*bb-aa))
            where
             aa = (a-b) *. norm (1 - conjugate a * b)
             bb = (1 - conjugate a * b) *. norm (a-b)

theta2theory α φ = 2*log (-fromPolar 1 φ * (1+conjugate α)/(1+α))

fLimit ∷ Complex Double → Complex Double
fLimit x = converge . map (ellipticF x) . map fromReal . map ((1-).(2**).negate) $ [0..]
fTheory x = 0.5 * log ((1+x)/(1-x))
