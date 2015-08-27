{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module RationalBoundary
    ( Boundary(..)
    , boundaryArc
    , boundaryLink
    , limitPoints
    , Dir(..)
    , genChains
    , main
    ) where

import MathPrelude
import MathPrelude.Classes.Module
-- import MathPrelude.Classes.Norm
import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector
import MathPrelude.Constructions.Sphere1
-- import MathPrelude.Constructions.Sphere3
import MathPrelude.Calculus.Derivation
import MathPrelude.Calculus.NewtonsMethod

-- import Differentials
import DD

import Graphics.EasyPlot
import Data.Maybe

main =  plot' [Interactive] X11
    -- link5special
    -- link5normal
    -- link35normal
    -- link1loop
    linkfuckaround

linkfuckaround = [
    Data3D [Color Blue, Style Dots] [] (map viewBoundary $ boundaryPts 50)
    -- , Data3D [Color Blue, Style Dots] [] (map viewBoundary cutPts)
    -- , Data3D [Color Violet, Style Points] [] (limitPoints 5)
    -- , Data3D [Color Green, Style Dots] [] (map viewBoundary $ boundaryLink (5,32) DecQ 17 )
    , Data3D [Color Green, Style Dots] [] (map viewBoundary $ boundaryArc (11/5,0.9) True)
    ]

link5normal = [
    Data3D [Color Blue, Style Dots] [] (map viewBoundary $ boundaryPts 50)
    -- , Data3D [Color Blue, Style Dots] [] (map viewBoundary cutPts)
    -- normal links
    -- p = 5
    , Data3D [Color Violet, Style Points] [] (limitPoints 5)
    , Data3D [Color Green, Style Dots] [] (map viewBoundary $ boundaryLink (5,32) DecQ 17 )
    -- , Data3D [Color Red, Style Dots] [] (map viewBoundary $ boundaryLink (5,19) DecQ 10 )
    -- , Data3D [Color DarkOrange, Style Dots] [] (map viewBoundary $ boundaryLink (5,18) DecQ 10 )
    -- p = 0.2
    , Data3D [Color Violet, Style Points] [] (limitPoints 0.2)
    , Data3D [Color LightRed, Style Dots] [] (map viewBoundary $ boundaryLink (0.2,3.7) DecQ 10 )
    -- , Data3D [Color LightGreen, Style Dots] [] (map viewBoundary $ boundaryLink (0.2,3.6) DecQ 10 )
    -- , Data3D [Color Orange, Style Dots] [] (map viewBoundary $ boundaryLink (0.2,3.5) DecQ 10 )
    -- The two exceptional links
    -- , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryLink (5,21) DecQ 10 )
    -- , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (5,-21) IncQ 10 )
    ]

link35normal = [
    Data3D [Color Blue, Style Dots] [] (map viewBoundary $ boundaryPts 50)
    -- , Data3D [Color Blue, Style Dots] [] (map viewBoundary cutPts)
    -- normal links
    -- p = 3/5
    , Data3D [Color Violet, Style Points] [] (limitPoints 0.6)
    , Data3D [Color Green, Style Dots] [] (map viewBoundary $ boundaryLink (0.6,2) DecQ 10 )
    , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryLink (0.6,2.1) DecQ 10 )
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (0.6,2.2) DecQ 10 )
    , Data3D [Color Red, Style Dots] [] (map viewBoundary $ boundaryLink (0.6,2.3) DecQ 10 )
    , Data3D [Color Orange, Style Dots] [] (map viewBoundary $ boundaryLink (0.6,2.4) DecQ 10 )
    -- , Data3D [Color Red, Style Dots] [] (map viewBoundary $ boundaryLink (5,19) DecQ 10 )
    -- , Data3D [Color DarkOrange, Style Dots] [] (map viewBoundary $ boundaryLink (5,18) DecQ 10 )
    -- p = 0.2
    -- , Data3D [Color Violet, Style Points] [] (limitPoints 0.2)
    -- , Data3D [Color LightRed, Style Dots] [] (map viewBoundary $ boundaryLink (0.2,3.7) DecQ 10 )
    -- , Data3D [Color LightGreen, Style Dots] [] (map viewBoundary $ boundaryLink (0.2,3.6) DecQ 10 )
    -- , Data3D [Color Orange, Style Dots] [] (map viewBoundary $ boundaryLink (0.2,3.5) DecQ 10 )
    -- The two exceptional links
    -- , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryLink (5,21) DecQ 10 )
    -- , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (5,-21) IncQ 10 )
    ]

link1loop = [
    Data3D [Color Violet, Style Points] [] (limitPoints 1)
    -- p=1 Loops
    -- q>0
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (1,2) IncQ 2 )
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (1,3) IncQ 2 )
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (1,4) IncQ 2 )
    -- q<0
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (1,-2) IncQ 2 )
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (1,-3) IncQ 2 )
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (1,-4) IncQ 2 )
    ]

link5special = [
    Data3D [Color Blue, Style Dots] [] (map viewBoundary $ boundaryPts 50)
    , Data3D [Color Violet, Style Points] [] (limitPoints 5)
    -- , Data3D [Color Green, Style Dots] [] (map viewBoundary $ boundaryArc (5, 9) True)
    -- , Data3D [Color White, Style Points] [] (rationalPt False (sqrt (2*5^2/(5-1))) (sqrt ((5-1)/2)))
    -- , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryArc (5, 5) False)
    -- , Data3D [Color White, Style Points] [] [(pi,0,0)]
    -- , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryArc (0.2, 1) True)
    -- , Data3D [Color White, Style Points] [] (rationalPt True (sqrt (0.2*0.8/2)) (sqrt (2*0.2/0.8)))
    , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryLink (5,9) DecQ 10)
    ]

link0'2normal = [
    Data3D [Color Blue, Style Dots] [] (map viewBoundary $ boundaryPts 50)
    -- normal links
    , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryLink (0.2,18) DecQ 6 )
    -- , Data3D [Color Green, Style Dots] [] (map viewBoundary $ boundaryLink (5,19) False 10 )
    -- , Data3D [Color Red, Style Dots] [] (map viewBoundary $ boundaryLink (5,20) False 10 )
    -- , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryLink (5,21) False 10 )
    , Data3D [Color Violet, Style Points] [] (limitPoints 0.2)
    ]

filterCrap ∷ [(Double,Double,Double)] → [(Double,Double,Double)]
filterCrap = filter (\(_,_,r) → abs r < 0.95)





data Dir = IncQ | DecQ deriving (Show,Read,Eq)

boundaryLink ∷ (Double, Double) → Dir → Integer → [Boundary]
boundaryLink pq dir steps = concatMap (uncurry boundaryArc) $ zip chain altSides
    where
        chain = take steps $ genChains pq dir
        altSides = cycle [True,False]

genChains ∷ (Double,Double) → Dir → [(Double,Double)]
genChains (p,q) dir = pqs
    where
        seed = (p,q,dir)
        stepper (p,q,d)
            | d == IncQ = upArc (p,q)
            | d == DecQ = downArc (p,q)
        pqs' = iterate stepper seed
        stripDirs (x,y,_) = (x,y)
        pqs = map stripDirs pqs'

upArc ∷ (Double,Double) → (Double,Double,Dir)
upArc (p, q)
    | p < 1 = f1
    | p > 1 = f2
    | otherwise = (p, q, IncQ)
    where
        f1
            | (-1 < q) && (q < -p) = (p,q + (p+1), IncQ)
            | q =~ -1 = (1/p,-1/p, DecQ)
            | otherwise = (p,q + (1-p), IncQ)
        f2
            | (-p < q) && (q < -1) = (p,q + (p+1), IncQ)
            | q =~ -p = (1/p,-1, DecQ)
            | otherwise = (p,q + (p-1), IncQ)
downArc (p, q)
    | p < 1 = f1
    | p > 1 = f2
    | otherwise = (p, q, DecQ)
    where
        f1
            | (p < q) && (q < 1) = (p,q - (p+1), DecQ)
            | q =~ 1 = (1/p,1/p, IncQ)
            | otherwise = (p,q - (1-p), DecQ)
        f2
            | (1 < q) && (q < p) = (p,q - (p+1), DecQ)
            | q =~ p = (1/p,1, IncQ)
            | otherwise = (p,q - (p-1), DecQ)

boundaryArc ∷ (Double, Double) → Bool → [Boundary]
boundaryArc pq top = (if top then id else map swapBoundary) $ dsGoodPts pq

dsGoodPts ∷ (Double, Double) → [Boundary]
dsGoodPts (p, q) = map (\(a,b) → DS (fromJust a) b) . filter (isJust . fst) $ [(betaSol p q φ, Angle φ) | φ <- pts]
    where
        n = 400 :: Double
        -- we have squared our equations, losing some sign info
        -- sin t and q need to have the same sign
        pts = (if q < 0 then map (*(-1)) else id) . map (*(pi/n)) $ [1..(n-1)]

sdGoodPts ∷ (Double, Double) → [Boundary]
sdGoodPts = map swapBoundary . dsGoodPts


---- based on revised equations in write up
---- the second versions (with sqrt's) run faster but fail on a lot of points.
eqn1 ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn1 p φ αx αy = ((1-αx)^2+αy^2) * t^2 - (p^2) .* ((1+αx)^2+αy^2)
-- eqn1 p φ αx αy = sqrt ((1-αx)^2+αy^2) * t - p .* sqrt ((1+αx)^2+αy^2)
    where
        -- a dirty abs function for t
        φ' = constant . abs $ φ
        t = tan (φ'/2)
--
eqn2 ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn2 q φ αx αy = (((1-αx)^2 + αy^2)*((1-βx)^2 + βy^2)) - (q*sin φ)^2 .* ((αx-βx)^2 + (αy-βy)^2)
-- eqn2 q φ αx αy = sqrt (((1-αx)^2 + αy^2)*((1-βx)^2 + βy^2)) - (q*sin φ) .* sqrt ((αx-βx)^2 + (αy-βy)^2)
    where
        βx = cos φ'
        βy = sin φ'
        φ' = constant φ

eqnV ∷ Double → Double → Double → Vec 2 (Diff Double) → Vec 2 (Diff Double)
eqnV p q φ (V2 (x,y)) = V2 (eqn1 p φ x y, eqn2 q φ x y)

betaSol' p q φ = genNewtonsAD (eqnV p q φ) (V2 (0,0))
-- betaSol p q t = map (\(V2 (x,y)) → x:+y) $ betaSol' p q t
betaSol p q φ = map (\(V2 (x,y)) → x:+y) $ betaSol' p q φ

solvesEqns ∷ Double → Double → Boundary → Bool
solvesEqns p q (DS (αx:+αy) (Angle t)) = (abs v1 < ε) && (abs v2 < ε)
    where
        v1 = value (eqn1 p t (constant αx) (constant αy))
        v2 = value (eqn2 q t (constant αx) (constant αy))
        ε = 0.001
solvesEqns p q sd = solvesEqns p q (swapBoundary sd)












rationalPt ∷ Bool → Double → Double → [(Double,Double,Double)]
rationalPt top s t
    | top = [(s',t',0)]
    | otherwise = [(t',s',0)]
    where
        s' = fixθ . (*2) . atan $ s
        t' = fixθ . (*2) . atan $ t

limitPoints ∷ Double → [(Double,Double,Double)]
limitPoints p = [(θ0,θ0,0),(θ1,θ1,0)]
    where
        θ0 = fixθ . (*2) . atan . sqrt $ p
        θ1 = 2*pi - θ0
        -- θ2 = fixθ . (*2) . atan . sqrt $ 0.2
        -- θ3 = 2*pi - θ2
