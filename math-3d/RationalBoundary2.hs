{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
module RationalBoundary2
    ( Boundary(..)
    , boundaryArc
    , main
    ) where

import MathPrelude
import MathPrelude.Classes.Module
import MathPrelude.Classes.Norm
import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector
import MathPrelude.Constructions.Sphere1
-- import MathPrelude.Constructions.Sphere3
import MathPrelude.Calculus.Derivation
import MathPrelude.Calculus.NewtonsMethod

import Differentials
import DD

import Graphics.EasyPlot
import Data.Maybe





main =  plot' [Interactive] X11
    -- linkfuckaround
    zwfuckaround

linkfuckaround = [
    -- Data3D [Color Blue, Style Dots] [] (map viewBoundary $ boundaryPts 50)
    -- , Data3D [Color Green, Style Dots] [] (map viewBoundary $ boundaryArc (1,2) True)
    -- , Data3D [Color Red, Style Dots] [] (map viewBoundary $ boundaryArc (1,0) False)
    -- , Data3D [Color Yellow, Style Dots] [] (map viewBoundary $ boundaryArc (1,1) False)
    -- , Data3D [Color White, Style Dots] [] (map viewBoundary $ boundaryArc (1,2) False)
    -- , Data3D [Color Yellow, Style Dots] [] (map viewZinf $ boundaryArc (1,2) False)
    -- , Data3D [Color Orange, Style Dots] [] (map viewBoundary $ boundaryArc (1,3) False)
    Data3D [Color Green, Style Dots] [] (map viewBoundary . boundaryArc (4/3,4/3) $ True)
    , Data3D [Color White, Style Dots] [] (map viewBoundary . boundaryArc (4/3,5/3) $ False)
    , Data3D [Color Yellow, Style Dots] [] (map viewBoundary . boundaryArc (4/3,6/3) $ True)
    , Data3D [Color Red, Style Dots] [] (map viewBoundary . boundaryArc (4/3,7/3) $ False)
    , Data3D [Color Blue, Style Dots] [] (map viewBoundary . boundaryArc (4/3,8/3) $ True)
    ]

zwfuckaround = [
    Data3D [Color Green, Style Dots] [] (filterBigCrap . map viewzw . map b2zw . boundaryArc (4/3,4/3) $ True)
    , Data3D [Color White, Style Dots] [] (filterBigCrap . map viewzw . map b2zw . boundaryArc (4/3,5/3) $ False)
    , Data3D [Color Yellow, Style Dots] [] (filterBigCrap . map viewzw . map b2zw . boundaryArc (4/3,6/3) $ True)
    , Data3D [Color Red, Style Dots] [] (filterBigCrap . map viewzw . map b2zw . boundaryArc (4/3,7/3) $ False)
    , Data3D [Color Orange, Style Dots] [] (filterBigCrap . map viewzw . map b2zw . boundaryArc (4/3,8/3) $ True)
    , Data3D [Color LightBlue, Style Dots] [] (filterBigCrap . map viewzw . map b2zw . boundaryArc (4/3,9/3) $ False)
    ]



viewZinf ∷ Boundary → (Double,Double,Double)
viewZinf p@(SD (Angle θ) _) = (fixθ θ, a, b)
    where
        (a:+b) = zinf' p



filterCrap ∷ [(Double,Double,Double)] → [(Double,Double,Double)]
filterCrap = filter (\(_,_,r) → abs r < 0.95)

crapThreshold = 100
filterBigCrap ∷ [(Double,Double,Double)] → [(Double,Double,Double)]
filterBigCrap = filter (\(x,y,z) → abs x < crapThreshold && abs y < crapThreshold && abs z < crapThreshold)




boundaryArc ∷ (Double, Double) → Bool → [Boundary]
boundaryArc pq True = dsGoodPts pq
boundaryArc pq False = sdGoodPts pq






dsGoodPts ∷ (Double, Double) → [Boundary]
dsGoodPts (p, q) = map (\(a,b) → DS (fromJust a) b) . filter (isJust . fst) $ [(βSol p q φ, Angle φ) | φ <- pts]
    where
        n = 400 :: Double
        -- we have squared our equations, losing some sign info
        -- sin t and q need to have the same sign
        pts = (if q < 0 then map (*(-1)) else id) . map (*(pi/n)) $ [1..(n-1)]

-- There are two solutions to the equations

---- based on revised equations in write up
----- with β \in \S^1
---- the second versions (with sqrt's) run faster but fail on a lot of points.
eqn1β ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn1β p φ αx αy = ((1-αx)^2+αy^2) * t^2 - (p^2) .* ((1+αx)^2+αy^2)
-- eqn1 p φ αx αy = sqrt ((1-αx)^2+αy^2) * t - p .* sqrt ((1+αx)^2+αy^2)
    where
        -- a dirty abs function for t
        φ' = constant . abs $ φ
        t = tan (φ'/2)
--
eqn2β ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn2β q φ αx αy = (((1-αx)^2 + αy^2)*((1-βx)^2 + βy^2)) - (q*sin φ)^2 .* ((αx-βx)^2 + (αy-βy)^2)
-- eqn2 q φ αx αy = sqrt (((1-αx)^2 + αy^2)*((1-βx)^2 + βy^2)) - (q*sin φ) .* sqrt ((αx-βx)^2 + (αy-βy)^2)
    where
        βx = cos φ'
        βy = sin φ'
        φ' = constant φ

eqnVβ ∷ Double → Double → Double → Vec 2 (Diff Double) → Vec 2 (Diff Double)
eqnVβ p q φ (V2 (x,y)) = V2 (eqn1β p φ x y, eqn2β q φ x y)

βSol' p q φ = genNewtonsAD (eqnVβ p q φ) (V2 (0,0))
βSol p q φ = map (\(V2 (x,y)) → x:+y) $ βSol' p q φ











sdGoodPts ∷ (Double, Double) → [Boundary]
sdGoodPts (p, q) = map (\(b,a) → SD a (fromJust b)) . filter (isJust . fst) $ [(αSol p q φ, Angle φ) | φ <- pts]
    where
        n = 800 :: Double
        -- we have squared our equations, losing some sign info
        -- sin t and q need to have the same sign
        -- pts = (if q < 0 then map (*(-1)) else id) . map (*(pi/n)) $ [1..(n-1)]
        -- pts = map (*(2*pi/n)) $ [1..(n-1)]
        pts = (if q < 0 then map (*(-1)) else id) . map (*(pi/n)) $ [1..(n-1)]


----- with α \in \S^1
eqn1α ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn1α p φ βx βy = ((1-βx)^2+βy^2) * t^2 - (p^2) .* ((1+βx)^2+βy^2)
    where
        φ' = constant . abs $ φ
        t = tan (φ'/2)
--
eqn2α ∷ Double → Double → Diff Double → Diff Double → Diff Double
eqn2α q φ βx βy = (((1-βx)^2 + βy^2)*((1-αx)^2 + αy^2)) - (q*sin φ)^2 .* ((βx-αx)^2 + (βy-αy)^2)
    where
        αx = cos φ'
        αy = sin φ'
        φ' = constant φ

eqnVα ∷ Double → Double → Double → Vec 2 (Diff Double) → Vec 2 (Diff Double)
eqnVα p q φ (V2 (x,y)) = V2 (eqn1α p φ x y, eqn2α q φ x y)

αSol' p q φ = genNewtonsAD (eqnVα p q φ) (V2 (0,0))
αSol ∷ Double → Double → Double → Maybe (Complex Double)
αSol p q φ = map (\(V2 (x,y)) → x:+y) $ αSol' p q φ


zinf' :: Boundary -> Complex Double
zinf' (SD α' β) = (-α)*(1 - 2*conjugate α*β + normsq β)/(1 - 2*α*conjugate β + normsq β)
    where α = toC α'



-- bruteForce ∷ (Complex Double, Complex Double) → Bool
bruteForce (α, β) = eqn2
    where
        -- aa = (α-β) *. norm (1 - conjugate α * β)
        -- bb = (1 - conjugate α * β) *. norm (α-β)
        -- zinf = (aa - α*bb)/(conjugate α * aa - bb)
        zinf = (-α)*(1 - 2*conjugate α*β + normsq β)/(1 - 2*α*conjugate β + normsq β)
        eqn2 = normsq (1-α) * normsq (1-β) / normsq (α-β) * (2*zinf/(1-zinf^2)/iu)^2













--
-- cutPts ∷ [Boundary]
-- cutPts' = [DS a (Angle t) | a <- discPts, t <- [0,pi]]
-- cutPts = cutPts' ++ map swapBoundary cutPts'

rationalPt ∷ Bool → Double → Double → [(Double,Double,Double)]
rationalPt top s t
    | top = [(s',t',0)]
    | otherwise = [(t',s',0)]
    where
        s' = fixθ . (*2) . atan $ s
        t' = fixθ . (*2) . atan $ t

-- limitPoints ∷ Double → [(Double,Double,Double)]
-- limitPoints p = [(θ0,θ0,0),(θ1,θ1,0)]
--     where
--         θ0 = fixθ . (*2) . atan . sqrt $ p
--         θ1 = 2*pi - θ0
--         -- θ2 = fixθ . (*2) . atan . sqrt $ 0.2
--         -- θ3 = 2*pi - θ2







-- An alternative parameterisation of the p-constant balls.
-- This is meant to represent the imaginary coords of the pair of points f(1) and f(-1), followed by k
type ZWcoords = (Double, Double, Double)

b2zw ∷ Boundary → ZWcoords
b2zw bound = (imagPart f1, imagPart fn1, 0)
    where
        dd = includeBoundary bound
        k = realPart $ modulus dd
        -- k = if k' > 1 then 1/k' else k'
        f1 = mobTrans dd 1
        fn1 = mobTrans dd (-1)

-- it seems to go off to infinity alot, which is annoying.
-- flip it back to go to zero, but then put it on a higher k level so i can still see different parts
viewzw ∷ ZWcoords → (Double, Double, Double)
viewzw (a, b, k)
    | abs a > 1 && abs b > 1= (1/a, 1/b , k+2)
    | abs a > 1 = (1/a, b , k+1)
    | abs b > 1 = (a, 1/b , k+1)
    | otherwise = (a,b,k)
