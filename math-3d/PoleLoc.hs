{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
module PoleLoc where

import MathPrelude
import MathPrelude.Classes.Module
import MathPrelude.Classes.Norm
import MathPrelude.Constructions.Complex


-- import Differentials

import Graphics.EasyPlot

main =  plot' [Interactive] X11
    [
    Data3D [Color Blue, Style Dots] [] (map viewC poles)
    , Data3D [Color White, Style Points] [] (map viewC axes)
    ]

viewC ∷ Complex Double → (Double, Double, Double)
viewC (x :+ y) = (x, y, 0)

discPts ∷ [Complex Double]
discPts =  map (uncurry (:+)) . filter (\(x,y) → x^2 + y^2 <= r2) $ [(x,y) | x <- pts, y <- pts ]
    where
        n = 10
        r2 = 0.99 -- stay off the edge of the disc
        pts = map (\k → 2*fromInteger k/fromInteger n *(pi/3) - 1) [0..n] -- put a pi/3 here to avoid degenerate situations (pi/3 ~ 1.1)

mobTrans :: Complex Double → Complex Double -> Complex Double -> Complex Double
mobTrans α β ζ = (ζ*(conjugate α * aa + bb) - (aa + α*bb))/(ζ*(conjugate α * aa - bb) - (aa - α*bb))
    where
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)

mixWith f xs ys = map (uncurry f) (pairs xs ys)
pairs xs ys = [(x, y) | x <- xs, y <- ys]

poles ∷ [Complex Double]
poles = [mobTrans α β 0 | α <- discPts, β <- discPts]

axes ∷ [Complex Double]
axes = [ x :+ 0 | x<- [-2,-1.9..2]] ++ [ 0 :+ y | y<- [-2,-1.9..2]]
