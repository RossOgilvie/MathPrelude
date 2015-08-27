{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE DataKinds #-}
module Den
    ( main
    ) where

import MathPrelude
import MathPrelude.Classes.Module
import MathPrelude.Classes.Norm
import MathPrelude.Constructions.Complex
import MathPrelude.SpecialFunctions.EllipticFunctions

import Differentials

import Graphics.EasyPlot



main = mainDen1



stdOptions :: [Option3D Double Double Double]
stdOptions = [RangeX (-1) 1, RangeY (-1) 1]


numberOfPts = 30

---- find moduli points by brute force testing a grid
discPts :: [Complex Double]
discPts =  map (uncurry (:+)) . filter (\(x,y) → x^2 + y^2 <= r2) $ [(x,y) | x <- pts, y <- pts ]
    where
        n = numberOfPts -- remember the number of points is ^4 of this
        r2 = 0.99 -- stay off the edge of the disc
        pts = map (\k → 2*fromInteger k/fromInteger n *(pi/3) - 1) [0..n] -- put a pi/3 here to avoid degenerate situations (pi/3 ~ 1.1)

ddPts = filter offDiag . filter inDisc $ [DD α β | α <- discPts, β <- discPts]

nearInteger x eps = d' < eps || d_ < eps
    where
        x' = fromInteger $ ceiling x
        d' = x' - x -- x'>x so this is always pos
        x_ = fromInteger $ floor x
        d_ = x - x_  -- x_<x so this is always pos too



den1 (DD α β) = (conjugate α - 1) * a + (α-1) * b
    where
        a = (α-β) *. norm (1-conjugate α * β)
        b = (1-conjugate α * β) *. norm (α-β)

isDen1Zero dd = norm (den1 dd) < 0.001

mainDen1 = do
    let pts = filter isDen1Zero ddPts
    mainGraph pts




---- boundary pts
s1pts :: [Complex Double]
s1pts = map (fromPolar 1) $ map (*(2*pi/s1ptsn)) [1..s1ptsn]
s1ptsn = 100 :: Double

boundaryPts = [DD a b | a <- s1pts, b <- s1pts]

conformalPts = map (flip DD 0) discPts ++ map (DD 0) discPts

singularPts = zipWith DD discPts discPts

symPts = map (DD 1) s1pts ++ map (flip DD 1) s1pts ++ map (DD (-1)) s1pts ++ map (flip DD (-1)) s1pts







-- CUT POINTS
f1Infinite eps (DD (x:+y) (z:+w)) = abs (((z-1)^2 + w^2)*y - ((x-1)^2 + y^2)*w) < eps

cutPts = filter (f1Infinite 0.001) ddPts






mainGraph :: [DD] → IO Bool
mainGraph pts = do
    plot' [Interactive] X11 $
        [
        -- Data3D [Color White, Style Dots] [] (map projby pts)
        Data3D [Color Yellow, Style Dots] [] (map projbx pts)
        -- , Data3D [Color White, Style Dots] [] (map projby pts)
        -- , Data3D [Color Green, Style Points] [] (map projby [startPt])
        -- , Data3D [Color Blue, Style Dots] [] (map projbx boundaryPts)
        -- , Data3D [Color White, Style Dots] [] (map projbx symPts)
        -- , Data3D [Color White, Style Dots] [] (map projby pts)
        -- , Data3D [Color Green, Style Points] [] (map projay [startPt])
        -- , Data3D [Color Blue, Style Dots] [] (map projay boundaryPts)
        -- , Data3D [Color Green, Style Dots] [] (map projay conformalPts)
        -- , Data3D [Color White, Style Dots] [] (map projay singularPts)
        ]

colours = cycle [Blue, White, Yellow, Orange, LightGreen, LightBlue,LightRed]
mainGraphs :: (DD → (Double,Double,Double)) → [[DD]] → IO Bool
mainGraphs proj pts = do
    let zipper c ps = Data3D [Color c, Style Dots] stdOptions (map proj ps)
    plot' [Interactive] X11 $
        zipWith zipper colours pts
