{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

module MathPrelude.Graphics.ThreeDViewer(
    ThreeDPoint(..)
    , Viewer(..)
    , graph
    , graphs
    ) where

import MathPrelude
import Graphics.EasyPlot

type ThreeDPoint = (Double,Double,Double)
type Viewer a = a → ThreeDPoint

toGraph ∷ Color → Viewer a → [a] → Graph3D Double Double Double
toGraph c pr pts = Data3D [Color c, Style Dots] [] $ map pr pts


graph :: Viewer a → [a] → IO Bool
graph pr = plot' [Interactive] X11 . toGraph White pr

graphs :: Viewer a -> [[a]] -> IO Bool
graphs pr pts = plot' [Interactive] X11 $ zipWith zipper colours pts
    where
        zipper c ps = Data3D [Color c, Style Dots] [] (map pr ps)
        colours = cycle [Blue, White, Yellow, Orange, LightGreen, LightBlue, LightRed]
