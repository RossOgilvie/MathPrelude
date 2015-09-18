{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

module MathPrelude.Graphics.ThreeDViewer(
    ThreeDPoint
    , Viewer
    , PreGraph
    , graph
    , graphs
    , graphs'
    ) where

import MathPrelude
import Graphics.EasyPlot

type ThreeDPoint = (Double,Double,Double)
type Viewer a = a → ThreeDPoint
type PreGraph = Color → Graph3D Double Double Double

preGraph ∷ Viewer a → [a] → PreGraph
preGraph pr pts c = Data3D [Color c, Style Dots] [] $ map pr pts

graph :: Viewer a → [a] → IO Bool
graph pr pts = plot' [Interactive] X11 $ preGraph pr pts White

graphs :: Viewer a -> [[a]] -> IO Bool
graphs pr = graphs' . map (preGraph pr)

graphs' ∷ [PreGraph] → IO Bool
graphs' pgs = plot' [Interactive] X11 $ zipWith ($) pgs colours
    where
        colours = cycle [White, Yellow, Orange, Red, LightGreen, LightBlue, LightRed, Green, Blue]
