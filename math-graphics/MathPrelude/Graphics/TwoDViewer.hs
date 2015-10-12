{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

module MathPrelude.Graphics.TwoDViewer(
    TwoDPoint
    , Viewer
    , PreGraph
    , graph
    , graphs
    , graphs'
    ) where

import MathPrelude
import Graphics.EasyPlot

type TwoDPoint = (Double,Double)
type Viewer a = a → TwoDPoint
type PreGraph = Color → Graph2D Double Double

preGraph ∷ Viewer a → [a] → PreGraph
preGraph pr pts c = Data2D [Color c, Style Dots] [] $ map pr pts

graph :: Viewer a → [a] → IO Bool
graph pr pts = plot' [] X11 $ preGraph pr pts White

graphs :: Viewer a -> [[a]] -> IO Bool
graphs pr = graphs' . map (preGraph pr)

graphs' ∷ [PreGraph] → IO Bool
graphs' pgs = plot' [] X11 $ zipWith ($) pgs colours
    where
        colours = cycle [White, Yellow, Orange, Red, LightGreen, LightBlue, LightRed, Green, Blue]
