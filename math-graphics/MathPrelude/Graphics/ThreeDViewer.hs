{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

module MathPrelude.Graphics.ThreeDViewer(
    ThreeDPoint
    , Viewer
    , PreGraph
    , pointsToPreGraph
    , plotPoints
    , plotPoints'
    , plotPreGraph
    , plotPreGraphs
    ) where

import MathPrelude
import Graphics.EasyPlot

type ThreeDPoint = (Double,Double,Double)
type Viewer a = a → ThreeDPoint
type PreGraph = Color → Graph3D Double Double Double


plotPreGraph ∷ PreGraph → IO Bool
plotPreGraph pg = plot' [Interactive, Debug] X11 $ pg White

plotPreGraphs ∷ [PreGraph] → IO Bool
plotPreGraphs pgs = plot' [Interactive, Debug] X11 $ zipWith ($) pgs colours
    where
        colours = cycle [White, Yellow, Orange, Red, LightGreen, LightBlue, LightRed, Green, Blue]


pointsToPreGraph ∷ Viewer a → [a] → PreGraph
pointsToPreGraph vw pts c = Data3D [Color c, Style Dots] [] $ map vw pts

plotPoints :: Viewer a → [a] → IO Bool
plotPoints vw pts = plotPreGraph $ pointsToPreGraph vw pts

plotPoints' :: Viewer a -> [[a]] -> IO Bool
plotPoints' vw = plotPreGraphs . map (pointsToPreGraph vw)
