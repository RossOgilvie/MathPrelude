{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.DifferentialEqns
  ( module MathPrelude.Constructions.PowerSeries
  , firstODE
  , secondODE
  , expPS
  , sinPS, cosPS, tanPS
  ) where

----------------------------------
-- Imports
----------------------------------
import BasicPrelude
import MathPrelude.Constructions.PowerSeries
import MathPrelude.Extras.Combinatorics
import MathPrelude.Common.Integral
----------------------------------
-- Methods
----------------------------------

-- y' = f y
firstODE :: Field a => (PS a -> PS a) -> a -> PS a
firstODE f y0 =
  let
    y = PS $ defIntegrate y0 y'
    PS y' = f y
  in y

secondODE :: Field a => (PS a -> PS a -> PS a) -> a -> a -> PS a
secondODE f y0 y0' =
  let
    y = defIntegrate y0 y'
    y' = defIntegrate y0' y''
    PS y'' = f (PS y) (PS y')
  in PS y

defIntegrate c p = c : zipWith (/) p (map fromInteger [1..])

expPS :: PS Double
expPS = firstODE id 1

sinPS :: PS Double
sinPS = secondODE (\y _ -> - y) 0 1

cosPS :: PS Double
cosPS = secondODE (\y _ -> - y) 1 0

tanPS :: PS Double
tanPS = sinPS / cosPS

hypergeometricPS :: Field a => a -> a -> a -> PS a
hypergeometricPS a b c = fromGenFunc ((\n -> pochhammer a n * pochhammer b n/pochhammer c n/ fromInteger (factorial n)).fromIntegral)
