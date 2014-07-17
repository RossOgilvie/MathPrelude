{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.DifferentialEqns
  (
  -- module MathPrelude.Constructions.Complex
  -- agm
  -- , ellipticK
  -- , ellipticE
  ) where

----------------------------------
-- Imports
----------------------------------
import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Constructions.PowerSeries
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

secODE :: Field a => (PS a -> PS a -> PS a) -> a -> a -> PS a
secODE f y0 y0' =
  let
    y = defIntegrate y0 y'
    y' = defIntegrate y0' y''
    PS y'' = f (PS y) (PS y')
  in PS y

defIntegrate c p = c : zipWith (/) p (map fromInteger [1..])

expPS :: PS Double
expPS = firstODE id 1

sinPS :: PS Double
sinPS = secODE (\y _ -> - y) 0 1
