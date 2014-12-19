{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
module MathPrelude.Extras.DifferentialEqns
  ( module MathPrelude.Constructions.PowerSeries
  , firstODE
  , secondODE
  , expPS
  , sinPS, cosPS, tanPS
  , hypergeometricPS
  ) where

----------------------------------
-- Imports
----------------------------------
import           BasicPrelude
import           MathPrelude.Classes.Integral
import           MathPrelude.Constructions.PowerSeries
import           MathPrelude.Extras.Combinatorics
----------------------------------
-- Methods
----------------------------------
-- | Solve a first order differential equation of the form y' = f y, satisfying the initial condition y0 via a power series.
firstODE ∷ Field a ⇒ (PS a → PS a) → a → PS a
firstODE f y0 =
  let
    y = PS $ defIntegrate y0 y'
    PS y' = f y
  in y

-- | Solve a second order differential equation of the form y'' = f y y', satisfying the initial conditions y0 y0' via a power series.
secondODE ∷ Field a ⇒ (PS a → PS a → PS a) → a → a → PS a
secondODE f y0 y0' =
  let
    y = defIntegrate y0 y'
    y' = defIntegrate y0' y''
    PS y'' = f (PS y) (PS y')
  in PS y

-- | Definite integration for a power series with given constant term.
defIntegrate c p = c : zipWith (/) p (map fromInteger [1..])

-- | The power series of the exponential function
expPS ∷ PS Double
expPS = firstODE id 1

-- | The power series of the sine function
sinPS ∷ PS Double
sinPS = secondODE (\y _ → - y) 0 1

-- | The power series of the cosine function
cosPS ∷ PS Double
cosPS = secondODE (\y _ → - y) 1 0

-- | The power series of the tangent function
tanPS ∷ PS Double
tanPS = sinPS / cosPS

-- | The power series of the classical hypergeometric functions 2_F_1(a,b;c;z)
hypergeometricPS ∷ Field a ⇒ a → a → a → PS a
hypergeometricPS a b c = fromGenFunc ((\n → pochhammer a n * pochhammer b n/pochhammer c n/ fromInteger (factorial n)).fromIntegral)
