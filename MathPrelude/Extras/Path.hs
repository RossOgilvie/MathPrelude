{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleContexts #-}
module MathPrelude.Extras.Path
  ( Path
  ) where

import BasicPrelude
import MathPrelude.Structures.Field
import MathPrelude.Structures.Module
import MathPrelude.Extras.Integration
import MathPrelude.Structures.Complex
import MathPrelude.Common.CharZero
import MathPrelude.Common.Transcendental

data Path a = Path {path :: Double -> a, start :: Double, end :: Double}

pathIntegration :: (Field b, Module b Double, NumEq b) => (a -> b) -> Path a -> b
pathIntegration f g = numIntegrate (f. path g) (start g) (end g)

residue f z = pathIntegration f g
  where g = Path (\t -> z + fromPolar 1e-3 t) (-pi) (pi)
