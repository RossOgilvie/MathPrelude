{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE UnicodeSyntax         #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module MathPrelude.Extras.Integration
  ( numIntegrate
  , trapezoid
  , simpsons
  ) where

import           BasicPrelude
import           MathPrelude.Classes.Field
import           MathPrelude.Extras.Convergence
-- import MathPrelude.Constructions.Complex
import           MathPrelude.Classes.Module
-- import MathPrelude.Classes.Transcendental


-- | Numerical integration of a function on from the lower bound to the uper bound via Simpons' method with an increasingly fine partition.
numIntegrate ∷ (Field a, NumEq b, Field b, Module b a) ⇒ (a→b)→a→a→b
numIntegrate f a b = converge . map (simpsons f a b) . map (2^)$ [1..]

-- | The trapezoid rule for a function with the specified number of partitions.
trapezoid ∷ (Field a, Field b, Module b a) ⇒ (a→b) → a → a → Integer → b
trapezoid f a b n = h .* sum (map f xs)
  where
    h = (b-a) / fromInteger n
    xs = map (\k → a + (k+1)*h) . map fromInteger $ [0..(n-1)]

-- | Simpons' rule for a function with the specified number of partitions.
simpsons ∷ (Field a, Field b, Module b a) ⇒ (a→b) → a → a → Integer → b
simpsons f a b n = (h / 3) .* (fa + fb + 2*sum evens + 4*sum odds)
  where
    fa = f a
    fb = f b
    h = (b-a) / fromInteger (2*n)
    evens = map f . map (\k → a + 2*k*h) . map fromInteger  $ [1..(n-1)]
    odds = map f . map (\k → a + (2*k + 1)*h) . map fromInteger $ [0..(n-1)]
