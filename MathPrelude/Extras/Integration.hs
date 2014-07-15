{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses #-}
module MathPrelude.Extras.Integration
  ( numIntegrate
  , trapezoid
  , simpsons
  ) where

import BasicPrelude
import MathPrelude.Structures.Field
import MathPrelude.Extras.Sequence
-- import MathPrelude.Structures.Complex
import MathPrelude.Structures.Module
-- import MathPrelude.Common.Transcendental



numIntegrate :: (Field a, NumEq b, Field b, Module b a) => (a->b)->a->a->b
numIntegrate f a b = converge . map (simpsons f a b) . map (2^)$ [1..]

trapezoid :: (Field a, Field b, Module b a) => (a->b) -> a -> a -> Integer -> b
trapezoid f a b n = h .* sum (map f xs)
  where
    h = (b-a) / fromInteger n
    xs = map (\k -> a + (k+1)*h) . map fromInteger $ [0..(n-1)]

simpsons :: (Field a, Field b, Module b a) => (a->b) -> a -> a -> Integer -> b
simpsons f a b n = (h / 3) .* (fa + fb + 2*sum evens + 4*sum odds)
  where
    fa = f a
    fb = f b
    h = (b-a) / fromInteger (2*n)
    evens = map f . map (\k -> a + 2*k*h) . map fromInteger  $ [1..(n-1)]
    odds = map f . map (\k -> a + (2*k + 1)*h) . map fromInteger $ [0..(n-1)]
