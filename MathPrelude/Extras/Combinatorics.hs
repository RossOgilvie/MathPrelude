{-# LANGUAGE RebindableSyntax, OverloadedStrings, BangPatterns #-}
module MathPrelude.Extras.Combinatorics
  ( factorial
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Ring
-- import MathPrelude.Constructions.Complex
-- import MathPrelude.Algebraic.Module
-- import MathPrelude.Common.Transcendental

factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]
