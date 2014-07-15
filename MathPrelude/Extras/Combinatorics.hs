{-# LANGUAGE RebindableSyntax, OverloadedStrings, BangPatterns #-}
module MathPrelude.Extras.Combinatorics
  ( factorial
  ) where

import BasicPrelude
import MathPrelude.Structures.Ring
-- import MathPrelude.Structures.Complex
-- import MathPrelude.Structures.Module
-- import MathPrelude.Common.Transcendental

factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]
