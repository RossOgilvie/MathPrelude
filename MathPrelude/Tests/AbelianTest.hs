{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Tests.AbelianTest where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Abelian
import Test.QuickCheck

prop_assoc a b c = (a + b) + c =~ a + (b + c)
prop_comm a b = a + b =~ b + a
prop_zero a = a + zero =~ a
prop_negate a = a + negate a =~ zero
prop_subtract a b = a + negate b =~ a - b
