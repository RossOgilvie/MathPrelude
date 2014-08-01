{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Tests.NumEqTest where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.Logic
import MathPrelude.Classes.NumEq
import MathPrelude.Common.PreludeNumConst

import Test.QuickCheck

-- prop_reflex a = a =~ a
-- prop_trans a b c = (a =~ b && b =~ c) ==> (a =~ c) -- not actually true, eg x, x + eps, x+ 2*eps
-- prop_sym a b = (a =~ b) `implies` (b =~ a)
--
-- prop_small a = a < epsDouble && a > P.negate epsDouble ==> nearZero a
