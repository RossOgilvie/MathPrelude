{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Tests.RatioTest() where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Constructions.Quotient

import MathPrelude.Constructions.Ratio
import MathPrelude.Common.Rational

import Test.QuickCheck


-----------------------------------
--- QuickCheck
-----------------------------------

instance (Arbitrary a, NumEq a, Monoid a) => Arbitrary (Ratio a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary `suchThat` (/=~ mempty)
    return (x:%y)
