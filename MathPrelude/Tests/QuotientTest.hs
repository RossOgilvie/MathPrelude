{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module MathPrelude.Tests.QuotientTest() where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Constructions.Quotient

import MathPrelude.Common.Floating
import MathPrelude.Common.Rational

import Test.QuickCheck


-----------------------------------
--- QuickCheck
-----------------------------------

instance (Arbitrary a, NumEq a, Monoid a) => Arbitrary (Quotient a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary `suchThat` (/=~ mempty)
    return (x:%y)
