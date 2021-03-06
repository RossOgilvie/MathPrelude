{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Tests.RatioTest() where

import MathPrelude
import qualified Prelude as P

-- import MathPrelude.Constructions.Ratio
import MathPrelude.Classes.Rational

-- import MathPrelude.Tests.Laws
import MathPrelude.Tests.RingTest

import Test.QuickCheck


-- -----------------------------------
-- --- Arbitrary
-- -----------------------------------
--
-- instance (Arbitrary a, Approx a, Monoid a) => Arbitrary (Ratio a) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary `suchThat` (/=~ mempty)
--     return (x:%y)
--
-- -----------------------------------
-- --- Generic
-- -----------------------------------
-- qIsRing ∷ Rational → Rational → Rational → Bool
-- qIsRing = isRing
