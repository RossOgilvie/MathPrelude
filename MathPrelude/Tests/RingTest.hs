{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Tests.RingTest where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Classes.Ring
import MathPrelude.Tests.AbelianTest
import MathPrelude.Tests.Laws as Laws
import Test.QuickCheck

isRing ∷ (Show a, Arbitrary a, Ring a) ⇒ a → a → a → Bool
isRing a b c =
  isAbelian a b c
  && Laws.associative (*) a b c
  && Laws.leftDistributive  (*) (+) a b c
  && Laws.rightDistributive (*) (+) a b c
  && Laws.leftIdentity  (*) one a
  && Laws.rightIdentity (*) one a
  && Laws.commutative (*) a b

-- propPowerCascade      :: (NumEq a, Ring a) => a -> Int -> Int -> Property
-- propPowerProduct      :: (NumEq a, Ring a) => a -> Int -> Int -> Property
-- propPowerDistributive :: (NumEq a, Ring a) => Int -> a -> a -> Property
--
-- propPowerCascade      x i j  =  i>=0 && j>=0 ==> Laws.rightCascade (*) (^) x i j
-- propPowerProduct      x i j  =  i>=0 && j>=0 ==> Laws.homomorphism (x^) (+) (*) i j
-- propPowerDistributive i x y  =  i>=0 ==> Laws.leftDistributive (^) (*) i x y
