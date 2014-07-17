{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Tests.RingTest where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Algebraic.Ring
import MathPrelude.Tests.Laws as Laws
import Test.QuickCheck


propAssociative       :: (NumEq a, Ring a) => a -> a -> a -> Bool
propLeftDistributive  :: (NumEq a, Ring a) => a -> a -> a -> Bool
propRightDistributive :: (NumEq a, Ring a) => a -> a -> a -> Bool
propLeftIdentity      :: (NumEq a, Ring a) => a -> Bool
propRightIdentity     :: (NumEq a, Ring a) => a -> Bool

propAssociative       =  Laws.associative (*)
propLeftDistributive  =  Laws.leftDistributive  (*) (+)
propRightDistributive =  Laws.rightDistributive (*) (+)
propLeftIdentity      =  Laws.leftIdentity  (*) one
propRightIdentity     =  Laws.rightIdentity (*) one

propPowerCascade      :: (NumEq a, Ring a) => a -> Int -> Int -> Property
propPowerProduct      :: (NumEq a, Ring a) => a -> Int -> Int -> Property
propPowerDistributive :: (NumEq a, Ring a) => Int -> a -> a -> Property

propPowerCascade      x i j  =  i>=0 && j>=0 ==> Laws.rightCascade (*) (^) x i j
propPowerProduct      x i j  =  i>=0 && j>=0 ==> Laws.homomorphism (x^) (+) (*) i j
propPowerDistributive i x y  =  i>=0 ==> Laws.leftDistributive (^) (*) i x y

{- | Commutativity need not be satisfied by all instances of 'MathPrelude.Algebraic.Ring.Ring'. -}
propCommutative :: (NumEq a, Ring a) => a -> a -> Bool

propCommutative  =  Laws.commutative (*)
