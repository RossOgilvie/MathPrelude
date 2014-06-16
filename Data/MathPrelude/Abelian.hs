{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.Abelian(Abelian(..), module Data.MathPrelude.OverrideEQ, sum) where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Monoid
import Data.MathPrelude.OverrideEQ

class (NumEq a, Monoid a) => Abelian a where
	zero :: a
	(+) :: a -> a -> a
	negate :: a -> a
	(-) :: a -> a -> a

	zero = mempty
	(+) = (<>)
	negate x = zero - x
	(-) x y = x + negate y
infixl 6  +, -

instance Abelian Int where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Integer where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Int32 where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Int64 where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Float where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Double where zero = mempty; (+) = (P.+); negate = P.negate; (-) = (P.-)
