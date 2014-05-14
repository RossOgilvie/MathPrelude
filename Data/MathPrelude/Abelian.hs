{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.Abelian where

import BasicPrelude
import qualified Prelude as P

import Data.MathPrelude.Monoid

class (Eq a, Monoid a) => Abelian a where
	zero :: a
	(+) :: a -> a -> a
	negate :: a -> a
	(-) :: a -> a -> a

	zero = mempty
	(+) = (<>)
	negate x = zero - x
	(-) x y = x + negate y
infixl 6  +, -

instance Abelian Int where negate = P.negate; (-) = (P.-)
--instance Abelian Int where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Integer where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Int32 where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Int64 where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Float where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
instance Abelian Double where zero = 0; (+) = (P.+); negate = P.negate; (-) = (P.-)
