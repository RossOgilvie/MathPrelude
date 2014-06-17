{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Structures.Abelian(Abelian(..), module MathPrelude.Structures.OverrideEQ, sum) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.OverrideEQ

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

sum :: Monoid a => [a] -> a
sum = foldr mappend mempty
