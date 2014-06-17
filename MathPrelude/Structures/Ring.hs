{-# LANGUAGE NoImplicitPrelude #-}
module MathPrelude.Structures.Ring
	( module MathPrelude.Structures.Abelian
	, Ring(..)
	, (^)
	, product
	, two
	, Num(..)
	, IntDom(..)
	, ifThenElse
	) where

import BasicPrelude
import qualified Prelude as P

import MathPrelude.Structures.Abelian
import MathPrelude.Representations.PreludeNumConst

class Abelian a => Ring a where
	one :: a
	(*) :: a -> a -> a

infixl 7 *



product :: Ring a => [a] -> a
product = foldr (*) one

(^) :: Ring a => a -> Int -> a
(^) x n = product $ take n $ repeat x

two :: Ring a => a
two = one + one




class (Eq a, Show a, Ring a) => Num a  where
    abs, signum :: a -> a




class Ring a => IntDom a











