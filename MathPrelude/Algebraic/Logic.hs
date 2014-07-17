{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Algebraic.Logic
	( P.not, (P.&&), (P.||)
	, L.and, L.or
	, implies, xor
	, ifThenElse
	) where

import BasicPrelude
import qualified Prelude as P
import qualified Data.List as L

-- not = P.not
-- (&&) = (P.&&)
-- (||) = (P.||)
--
-- and = L.and
-- or = L.or

implies :: Bool -> Bool -> Bool
implies True True = True
implies True False = False
implies False True = True
implies False False = True

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

ifThenElse :: Bool -> a -> a -> a
ifThenElse t a b
	| t = a
	| otherwise = b
