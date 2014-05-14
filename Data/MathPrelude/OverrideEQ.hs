{-# LANGUAGE NoImplicitPrelude #-}
module Data.MathPrelude.OverrideEQ where

import BasicPrelude
import qualified Prelude as P

class NumEq a where
	(===) :: a -> a -> Bool
	(/==) :: a -> a -> Bool

	(===) x y = not $ (/==) x y
	(/==) x y = not $ (===) x y

instance NumEq Int where	(===) = (==)
instance NumEq Int32 where	(===) = (==)
instance NumEq Int64 where	(===) = (==)
instance NumEq Integer where	(===) = (==)