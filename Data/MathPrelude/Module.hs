{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}
module Data.MathPrelude.Module where

import BasicPrelude

import Data.MathPrelude.Ring
import Data.MathPrelude.Abelian
import Data.MathPrelude.Field

-----------------------------------
--- Module
-----------------------------------

class (Abelian m, Ring s) => Module m s where
	scale :: s -> m -> m
	(.*) :: s -> m -> m
	(*.) :: m -> s -> m

	(.*) = scale
	(*.) = flip (.*)
	scale = flip (*.)

-----------------------------------
--- Instances
-----------------------------------

instance Ring r => Module r r where
  scale r s = r*s

-----------------------------------
--- Vector Space
-----------------------------------

-- class (Abelian v, Field k) => VectorSpace v k

-----------------------------------
--- Instances
-----------------------------------

-- instance Module v k => VectorSpace v k

-----------------------------------
--- Methods
-----------------------------------
(./) :: (Field k, Module v k) => k -> v -> v
(./) k v = scale (recip k) v
(/.) :: (Field k, Module v k) => v -> k -> v
(/.) = flip (./)




