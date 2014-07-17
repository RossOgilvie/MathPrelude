{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Classes.Derivation where

import BasicPrelude
import MathPrelude.Algebraic.Abelian
-- import qualified Prelude as P

class Derivation r where
  derive :: r -> r

class Integration r where
  integrate :: r -> r

-----------------------------------
--- Instances
-----------------------------------
instance Derivation Integer where derive _ = zero;
instance Derivation Int where derive _ = zero;
instance Derivation Int32 where derive _ = zero;
instance Derivation Int64 where derive _ = zero;
instance Derivation Float where derive _ = zero;
instance Derivation Double where derive _ = zero;
