{-# LANGUAGE RebindableSyntax, UnicodeSyntax #-}
module MathPrelude.Classes.Derivation where

import BasicPrelude
import MathPrelude.Algebraic.Group
-- import qualified Prelude as P

-- | A class representing derivable types.
-- Should replace with Kmett's AD
class Derivation r where
  derive ∷ r → r

-- | A class representing integrable types.
class Integration r where
  integrate ∷ r → r

-----------------------------------
--- Instances
-----------------------------------
instance Derivation Integer where derive _ = zero;
instance Derivation Int where derive _ = zero;
instance Derivation Int32 where derive _ = zero;
instance Derivation Int64 where derive _ = zero;
instance Derivation Float where derive _ = zero;
instance Derivation Double where derive _ = zero;
