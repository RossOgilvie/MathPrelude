{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | A derivation is generalisation of taking the derivative of a function. In most cases however it will actually just be the derivative you expect
module MathPrelude.Classes.Derivation where

import           BasicPrelude
import           MathPrelude.Classes.Ring
-- import qualified Prelude as P

-- | A class representing derivable types. A derivation is a map such that the 'constants' are mapped to zero, and Leibniz' rule holds ie d(ab) = da b + a db
class Ring r ⇒ Derivation r where
  derive ∷ r → r

-- | A class representing integrable types.
-- class Integration r where
--   integrate ∷ r → r

-----------------------------------
--- Instances
-----------------------------------
instance Derivation Integer where derive _ = zero;
instance Derivation Int where derive _ = zero;
instance Derivation Int32 where derive _ = zero;
instance Derivation Int64 where derive _ = zero;
instance Derivation Float where derive _ = zero;
instance Derivation Double where derive _ = zero;
