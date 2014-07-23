{-# LANGUAGE RebindableSyntax, UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module MathPrelude.Classes.Evaluable
  ( Evaluable(..)
  , ($$)
  ) where

import BasicPrelude

-- | A class for function like objects of type a that can act on points in type b to produce c's. Types a and b determine c.
class Evaluable a b c | a b → c where
  eval ∷ a → b → c

-- | An operator for eval
($$) ∷ Evaluable a b c ⇒ a → b → c
($$) = eval

infixr 9 $$

instance Evaluable (a→a) a a where
  eval = ($)
