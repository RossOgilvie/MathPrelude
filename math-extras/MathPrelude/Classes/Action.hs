{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE UnicodeSyntax          #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | A class for the general notion of one object acting on another. Typically, the acting objects will themselves form a semigroup in a way such that the action (the map from the objects to functions) will become a homomorphism. A typical example is a polynomial acting on the field by evaulation.
module MathPrelude.Classes.Action
  ( Action(..)
  , ($$)
  ) where

import MathPrelude

-- | A class for function like objects of type a that can act on points in type b to produce c's. Types a and b determine c.
class Action a b c | a b → c where
  act ∷ a → b → c

-- | An operator for act
($$) ∷ Action a b c ⇒ a → b → c
($$) = act

infixr 9 $$

instance Action (a→b) a b where
  act = ($)
