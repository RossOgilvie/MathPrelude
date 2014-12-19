{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE UnicodeSyntax          #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module MathPrelude.Classes.Action
  ( Action(..)
  , ($$)
  ) where

import           BasicPrelude

-- | A class for function like objects of type a that can act on points in type b to produce c's. Types a and b determine c.
class Action a b c | a b → c where
  act ∷ a → b → c

-- | An operator for act
($$) ∷ Action a b c ⇒ a → b → c
($$) = act

infixr 9 $$

instance Action (a→b) a b where
  act = ($)
