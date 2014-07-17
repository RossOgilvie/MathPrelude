{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module MathPrelude.Classes.Evaluable
  ( Evaluable(..)
  , ($$)
  ) where

import BasicPrelude

class Evaluable a b c | a b -> c where
  eval :: a -> b -> c

($$) :: Evaluable a b c => a -> b -> c
($$) = eval

infixr 9 $$

instance Evaluable (a->a) a a where
  eval = ($)
