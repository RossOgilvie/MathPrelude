{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module MathPrelude.Extras.Evaluable
  ( Evaluable(..)
  , ($$)
  ) where

import BasicPrelude

class Evaluable a b | a -> b where
  eval :: a -> b -> b

($$) :: Evaluable a b => a -> b -> b
($$) = eval

infixr 9 $$

instance Evaluable (a->a) a where
  eval = ($)
