{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.NewtonsMethod
  ( newtons
  ) where

import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Common.Convergence

newton_step f f' x = x - (f x / f' x)

newtons :: Field a => (a->a) -> (a->a) -> a -> a
newtons f f' x0 = converge . iterate (newton_step f f') $ x0
