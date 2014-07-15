{-# LANGUAGE RebindableSyntax #-}
module MathPrelude.Extras.NewtonsMethod
  ( newtons
  ) where

import BasicPrelude
import MathPrelude.Structures.Field
import MathPrelude.Extras.Sequence

newton_step f f' x = x - (f x / f' x)

newtons :: Field a => (a->a) -> (a->a) -> a -> a
newtons f f' x0 = converge . iterate (newton_step f f') $ x0
