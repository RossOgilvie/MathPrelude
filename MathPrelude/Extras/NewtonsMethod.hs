{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
module MathPrelude.Extras.NewtonsMethod
  ( newtons
  ) where

import           BasicPrelude
import           MathPrelude.Classes.Field
import           MathPrelude.Extras.Convergence

newton_step f f' x = x - (f x / f' x)

-- | Newton's method given a function and its derivative from the given initial point.
newtons ∷ Field a ⇒ (a→a) → (a→a) → a → a
newtons f f' x0 = converge . iterate (newton_step f f') $ x0
