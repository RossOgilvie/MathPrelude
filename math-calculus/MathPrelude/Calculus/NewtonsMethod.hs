{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts    #-}
module MathPrelude.Calculus.NewtonsMethod
  ( newtons
  , newtonsAD
  , genNewtons
  , genNewtonsAD
  ) where

import      GHC.TypeLits

import      MathPrelude
import      MathPrelude.Classes.Field
import      MathPrelude.Classes.Module
import      MathPrelude.Constructions.Vector
import      MathPrelude.Constructions.Matrix
import      MathPrelude.Calculus.Convergence
import      MathPrelude.Calculus.Derivation
import      MathPrelude.Calculus.VectorCalc

newton_step f f' x = x - (f x / f' x)

discardFailure (Left x) = Just x
discardFailure (Right _) = Nothing

-- | Newton's method given a function and its derivative from the given initial point.
-- Number of steps before giving up
maxSteps = 100
newtons ∷ Field a ⇒ (a→a) → (a→a) → a → Maybe a
newtons f f' x0 = discardFailure . eitherConverge (=~) . take maxSteps . iterate (newton_step f f') $ x0

newtonsAD :: Field a => (a -> Diff a) -> a -> Maybe a
newtonsAD f x0 = newtons (value . f) (value . derive f) x0

genNewtons :: (KnownNat n, Field a) => (Vec n a -> Vec n a) -> (Vec n a -> Mat n n a) -> Vec n a -> Maybe (Vec n a)
genNewtons f j v0 = discardFailure . eitherConverge (=~) . take maxSteps . iterate (gen_step f j) $ v0
-- genNewtons f j v0 = take maxSteps . iterate (gen_step f j) $ v0
--
gen_step f j v = v + solveSystem (j v) (negate (f v))
--
genNewtonsAD :: (KnownNat n, Field a) => (Vec n (Diff a) -> Vec n (Diff a)) -> Vec n a -> Maybe (Vec n a)
genNewtonsAD f v0 = genNewtons (map value . f . map constant) (map value . jacobian f) v0
