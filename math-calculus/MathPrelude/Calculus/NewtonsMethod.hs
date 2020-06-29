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
import      MathPrelude.Classes.VectorSpace
import      MathPrelude.Constructions.Vector
import      MathPrelude.Constructions.Matrix
import      MathPrelude.Calculus.Convergence
import      MathPrelude.Calculus.Derivation
import      MathPrelude.Calculus.VectorCalc

newton_step f f' x = x - (f x / f' x)

discardFailure (Left x) = Just x
discardFailure (Right _) = Nothing

-- | Newton's method given a function and its derivative from the given initial point.
newtons ∷ (Field a, Approx a) ⇒ Int → (a→a) → (a→a) → a → Maybe a
newtons maxSteps f f' x0 = discardFailure . eitherConverge (=~) . take maxSteps . iterate (newton_step f f') $ x0

newtonsAD :: (Field a, Approx a) => Int → (a -> Diff a) -> a -> Maybe a
newtonsAD maxSteps f x0 = newtons maxSteps (value . f) (value . derive f) x0

genNewtons :: (KnownNat n, Field a, Approx a) => Int → (Vec n a -> Vec n a) -> (Vec n a -> Mat n n a) -> Vec n a -> Maybe (Vec n a)
genNewtons maxSteps f j v0 = discardFailure . eitherConverge (=~) . take maxSteps . iterate (gen_step f j) $ v0
-- genNewtons f j v0 = take maxSteps . iterate (gen_step f j) $ v0

gen_step f j v = v + solveSystemApprox (j v) (negate (f v))

genNewtonsAD :: (KnownNat n, Field a, Approx a) => Int → (Vec n (Diff a) -> Vec n (Diff a)) -> Vec n a -> Maybe (Vec n a)
genNewtonsAD maxSteps f v0 = genNewtons maxSteps (map value . f . map constant) (map value . jacobian f) v0
