{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module MathPrelude.Constructions.Sphere1 where

import MathPrelude

import MathPrelude.Classes.Field
import MathPrelude.Classes.Transcendental
import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector
import MathPrelude.Constructions.Matrix

-- import GHC.TypeLits

-- import Control.Lens

data S1 = S1 { theta :: Double} deriving Show

toAngle :: S1 → Double
toAngle (S1 t) = t

fromAngle :: Double → S1
fromAngle t = S1 $ t - 2*pi*n
    where
        n = fromInteger . ceiling $ t / (2*pi) - 0.5

pattern Angle t <- (toAngle -> t)
    where Angle t = fromAngle t

toR2 ∷ S1 → Vec 2 Double
toR2 (S1 theta) = V2 (cos theta, sin theta)

fromR2 ∷ Vec 2 Double → S1
fromR2 (V2 (x,y)) = S1 (atan2 y x)

pattern S1inR2 v <- (toR2 -> v)
    where S1inR2 v = fromR2 v

fromC ∷ Complex Double → S1
fromC (x:+y) = S1 (atan2 y x)
--
toC ∷ S1 → Complex Double
toC (S1 theta) = fromArg theta

pattern S1inC z <- (toC -> z)
    where S1inC z = fromC z

toStereo :: S1 → Double
toStereo (S1 t) = 2 * tan (t/2)

fromStereo :: Double → S1
fromStereo y = S1 $ 2 * atan (y/2)

pattern S1inStereo y <- (toStereo -> y)
    where S1inStereo y = fromStereo y

toRot :: S1 → Mat 2 2 Double
toRot (S1 t) = fromListsMt [[cos t, - sin t],[sin t, cos t]]

pattern S1Rot mt <- (toRot -> mt)

spacedPts k = map (*(2*pi/k)) [1..k]
