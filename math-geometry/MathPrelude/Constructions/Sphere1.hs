{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module MathPrelude.Constructions.Sphere1(
    S1(..)
    , pattern Angle, s1ToAngle, s1FromAngle
    , pattern S1inR2, s1ToR2, s1FromR2
    , pattern S1inC, s1ToC, s1FromC
    , pattern S1inStereo, s1ToStereo, s1FromStereo
    , pattern S1inMat, s1inMat
    , s1Pts
    ) where

import MathPrelude

import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector
import MathPrelude.Constructions.Matrix

data S1 = S1 { theta :: Double} deriving Show


pattern Angle θ <- (s1ToAngle -> θ)
    where Angle θ = s1FromAngle θ

s1ToAngle :: S1 → Double
s1ToAngle (S1 θ) = θ

s1FromAngle :: Double → S1
s1FromAngle θ = S1 $ θ - 2*pi*n
    where
        n = fromInteger . ceiling $ θ / (2*pi) - 0.5



pattern S1inR2 v <- (s1ToR2 -> v)
    where S1inR2 v = s1FromR2 v

s1ToR2 ∷ S1 → Vec 2 Double
s1ToR2 (S1 θ) = V2 (cos θ, sin θ)

s1FromR2 ∷ Vec 2 Double → S1
s1FromR2 (V2 (x,y)) = S1 (atan2 y x)



pattern S1inC z <- (s1ToC -> z)
    where S1inC z = s1FromC z

s1ToC ∷ S1 → Complex Double
s1ToC (S1 θ) = fromArg θ

s1FromC ∷ Complex Double → S1
s1FromC (x:+y) = S1 (atan2 y x)



pattern S1inStereo y <- (s1ToStereo -> y)
    where S1inStereo y = s1FromStereo y

s1ToStereo :: S1 → Double
s1ToStereo (S1 t) = 2 * tan (t/2)

s1FromStereo :: Double → S1
s1FromStereo y = S1 $ 2 * atan (y/2)



pattern S1inMat mt <- (s1inMat -> mt)

s1inMat :: S1 → Mat 2 2 Double
s1inMat (S1 t) = fromListsMt [[cos t, - sin t],[sin t, cos t]]


s1Pts ∷ Integer → [S1]
s1Pts k' = map Angle pts
    where
        -- k = 50
        k = fromInteger k'
        pts = map (*(2*pi/k)) [1..k]
