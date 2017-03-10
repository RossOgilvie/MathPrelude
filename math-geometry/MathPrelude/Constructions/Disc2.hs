{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module MathPrelude.Constructions.Disc2(
    D2(..)
    , pattern D2inR2, d2ToR2, d2FromR2
    , pattern D2inC, d2ToC, d2FromC
    , d2Pts
    ) where

import MathPrelude

import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector

newtype D2 = D2 (Complex Double) deriving (Show,Read,Eq,Approx)

pattern D2inR2 v <- (d2ToR2 -> v)
    where D2inR2 v = d2FromR2 v

d2ToR2 ∷ D2 → Vec 2 Double
d2ToR2 (D2 (x:+y)) = V2 (x, y)

d2FromR2 ∷ Vec 2 Double → D2
d2FromR2 (V2 (x,y)) = D2 (x:+y)



pattern D2inC z <- (d2ToC -> z)
    where D2inC z = d2FromC z

d2ToC ∷ D2 → Complex Double
d2ToC (D2 z) = z

d2FromC ∷ Complex Double → D2
d2FromC z = D2 z


d2Pts :: Integer → [D2]
d2Pts n = map D2 . map (uncurry (:+)) . filter (\(x,y) → x^2 + y^2 <= r2) $ [(x,y) | x <- pts, y <- pts ]
    where
        r2 = 0.99 -- stay off the edge of the disc
        pts = map (\k → 2*fromInteger k/fromInteger n *(pi/3) - 1) [0..n] -- put a pi/3 here to avoid degenerate situations (pi/3 ~ 1.1)
