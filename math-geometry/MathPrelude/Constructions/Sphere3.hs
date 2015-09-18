{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module MathPrelude.Constructions.Sphere3 (
    S3(..)
    , pattern S3inR4, s3ToR4, s3FromR4
    , pattern S3inC2, s3ToC2, s3FromC2
    , pattern S3inSC, s3ToSphericalCoord, s3FromSphericalCoord
    , pattern S3inStereo, s3ToStereo, s3FromStereo
    ) where

import MathPrelude

import MathPrelude.Classes.Module
import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector

newtype S3 = S3 (Double,Double,Double,Double) deriving Show

pattern S3inR4 v <- (s3ToR4 -> v)
    where S3inR4 = s3FromR4

s3ToR4 ∷ S3 → Vec 4 Double
s3ToR4 (S3 t) = V4 t

s3FromR4 ∷ Vec 4 Double → S3
s3FromR4 (normalise -> V4 t)= S3 t



pattern S3inC2 v <- (s3ToC2 -> v)
    where S3inC2 = s3FromC2

s3ToC2 ∷ S3 → Vec 2 (Complex Double)
s3ToC2 (S3 (r1,c1,r2,c2)) = V2 (r1 :+ c1, r2 :+ c2)

s3FromC2 ∷ Vec 2 (Complex Double) → S3
s3FromC2 (V2 (r1 :+ c1, r2 :+ c2)) = s3FromR4 . fromListV $ [r1,c1,r2,c2]



pattern S3inSC v <- (s3ToSphericalCoord -> v)
    where S3inSC = s3FromSphericalCoord

s3ToSphericalCoord ∷ S3 → (Double, Double, Double)
s3ToSphericalCoord (S3 (x1,x2,x3,x4)) =
  ( t12 (sqrt (x1*x1+x2*x2+x3*x3+x4*x4)) x1
  , t12 (sqrt (x2*x2+x3*x3+x4*x4)) x2
  , t3 (sqrt (x3*x3+x4*x4)) x3
  )
  where
    t12 d x
      | d == 0 = 0
      | otherwise = acos (x/d)
    t3 d x
      | d == 0 = 0
      | x >= 0 = acos (x/d)
      | otherwise = 2*pi - acos (x/d)

s3FromSphericalCoord ∷ (Double, Double, Double) → S3
s3FromSphericalCoord (a,b,c) = S3
    (1*cos a
    , 1*sin a*cos b
    , 1*sin a*sin b*cos c
    , 1*sin a*sin b*sin c
    )



pattern S3inStereo v <- (s3ToStereo -> v)
    where S3inStereo = s3FromStereo

s3ToStereo :: S3 -> Vec 3 Double
s3ToStereo (S3 (x0,x1,x2,x3)) = V3 (x1/d,x2/d,x3/d)
    where d = 1 + x0

s3FromStereo :: Vec 3 Double -> S3
s3FromStereo (V3 (x,y,z)) = S3 ((1-d)/(1+d),x/(1+d),y/(1+d),z/(1+d))
    where d = x*x+y*y+z*z


-- e1 (a,b,c) = s3FromListV
--   [ (-1)*sin a
--   , 1*cos a*cos b
--   , 1*cos a*sin b*cos c
--   , 1*cos a*sin b*sin c
--   ]
-- e2 (a,b,c) = s3FromListV
--   [ 0
--   , (-1)*sin b
--   , 1*cos b*cos c
--   , 1*cos b*sin c
--   ]
-- e3 (a,b,c) = s3FromListV
--   [ 0
--   , 0
--   , (-1)*sin c
--   , 1*cos c
--   ]
--
-- newtype SU2 = SU2 { su2mat ∷ Mat 2 2 (Complex Double) } deriving Show
--
-- s3FromC2mat (view (from vectorT2) → (z1, z2)) = [z1,z2,- conjugate z2, conjugate z1]
