{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module MathPrelude.Constructions.Sphere3 where

import MathPrelude

import MathPrelude.Classes.Field
import MathPrelude.Classes.Module
import MathPrelude.Classes.Transcendental
import MathPrelude.Constructions.Complex
import MathPrelude.Constructions.Vector
import MathPrelude.Constructions.Matrix

-- import GHC.TypeLits

import Control.Lens

newtype S3 = S3 (Double,Double,Double,Double) deriving Show

pattern S3inR4 v <- (toR4 -> v)
    where S3inR4 = fromR4

-- isoR4 ∷ Iso' (Vec 4 Double) S3
-- isoR4 = iso (S3 . normalise) r4

toR4 ∷ S3 → Vec 4 Double
toR4 (S3 t) = V4 t

fromR4 ∷ Vec 4 Double → S3
fromR4 (normalise -> V4 t)= S3 t

pattern S3inC2 v <- (toC2 -> v)
    where S3inC2 = fromC2

fromC2 ∷ Vec 2 (Complex Double) → S3
fromC2 (V2 (r1 :+ c1, r2 :+ c2)) = fromR4 . fromListV $ [r1,c1,r2,c2]
--
toC2 ∷ S3 → Vec 2 (Complex Double)
toC2 (S3 (r1,c1,r2,c2)) = V2 (r1 :+ c1, r2 :+ c2)

pattern S3inSC v <- (toSphericalCoord -> v)
    where S3inSC = fromSphericalCoord

fromSphericalCoord ∷ (Double, Double, Double) → S3
fromSphericalCoord (a,b,c) = S3
    (1*cos a
    , 1*sin a*cos b
    , 1*sin a*sin b*cos c
    , 1*sin a*sin b*sin c
    )

toSphericalCoord ∷ S3 → (Double, Double, Double)
toSphericalCoord (S3 (x1,x2,x3,x4)) =
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
      -- | otherwise = acos (x/d)
      | x >= 0 = acos (x/d)
      | otherwise = 2*pi - acos (x/d)

toStereo :: S3 -> Vec 3 Double
toStereo (S3 (x0,x1,x2,x3)) = V3 (x1/d,x2/d,x3/d)
    where d = 1 + x0

fromStereo :: Vec 3 Double -> S3
fromStereo (V3 (x,y,z)) = S3 ((1-d)/(1+d),x/(1+d),y/(1+d),z/(1+d))
    where d = x*x+y*y+z*z

pattern S3Stereo v <- (toStereo -> v)
    where S3Stereo = fromStereo

-- e1 (a,b,c) = fromListV
--   [ (-1)*sin a
--   , 1*cos a*cos b
--   , 1*cos a*sin b*cos c
--   , 1*cos a*sin b*sin c
--   ]
-- e2 (a,b,c) = fromListV
--   [ 0
--   , (-1)*sin b
--   , 1*cos b*cos c
--   , 1*cos b*sin c
--   ]
-- e3 (a,b,c) = fromListV
--   [ 0
--   , 0
--   , (-1)*sin c
--   , 1*cos c
--   ]
--
-- newtype SU2 = SU2 { su2mat ∷ Mat 2 2 (Complex Double) } deriving Show
--
-- fromC2mat (view (from vectorT2) → (z1, z2)) = [z1,z2,- conjugate z2, conjugate z1]
