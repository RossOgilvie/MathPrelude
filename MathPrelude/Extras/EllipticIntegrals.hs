{-# LANGUAGE RebindableSyntax, BangPatterns #-}
module MathPrelude.Extras.EllipticIntegrals
  (
  -- module MathPrelude.Constructions.Complex
  agm
  , ellipticK
  , ellipticE
  ) where

-----------------------------------
--- Imports
-----------------------------------
import BasicPrelude
import MathPrelude.Algebraic.Field
import MathPrelude.Common.Transcendental
-- import MathPrelude.Constructions.Complex
-- import MathPrelude.Algebraic.Module

-----------------------------------
--- Methods
-----------------------------------
-- arithmetic geometric mean
agm :: Transcendental a => a -> a -> a
agm a g = fst . head . dropWhile test . iterate next $ (a,g)
  where
    next (!a,!g) = ((a+g)/ 2, sqrt (a*g))
    test (!a,!g) = not $ a =~ g

ellipticK :: Transcendental a => a -> a
ellipticK k = (pi/2)/ agm (1-k) (1+k)

-- adlaj's modified agm for elliptic integrals of the 2nd kind
magm :: Transcendental a => a -> a -> a
magm x y = (\(x,_,_) -> x) .head . dropWhile test . iterate next $ (x,y,zero)
  where
    root (x,y,z) = sqrt ((x-z)*(y-z))
    nextx (x,y,_) = (x+y)/ 2
    nexty p@(_,_,z) = z + root p
    nextz p@(_,_,z) = z - root p
    next p = (nextx p, nexty p, nextz p)
    test (x,y,_) = not $ x =~ y

ellipticE :: Transcendental a => a -> a
ellipticE k = (pi/ 2) * (magm 1 k') / (agm 1 k')
  where k' = sqrt $ 1 - k^2

------------------------------------
-- Misc
------------------------------------
-- aperiod :: Double -> Complex Double
-- -- aperiod :: (Ord a, Field a, Transcendental a) => a -> Complex a Double -> Complex Double
-- aperiod r = (-4::Double) .* iu * (fromReal $ ellipticK (r ^ 2))
-- --bperiod :: (Ord a, Field a, Transcendental a) => a -> Complex a
-- bperiod :: Double -> Complex Double
-- bperiod r = ((-8)/(1+r)/(1+r)) .* (fromReal $ ellipticK ktilde)
--   where ktilde = (1-r)*(1-r)/(1+r)/(1+r)
--
-- tau r = (aperiod r)/(bperiod r)