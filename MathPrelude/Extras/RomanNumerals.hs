{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-|
Module      : Roman Numerals
Description : A data type for integers represented as Roman numerals
-}
module MathPrelude.Extras.RomanNumerals
  (  module MathPrelude.Classes.Ring
  , Roman()
  )  where

import           BasicPrelude
import qualified Prelude                             as P

import           MathPrelude.Classes.EuclideanDomain
import           MathPrelude.Classes.Integral
import           MathPrelude.Classes.Ring

newtype Roman = R Integer
  deriving (Enum, Eq, NumEq, Ord, Monoid, Group, Abelian, Ring, CRing, IntDom, EuclideanDomain, Integral)

instance Show Roman where
  show n
    | n == 0 = "Nullus"
    | n < 0 = "Minus " ++ P.show (-n)
    | otherwise = romanize n

romanize n = replicate (fromIntegral md) 'M' ++ digitise (cd*100) ++ digitise (xd*10) ++ digitise xr
  where
    (md, mr) = n `divMod` 1000
    (cd, cr) = mr `divMod` 100
    (xd, xr) = cr `divMod` 10

digitise 0 = ""
digitise 1 = "I"
digitise 2 = "II"
digitise 3 = "III"
digitise 4 = "IV"
digitise 5 = "V"
digitise 6 = "VI"
digitise 7 = "VII"
digitise 8 = "VIII"
digitise 9 = "IX"
digitise 10 = "X"
digitise 20 = "XX"
digitise 30 = "XXX"
digitise 40 = "XL"
digitise 50 = "L"
digitise 60 = "LX"
digitise 70 = "LXX"
digitise 80 = "LXXX"
digitise 90 = "XC"
digitise 100 = "C"
digitise 200 = "CC"
digitise 300 = "CCC"
digitise 400 = "CD"
digitise 500 = "D"
digitise 600 = "DC"
digitise 700 = "DCC"
digitise 800 = "DCCC"
digitise 900 = "CM"
