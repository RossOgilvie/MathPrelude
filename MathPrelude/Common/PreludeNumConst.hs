module MathPrelude.Common.PreludeNumConst where

import Prelude(fromInteger, fromRational, Int, Integer, Float, Double)
import Data.Int (Int32, Int64)

zeroInt = 0 :: Int
zeroInteger = 0 :: Integer
zeroInt32 = 0 :: Int32
zeroInt64 = 0 :: Int64
zeroFloat = 0 :: Float
zeroDouble = 0 :: Double

oneInt = 1 :: Int
oneInteger = 1 :: Integer
oneInt32 = 1 :: Int32
oneInt64 = 1 :: Int64
oneFloat = 1 :: Float
oneDouble = 1 :: Double

twoInteger = 2 :: Integer

epsFloat = 1e-5 :: Float
epsDouble = 1e-10 :: Double
