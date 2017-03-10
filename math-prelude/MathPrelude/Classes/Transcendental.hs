{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
-- | A module to do transcendental operations, such as exponentials and trigonometry.
module MathPrelude.Classes.Transcendental where

------------------------------
--- Imports
------------------------------
import           BasicPrelude
import qualified Prelude                   as P

import           MathPrelude.Classes.NumEq
import           MathPrelude.Classes.Field

------------------------------
--- Classes
------------------------------
-- | A class containg the simply transcendental functions, also known as the elementary transcendental functions.
class (Field a, NumEq a) ⇒ Transcendental a where
    -- | Everyone's favourite constant.
    pi                  ∷ a
    -- | The core functions. No specification about branch cuts.
    exp, log, sqrt      ∷ a → a
    -- | The generalised exponetiation operation (c.f. '^').
    (**)                           ∷ a → a → a
    -- | The logarithm in the specified base. The base is the first argument.
    logBase                   ∷ a → a → a
    -- | Standard trigonometry functions.
    sin, cos, tan       ∷ a → a
    -- | Inverse trigonometry functions, also known as sin^-1 or arcsin, etc.
    asin, acos, atan    ∷ a → a
    -- | atan x y = atan (y/x), but well defined when x is zero.
    atan2                                ∷ a → a → a
    -- | Hyperbolic trig functions.
    sinh, cosh, tanh    ∷ a → a
    -- | Inverse hyperbolic trig functions.
    asinh, acosh, atanh ∷ a → a

    sqrt x = x ** recip 2
    x ** y = exp ( y * log x )
    logBase b x = log x / log b
    tan x = sin x / cos x
    sinh x = (exp x - exp (negate x)) / 2
    cosh x = (exp x + exp (negate x)) / 2
    tanh x = sinh x / cosh x
    asinh x = log (x + sqrt(x*x  + 1))
    acosh x = log (x + sqrt(x*x  - 1))
    atanh x = log ((1 + x)/(1 - x)) / 2

    {-# MINIMAL pi, exp, log, sin, cos, asin, acos, atan, atan2 #-}

------------------------------
--- Extra trig stuff
------------------------------
-- | Reciprocal trig functions
cosec, sec, cot ∷ Transcendental a ⇒ a → a
cosec = 1/sin
sec = 1/cos
cot = 1/tan



------------------------------
--- Instances
------------------------------
-- | Transcendental instance for Float
instance Transcendental Float where
    pi = P.pi
    exp = P.exp
    sqrt = P.sqrt
    log = P.log
    (**) = (P.**)
    logBase = P.logBase
    sin = P.sin
    cos = P.cos
    tan = P.tan
    asin = P.asin
    acos = P.acos
    atan = P.atan
    atan2 = P.atan2
    sinh = P.sinh
    cosh = P.cosh
    tanh = P.tanh
    asinh = P.asinh
    acosh = P.acosh
    atanh = P.atanh

-- | Transcendental instance for Double
instance Transcendental Double where
    pi = P.pi
    exp = P.exp
    sqrt = P.sqrt
    log = P.log
    (**) = (P.**)
    logBase = P.logBase
    sin = P.sin
    cos = P.cos
    tan = P.tan
    asin = P.asin
    acos = P.acos
    atan = P.atan
    atan2 = P.atan2
    sinh = P.sinh
    cosh = P.cosh
    tanh = P.tanh
    asinh = P.asinh
    acosh = P.acosh
    atanh = P.atanh
