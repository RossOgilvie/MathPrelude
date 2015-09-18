{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Main where

-----------------------------------
--- Imports
-----------------------------------
import MathPrelude
import MathPrelude.SpecialFunctions.EllipticFunctions
import Criterion.Main

ks ∷ [Double]
-- ks = [0.1,0.2..0.9]
ks = [0.5]

-- Our benchmark harness.
main ∷ IO ()
main = defaultMain [
  bgroup "k" $ zipWith bench (map show' ks) (map (nf completeK) ks)
  , bgroup "e" $ zipWith bench (map show' ks) (map (nf completeE) ks)
  ]
