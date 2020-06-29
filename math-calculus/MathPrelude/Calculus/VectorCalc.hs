{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | Compute the derivative of multivariable functions and vector valued functions
module MathPrelude.Calculus.VectorCalc
    ( partial
    , gradient
    , jacobian
    ) where

import GHC.TypeLits
import Data.Proxy

import      MathPrelude
import      MathPrelude.Calculus.Derivation
import      MathPrelude.Constructions.Vector
import      MathPrelude.Constructions.Matrix

component :: KnownNat n => Integer -> Vec n a -> a
component k v
    | k < 0 || k >= dimV v = error ("Error MathPrelude.Calculus.VectorCalc.component: Index outside range. Dim v = " ++ show' (dimV v) ++ " index = " ++ show' k)
    -- TODO: int type mismatch here
    | otherwise = head . drop (fromInteger k) . toListV $ v

slice :: (KnownNat n, Ring a) => Integer -> Vec n a -> Vec n (Diff a)
slice k v = fromListV (pre ++ [variable x] ++ post)
    where
        v' = toListV v
        -- TODO: int type mismatch here
        pre = map constant . take (fromInteger k) $ v'
        x = component k v
        -- TODO: int type mismatch here
        post = map constant . drop (fromInteger $ k+1) $ v'

partial :: (KnownNat n, Ring a, Ring b) => (Vec n (Diff a) -> Diff b) -> Integer -> Vec n a -> Diff b
partial f k = derive (f . slice k)

gradient :: (KnownNat n, Ring a, Ring b) => (Vec n (Diff a) -> Diff b) -> Vec n a -> Vec n (Diff b)
gradient f v = fromListV . map (\k -> partial f k v) $ [0..(n'-1)]
    where
        n' = dimV v

jacobian :: forall n m a b. (KnownNat n, KnownNat m, Ring a, Ring b) => (Vec n (Diff a) -> Vec m (Diff b)) -> Vec n a -> Mat m n (Diff b)
jacobian f v = fromRowsMt partials
    where
        m' = fromInteger $ natVal (Proxy :: Proxy m)
        components = map (\j -> component j . f) [0..(m'-1)]
        partials = map (($v).gradient) components
