{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}
-- {-# LANGUAGE DataKinds #-}
module LsymmetryTest
    (
    ) where

import           MathPrelude
import           MathPrelude.Classes.Module
import           MathPrelude.Classes.Norm
import           MathPrelude.Constructions.Complex
import           MathPrelude.Constructions.Sphere1
import           MathPrelude.SpecialFunctions.EllipticFunctions
import           MathPrelude.Calculus.Convergence

import           DD
-- import Differentials

ddPts' = ddPts 7
ζpts ∷ [Complex Double]
ζpts = [fromPolar r θ | r <- [0.1,0.2..0.9], θ <- [0,1.0..6.0]]





switchDD (DD α β) = DD β α

diff ∷ Group a ⇒ (DD → a) → DD → a
diff f p = f p - f (switchDD p)









m (DD a b) = fromReal $ - norm (a-b) - norm (1 - conjugate a * b)

n pt@(DD a b) = 2 / m pt
    where
     aa = (a-b) *. norm (1 - conjugate a * b)
     bb = (1 - conjugate a * b) *. norm (a-b)

l (DD a b) = (term1 + term2 + term3)/den
    where
     aa = (a-b) *. norm (1 - conjugate a * b)
     bb = (1 - conjugate a * b) *. norm (a-b)
     term1 = 2*aa*bb*(1-normsq a)^2*b
     term2 = (conjugate a * aa - bb)*(a*bb-aa)*(-a*(1+normsq b) - b*(1+normsq a))
     term3 = -2*a*b*(conjugate a *aa - bb)^2
     den = (a*bb-aa)^2

modulus (DD a b) = fromReal $ (f-d)/(f+d)
    where
        d = norm (a-b)
        f = norm (1 - conjugate a * b)

zzero (DD α β) = (aa + α*bb)/(conjugate α * aa + bb)
    where
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)

zinf :: DD -> Complex Double
zinf (DD α β) = (aa - α*bb)/(conjugate α * aa - bb)
    where
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)


mobTrans :: DD -> Complex Double -> Complex Double
mobTrans (DD a b) ζ = (ζ*(conjugate a * aa + bb) - (aa + a*bb))/(ζ*(conjugate a * aa - bb) - (aa - a*bb))
    where
     aa = (a-b) *. norm (1 - conjugate a * b)
     bb = (1 - conjugate a * b) *. norm (a-b)

-- \int_{γ_+} Θ^2 = 4EF - 4K (( E + η(1)^+/M*(zinf + 1)/(zinf-1)  ))
sym1t2 pt@(DD α β) = firstTerm - 4*completeK k*bracketTerm
    where
     x = mobTrans pt 1
     k = modulus pt
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)
     eta1 = fromReal $ norm (1- α) * norm (1-β)
     firstTerm = 4 * completeE k * ellipticF x k
     bracketTerm = ellipticE x k + eta1/m pt*(zinf pt + 1)/(zinf pt - 1)

-- \int_{γ_-} Θ^2 = 4EF - 4K (( E + (-η(-1)^+)/M*(zinf - 1)/(zinf+1)  ))
symNeg1t2 pt@(DD α β) = firstTerm - 4*completeK k*bracketTerm
    where
     x = mobTrans pt (-1)
     k = modulus pt
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)
     negEtaNeg1 = fromReal $ norm (1+α) * norm (1+β)
     firstTerm = 4 * completeE k * ellipticF x k
     bracketTerm = ellipticE x k + negEtaNeg1/m pt*(zinf pt - 1)/(zinf pt + 1)

altSym1t2 pt@(DD α β) = firstTerm - 4*completeK k*bracketTerm
    where
     k = modulus pt
     x = (\z → 1/z/k) . mobTrans pt $ 1
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)
     eta1 = fromReal $ norm (1- α) * norm (1-β)
     firstTerm = 4 * completeE k * ellipticF x k
     bracketTerm = ellipticE x k + eta1/m pt*(zzero pt + 1)/(zzero pt - 1)



-- testg2p'' = results . map (nearInteger 0.05) . map (diff gamma2plus) . filter (not.onCutn1) $ ddPts'
-- testg2p' = map (=~ 0) . map (diff gamma2plus) . filter (not.onCutn1) $ ddPts'
-- testg2p = results testg2p'
-- testg2m' = map (=~ 0) . map (diff gamma2minus) . filter (not.onCutn1) $ ddPts'
-- testg2m = results testg2m'

results ∷ [Bool] → Text
results bs = let (t,f) = results' bs in "There were " ++ show t ++ " True and " ++ show f ++ " False."
results' ∷ [Bool] → (Integer, Integer)
results' = foldr (\b (t,f) → if b then (t+1,f) else (t,f+1)) (0,0)



nearInteger eps x = d' < eps || d_ < eps
    where
        x' = fromInteger $ ceiling x
        d' = x' - x -- x'>x so this is always pos
        x_ = fromInteger $ floor x
        d_ = x - x_  -- x_<x so this is always pos too







---- test e - Nd(blah)


p (DD α β) ζ = α*β - p1*ζ + p2*ζ^2 - conjugate p1 * ζ^3 + conjugate (α*β) * ζ^4
    where
        p1 =  α*(1+normsq β) + β*(1+normsq α)
        p2 = α*conjugate β + conjugate α *β + (1+normsq α)*(1+normsq β)
p' (DD α β) ζ= - p1 + 2*p2*ζ - 3* conjugate p1 * ζ^2 + 4*conjugate (α*β) * ζ^3
    where
        p1 =  α*(1+normsq β) + β*(1+normsq α)
        p2 = α*conjugate β + conjugate α *β + (1+normsq α)*(1+normsq β)

λ (DD α β) ζ = -α*β + 0.5*p1*ζ + 0.5*conjugate p1*ζ^3 - conjugate (α*β)*ζ^4
    where
        p1 =  α*(1+normsq β) + β*(1+normsq α)
lω pt ζ = l pt * ζ^2

exactζ pt@(DD α β) ζ = 0.5 * p' pt ζ * ζ - p pt ζ

e (DD α β) ζ = top/bottom
    where
        aa = (α-β) *. norm (1 - conjugate α * β)
        bb = (1 - conjugate α * β) *. norm (α-β)
        den = (conjugate α*aa - bb)*ζ - (aa - α*bb)
        top = (-2) * aa * bb * (1-normsq α)^2 * (ζ-β) * (1-conjugate β *ζ) * ζ^2
        bottom = ( norm (α-β) + norm (1-conjugate α *β) ) .* den^2

exactDen pt@(DD α β) ζ = top/bottom
    where
        aa = (α-β) *. norm (1 - conjugate α * β)
        bb = (1 - conjugate α * β) *. norm (α-β)
        β0 = (aa - α*bb)/(conjugate α*aa - bb)
        top = (-2) * (0.5*p' pt ζ*(ζ - β0) - p pt ζ) * ζ^2
        bottom = ( norm (α-β) + norm (1-conjugate α *β) ) .* (ζ - β0)^2

capΩ pt = 2 * l pt / m pt * kk + m pt * ee
    where
        k = modulus pt
        kk = completeK k
        ee = completeE k

capΛ pt = -2 / m pt * kk
    where
        k = modulus pt
        kk = completeK k

theta2 pt ζ = capΩ pt * ζ^2  + capΛ pt * λ pt ζ

theta2' pt ζ = ee*m pt*ζ^2 - 2*kk*(e pt ζ - exactDen pt ζ) - 2/m pt*kk*exactζ pt ζ
    where
        k = modulus pt
        kk = completeK k
        ee = completeE k





mixWith f xs ys = map (uncurry f) (pairs xs ys)
pairs xs ys = [(x, y) | x <- xs, y <- ys]

-- test pt ζ = λ pt ζ - lω pt ζ - exactζ pt ζ - m pt * (e pt ζ - exactDen pt ζ)
test pt ζ = let ee = completeE . modulus $ pt in ee

isZero f = results . map ((<= 1e-5).normsq') $ mixWith f ddPts' ζpts

isSym f = results . map ((<= 1e-5).normsq') $ mixWith (\dd ζ → f dd ζ - f (switchDD dd) ζ) ddPts' ζpts

myPt = DD 0.5 (0:+0.5)





---- TEST LIMITS
αPts = discPts 20
εs ∷ [Double]
εs = map ((1-).(0.5^)) [2..]
βlimits = filter (not . (`elem` [1,-1])) . map toC . s1Pts $ 20

ellipticPart pt = completeE k * ellipticF x k - completeK k * ellipticE x k
    where
     x = mobTrans pt 1
     k = modulus pt

takeβLimit f α β = converge . map f . map (DD α) . map (β*.) $ εs

testLimit f =  results . map ((<= 1e-5).normsq') $ mixWith (takeβLimit f) αPts βlimits

kLimit pt = completeK (modulus pt) - pi/2
eLimit pt = completeE (modulus pt) - pi/2
