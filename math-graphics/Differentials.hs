{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE DataKinds #-}
module Differentials
    ( module DD
    , onCut1
    , onCutn1
    , gamma_plus, gamma_minus
    , gamma_1_plus, gamma_1_minus
    , gamma_2_plus, gamma_2_minus
    , mobTrans
    , modulus
    , zinf, zzero
    ) where

import MathPrelude
import MathPrelude.Classes.Module
import MathPrelude.Classes.Norm
import MathPrelude.Constructions.Complex
import MathPrelude.SpecialFunctions.EllipticFunctions

import DD
-----------------------------------
-- Sym Values
-----------------------------------


m (DD a b) = fromReal $ - norm (a-b) - norm (1 - conjugate a * b)

-- n pt@(DD a b) = 2*(conjugate a * aa - bb)/ m pt
--     where
--      aa = (a-b) *. norm (1 - conjugate a * b)
--      bb = (1 - conjugate a * b) *. norm (a-b)
--
-- l (DD a b) = (term1 + term2 + term3)/den
--     where
--      aa = (a-b) *. norm (1 - conjugate a * b)
--      bb = (1 - conjugate a * b) *. norm (a-b)
--      term1 = 2*aa*bb*(1-normsq a)^2*b
--      term2 = (conjugate a * aa - bb)*(a*bb-aa)*(-a*(1+normsq b) - b*(1+normsq a))
--      term3 = -2*a*b*(conjugate a *aa - bb)^2
--      den = (a*bb-aa)^2

modulus (DD a b) = fromReal $ (-d+f)/(d+f)
    where
        d = norm (a-b)
        f = norm (1 - conjugate a * b)

mobTrans (DD a b) z = (z*(bb + conjugate a * aa) -(a*bb+aa))/(z*(conjugate a * aa - bb) + (a*bb-aa))
    where
     aa = (a-b) *. norm (1 - conjugate a * b)
     bb = (1 - conjugate a * b) *. norm (a-b)

zzero (DD α β) = (aa + α*bb)/(conjugate α * aa + bb)
    where
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)

zinf (DD α β) = (aa - α*bb)/(conjugate α * aa - bb)
    where
     aa = (α-β) *. norm (1 - conjugate α * β)
     bb = (1 - conjugate α * β) *. norm (α-β)


sym1t1 pt@(DD α β) = 2*iu*eta1
    where
        eta1 = fromReal $ norm (1-α) * norm (1-β)

symNeg1t1 pt@(DD α β) = 2*iu*negEtaNeg1
    where
        negEtaNeg1 = fromReal $ norm (1+α) * norm (1+β)

-- \int_{γ_+} Θ^2 = 4EF - 4K (( E + η(1)^+/M*(zinf + 1)/(zinf-1)  ))
sym1t2 pt@(DD α β) = firstTerm - 4*completeK k*bracketTerm
    where
     x = mobTrans pt 1
     k = modulus pt
     eta1 = fromReal $ norm (1- α) * norm (1-β)
     firstTerm = 4 * completeE k * ellipticF x k
     bracketTerm = ellipticE x k + eta1/m pt*(zinf pt + 1)/(zinf pt - 1)

-- \int_{γ_-} Θ^2 = 4EF - 4K (( E + (-η(-1)^+)/M*(zinf - 1)/(zinf+1)  ))
symNeg1t2 pt@(DD α β) = firstTerm - 4*completeK k*bracketTerm
    where
     x = mobTrans pt (-1)
     k = modulus pt
     negEtaNeg1 = fromReal $ norm (1+α) * norm (1+β)
     firstTerm = 4 * completeE k * ellipticF x k
     bracketTerm = ellipticE x k + negEtaNeg1/m pt*(zinf pt - 1)/(zinf pt + 1)



-- \int_{γ_+} a Θ^1 + n Θ^2
sym1 a n pt = a .* sym1t1 pt + fromInteger n .* sym1t2 pt
gamma_plus :: Double → Integer → DD → Double
gamma_plus a n pt = realPart $ sym1 a n pt / (2*pi*iu)

-- \int_{γ_-} a Θ^1 + n Θ^2
symNeg1 a n pt = a .* symNeg1t1 pt + fromInteger n .* symNeg1t2 pt
gamma_minus :: Double → Integer → DD → Double
gamma_minus a n pt = realPart $ symNeg1 a n pt / (2*pi*iu)

gamma_1_plus pt = realPart $ sym1t1 pt / (2*pi*iu)
gamma_1_minus pt = realPart $ symNeg1t1 pt / (2*pi*iu)
gamma_2_plus pt = realPart $ sym1t2 pt / (2*pi*iu)
gamma_2_minus pt = realPart $ symNeg1t2 pt / (2*pi*iu)


--
--
-- sym1Cond a n k eps pt = k - eps < gamma_plus' && gamma_plus' < k + eps
--     where gamma_plus' = gamma_plus a n pt
-- symNeg1Cond a n k eps pt = k - eps < gamma_minus' && gamma_minus' < k + eps
--     where gamma_minus' = gamma_minus a n pt
--
--
-- symCond a n g1 gn1 eps pt = sym1Cond a n g1 eps pt && symNeg1Cond a n gn1 eps pt
--

onCut1 pt = isNaN r || isNaN i
    where (r:+i) = mobTrans pt 1

onCutn1 pt = isNaN r || isNaN i
    where (r:+i) = mobTrans pt (-1)
