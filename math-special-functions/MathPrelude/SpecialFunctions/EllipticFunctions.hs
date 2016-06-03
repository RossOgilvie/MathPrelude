{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | The many variations of the elliptic functions and their inverses.
module MathPrelude.SpecialFunctions.EllipticFunctions
  (
  module MathPrelude.Constructions.Complex
  -- * Misc
  , agm
  -- * Incomplete Elliptic Integrals
  , ellipticF, fastEllipticF
  , ellipticE, fastEllipticE
  , ellipticPi
  -- * Complete Elliptic Integrals
  , completeK
  , completeE, fastCompleteE
  , completePi
  -- * Carlson Symmetric Elliptic Integrals
  -- $carlson
  , carlsonF
  , carlsonC
  , carlsonJ
  , carlsonD
  -- * Weierstrass Elliptic Functions
  -- $weierstrass
  , wp, wp2
  , wp', wp'2
  , wp_g2, wp_g3
  -- * Jacobi Elliptic Functions
  -- $jef
  , sn, cn, dn
  , ns, nc, nd
  , sc, sd, cs, cd, ds, dc
  -- * Jacobi Theta Functions
  -- $jtf
  , jacobiTheta
  , jacobiTheta00
  , jacobiTheta01
  , jacobiTheta10
  , jacobiTheta11
  ) where

-----------------------------------
--- Imports
-----------------------------------
import MathPrelude
-- import           MathPrelude.Classes.Field
-- import           MathPrelude.Classes.Integral
-- import           MathPrelude.Classes.Transcendental
import           MathPrelude.Constructions.Complex
import           MathPrelude.Calculus.Convergence
-- import MathPrelude.Classes.Module

-----------------------------------
--- Carlson Symmetric Integrals
-----------------------------------
carlSeq ∷ Transcendental a ⇒ a → a → a → a
carlSeq x y z = (\(a,_,_) → a) . converge' test . iterate step $ start
  where
    start = (x,y,z)
    test (!a, !b, !c) _ = (a =~ b) && (b=~ c) && (a=~c)
    step (!a, !b, !c) = (a',b',c')
      where
        a' = (a+l)/4
        b' = (b+l)/4
        c' = (c+l)/4
        l = sqrt (a*b) + sqrt (b*c) + sqrt (a*c)

-- $carlson
-- The Carlson symmetric forms of elliptic integrals are a modern alternative to the Legendre forms (the usual elliptic integrals). Their good duplication theorem make them easy to calculate.
-- <https://en.wikipedia.org/wiki/Carlson_symmetric_form Wikipedia>

carlsonF x y z = (\m → recip . sqrt $ m) $ carlSeq x y z
carlsonC x y = carlsonF x y y
carlsonD x y z = carlsonJ x y z z
carlsonJ x y z p
  | x =~ y && x =~ z && x =~ p = x ** (-3/2)
  | otherwise = (1/4) * carlsonJ x' y' z' p' + 6*carlsonC d2 s
    where
      l = sqrt (x*y) + sqrt (y*z) + sqrt (x*z)
      x' = (x + l)/4
      y' = (y + l)/4
      z' = (z + l)/4
      p' = (p + l)/4
      d = (sqrt p + sqrt x)*(sqrt p + sqrt y)*(sqrt p + sqrt z)
      d2 = d^2
      s = d2 + (p-x)*(p-y)*(p-z)

-----------------------------------
--- Incomplete Integrals
-----------------------------------

-- | The incomplete elliptic integral of the first kind. The first argument is the Jacobi argument x = sin φ and the second is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_first_kind Wikipedia>
ellipticF ∷ Transcendental a ⇒ a → a → a
ellipticF x k = x * carlsonF (1-x^2) (1-k^2*x^2) 1

fastEllipticF ∷ Complex Double → Complex Double → Complex Double
fastEllipticF x k
    | imagPart x < 0 = - fastEllipticF (-x) k
    | xNear0 = x*(1+ (1+k^2)/6*x^2)
    | not xNear0 && kNear0 && (not xNearInf || kxNear0) = asin x * (1+k^2)
    | kxNearInf = iu*completeK k' - fastEllipticF (1/k/x) k
    | kNear0 && xNearInf && kxMid= completeK k + iu*completeK k' + iu*log ((1-sqrtxk)/xk) + 1/4*iu*(log ((1-sqrtxk)/xk) - sqrtxk/xk^2)*k^2
    | otherwise = ellipticF x k
    where
        k' = sqrt (1-k^2)
        thres = 150
        thres2 = thres^2
        kNear1 = normsq' (1-k) < thres2
        kNear0 = normsq' k < 1/thres2
        xNear0 = normsq' x < 1/thres2
        xNearInf = normsq' x > thres2
        kxNear0 = normsq' (x*k) < 1/thres2
        kxNearInf = normsq' (x*k) > thres2
        kxMid = not kxNear0 && not kxNearInf
        xk = x*k
        sqrtxk = sqrt (1-xk^2)

-- | The incomplete elliptic integral of the second kind. The first argument is the Jacobi argument x = sin φ and the second is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_second_kind Wikipedia>
ellipticE ∷ Transcendental a ⇒ a → a → a
ellipticE x k = x * carlsonF (1-x^2) (1-k^2*x^2) 1 - (1/3)* k^2 *x^3 * carlsonD (1-x^2) (1-k^2*x^2) 1

fastEllipticE ∷ Complex Double → Complex Double → Complex Double
fastEllipticE x k
    | imagPart x < 0 = - fastEllipticE (-x) k
    | kNear1 && xNear0 = x*(1+ (1-k)/3*x^2)
    | not kNear1 && xNear0 = x*(1+ (1-k^2)/6*x^2)
    | kNear1 && not xNearInf = x + (1-k)*(-x + 0.5*log ((1+x)/(1-x)))
    | kNear0 && kxNear0 = asin x + 0.5*(asin x - x*sqrt (1-x^2))*k^2
    | xNearInf && kxNearInf = k*x + iu*(completeK k' - fastCompleteE k') + (1-k^2)/2/k/x
    | otherwise = ellipticE x k
    where
        k' = sqrt (1-k^2)
        thres = 400
        thres2 = thres^2
        kNear1 = normsq' (1-k) < thres2
        kNear0 = normsq' k < 1/thres2
        xNear0 = normsq' x < 1/thres2
        xNearInf = normsq' x > thres2
        kxNear0 = normsq' (x*k) < 1/thres2
        kxNearInf = normsq' (x*k) > thres2
        xk = x*k
        sqrtxk = sqrt (1-xk^2)


-- | The incomplete elliptic integral of the third kind. The first argument is the Jacobi argument x = sin φ, the second is the characteristic n and the third is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_third_kind Wikipedia>
ellipticPi ∷ Transcendental a ⇒ a → a → a → a
ellipticPi x n k = x * carlsonF (1-x^2) (1-k^2*x^2) 1 - (1/3)*n*x^3 * carlsonJ (1-x^2) (1-k^2*x^2) 1 (1-n*x^2)

-----------------------------------
--- Complete Integrals
-----------------------------------
-- | The arithmetic geometric mean.
-- <https://en.wikipedia.org/wiki/Arithmetic%E2%80%93geometric_mean Wikipedia>
agm ∷ Transcendental a ⇒ a → a → a
agm a g = fst . head . dropWhile test . iterate next $ (a,g)
  where
    next (!a,!g) = ((a+g)/ 2, sqrt (a*g))
    test (!a,!g) = not $ a =~ g

-- | The complete elliptic integral of the first kind. The argument is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_first_kind Wikipedia>
completeK ∷ Transcendental a ⇒ a → a
completeK k = (pi/2)/ agm (1-k) (1+k)

-- | The complete elliptic integral of the second kind. The argument is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_second_kind Wikipedia>
completeE ∷ Transcendental a ⇒ a → a
completeE k = carlsonF 0 (1-k^2) 1 - (1/3)* k^2* carlsonD 0 (1-k^2) 1
-- completeE = fastCompleteE

completeEnearK0 k = pi/2*(
    1
    - 1/4*k^2
    - 3/64*k^4
    -5/256*k^6
    - 175/16384*k^8
    -441/65536*k^10
    - 4851/1048576*k^12
    -14157 * k^14 /4194304
    -2760615 * k^16 /1073741824
    -8690825 * k^18 /4294967296
    -112285459* k^20 /68719476736)

completeEnearK1 k =
    1
    + 1/4*(log (1-m) - 4*log 2 + 1)*(m-1)
    + 1/64*(-6*log(1-m) + 24 * log 2 - 13) * (m-1)^2
    + 3/256*(5*log(1-m) - 20 * log 2 + 12) * (m-1)^3
    + 5/49152*(-420*log(1-m) + 1680 * log 2 - 1051) * (m-1)^4
    + 7/131072*(630*log(1-m) - 2520 * log 2 + 1613) * (m-1)^5
    +21/2621440*(-3465*log(1-m) + 13860* log 2 - 9001)*(m-1)^6
    +11/41943040*(90090*log(1-m) - 360360* log 2 + 236381)*(m-1)^7
    +429/7516192768*(-360360*log(1-m) + 1441440* log 2 - 952487)*(m-1)^8
    -10725/120259084288*(-204204*log(1-m) + 816816* log 2 - 542779)*(m-1)^9
    +2431/8658654068736*(-58198140*log(1-m) + 232792560*log 2 - 155378701)*(m-1)^10
    where m = k^2

fastCompleteE ∷ Complex Double → Complex Double
fastCompleteE k
    | realPart k <= 0.5 = completeEnearK0 k
    | realPart k <= 0.9 = completeE k
    | otherwise = completeEnearK1 k


-- | The complete elliptic integral of the third kind. The first argument is the characteristic n. The second argument is the modulus k.
-- <https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_third_kind Wikipedia>
completePi ∷ Transcendental a ⇒ a → a → a
completePi n k = carlsonF 0 (1-k^2) 1 - (1/3)*n* carlsonJ 0 (1-k^2) 1 (1-n)

------------------------------------
-- Jacobi Theta
------------------------------------
-- $jtf
-- The Jacobi theta functions. They are quasi-periodic in the first variable z and the second argument is the conformal factor τ (in the upper half plane). The numbered theta functions use Riemann-Mumford notation.
-- <https://en.wikipedia.org/wiki/Theta_function Wikipedia>

jacobiTheta :: (Ord a, Transcendental a) => Complex a -> Complex a -> Complex a
jacobiTheta z tau = 1 + 2* series' term 1
  where
    term n = q^(n''^2) * cos (2*pi*n'*z)
      where
          n' = fromInteger n
          n'' = fromIntegral n
    q = exp (pi*iu*tau)

jacobiTheta00, jacobiTheta01, jacobiTheta10, jacobiTheta11 :: (Ord a, Transcendental a) ⇒ Complex a -> Complex a -> Complex a
jacobiTheta00 = jacobiTheta
jacobiTheta01 z = jacobiTheta (z + (1/2))
jacobiTheta10 z tau = exp (pi*iu*tau/4 + pi*iu*z) * jacobiTheta (z + (tau/2)) tau
jacobiTheta11 z tau = exp (pi*iu*tau/4 + pi*iu*(z + (1/2))) * jacobiTheta (z + (1/2) + (tau/2)) tau

------------------------------------
-- Jacobi Elliptic
------------------------------------
-- $jef
-- The twelve Jacobi elliptic functions. The first argument is the variable u (the value of the elliptic integral) and the second is the elliptic modulus k. The functions are named in the form pq, with p and q choosen from s c n d. These letters represent the points 0, K, iK' and K + iK' respectively, where K and K' are the quarter periods.  pq has a zero at p and a pole at q.
-- <https://en.wikipedia.org/wiki/Jacobi%27s_elliptic_functions Wikipedia>

sn ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
sn u k = - (t00* jacobiTheta11 z tau) / (t10 * jacobiTheta01 z tau)
  where
    t00 = jacobiTheta00 0 tau
    t10 = jacobiTheta10 0 tau
    k' = sqrt (1-k^2)
    k'' = sqrt k'
    l = (1-k'')/(1+k'')/2
    q = l + 2*l^5 + 15*l^9 + 150*l^13 + 1707*l^17 + 20910*l^21 + 268616*l^25
    tau = log q / (iu*pi)
    z = u/pi/(t00^2)

cn ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
cn u k = (t01* jacobiTheta10 z tau) / (t10 * jacobiTheta01 z tau)
  where
    t00 = jacobiTheta00 0 tau
    t01 = jacobiTheta01 0 tau
    t10 = jacobiTheta10 0 tau
    k' = sqrt (1-k^2)
    k'' = sqrt k'
    l = (1-k'')/(1+k'')/2
    q = l + 2*l^5 + 15*l^9 + 150*l^13 + 1707*l^17 + 20910*l^21 + 268616*l^25
    tau = log q / (iu*pi)
    z = u/pi/(t00^2)

dn ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
dn u k = (t01* jacobiTheta00 z tau) / (t00 * jacobiTheta01 z tau)
  where
    t00 = jacobiTheta00 0 tau
    t01 = jacobiTheta01 0 tau
    k' = sqrt (1-k^2)
    k'' = sqrt k'
    l = (1-k'')/(1+k'')/2
    q = l + 2*l^5 + 15*l^9 + 150*l^13 + 1707*l^17 + 20910*l^21 + 268616*l^25
    tau = log q / (iu*pi)
    z = u/pi/(t00^2)

ns ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
ns = 1 / sn
nc ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
nc = 1 / cn
nd ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
nd = 1 / dn

sc ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
sc = sn / cn
sd ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
sd = sn / dn
ds ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
ds = dn / sn
dc ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
dc = dn / cn
cs ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
cs = cn / sn
cd ∷ (Ord a, Transcendental a) ⇒ Complex a → Complex a → Complex a
cd = cn / dn

------------------------------------
-- Weierstrass Elliptic
------------------------------------
-- $weierstrass
-- The Weierstrass elliptic function, its derivative and its invariants. Most functions take a complex number z and a conformal paramter τ (in the upper half plane. The primed wp' refers to derivative. The "2" versions of a function refer to it taking two lattice points ω_1 and ω_2, not τ. The invariants are functions of τ alone.
-- <https://en.wikipedia.org/wiki/Weierstrass%27s_elliptic_functions Wikipedia>

wp, wp' :: (Ord a, Transcendental a) ⇒ Complex a -> Complex a -> Complex a
wp2, wp'2 :: (Ord a, Transcendental a) ⇒ Complex a -> Complex a -> Complex a -> Complex a

wp z tau = (pi*t00*t10*jacobiTheta01 z tau / jacobiTheta11 z tau)^2 - pi^2/3*(t00^4+t10^4)
  where
    t00 = jacobiTheta00 0 tau
    t10 = jacobiTheta10 0 tau

wp2 z w1 w2 = wp (z/w1) (w2/w1) / w1^2

wp' z tau = sqrt (4*p^3 - g2*p - g3)
  where
    p = wp z tau
    g2 = wp_g2 tau
    g3 = wp_g3 tau

wp'2 z w1 w2 = 1/w1^3 * wp' (z/w1) (w2/w1)

wp_g2, wp_g3 :: (Ord a, Transcendental a) ⇒ Complex a -> Complex a
wp_g2 tau = pi^4/6*(a^8 + b^8 + c^8)
  where
    b = jacobiTheta00 0 tau
    c = jacobiTheta01 0 tau
    a = jacobiTheta10 0 tau

wp_g3 tau = pi^6/216*(a^12 - 33 *a^8*b^4 - 33*a^4 *b^8 + b^12)
  where
    b = jacobiTheta00 0 tau
    a = jacobiTheta10 0 tau
