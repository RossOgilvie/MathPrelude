{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE UnicodeSyntax     #-}
-- {-# LANGUAGE DataKinds #-}
module ModuliSpace2
    ( main
    , save
    ) where

import           MathPrelude
import           MathPrelude.Classes.Module
import           MathPrelude.Classes.Norm
import           MathPrelude.Constructions.Complex
import           MathPrelude.Constructions.Sphere1

import           Differentials
-- import DD
import           RationalBoundary                  hiding (main)
-- import RationalBoundary2 hiding (main)

import qualified Filesystem                        as F
import qualified Filesystem.Path.CurrentOS         as FP
import           Graphics.EasyPlot

-- import qualified Data.Set as Set


main = mainSortAndShow
-- main = save

mainSortAndShow = do
    pts <- goodAnnoP
    let piecesP = map (filter inBigishBallP . flip findPieceP pts) qs
    let piecesDD = map (flip findPieceDD pts) qs
    let piecesK = map (filter inBigishBallP) (findPiecesK piecesP)
    -- mainGraphs p2Trip (pDiags : piecesP)
    -- mainGraphs p2Trip ([pDiags] ++ piecesP ++ [pDelauney])
    -- mainGraphs projbx ([map p2DD pDiags] ++ piecesDD ++ [cutPtsDD])
    -- let expandedDD = map expand piecesDD
    -- mainGraphs projbx ([expand $ map p2DD pDiags] ++ expandedDD)
    mainGraphs p2Trip (pConformal : pDiags : piecesK)






-- known good paramters
-- m,n,mt,nt,k,kt :: Double
-- (m,n,mt,nt,k,kt) = (2,-1,0,1,0,-2)
-- symcond' = symcond m n mt nt k kt

stdOptions :: [Option3D Double Double Double]
stdOptions = [RangeX (-1) 1, RangeY (-1) 1]

p ∷ Double
-- p = 1
p = 4/3
-- p = 5/2

qs ∷ [Double]
qs = [-2,-5/3,-4/3,4/3,5/3,6/3]
-- qs = [-1,1,5/3,7/3,9/3]
-- qs = [2,3]

-- format where p=n/m: 'n-m-lots-'

-- few
-- linearSteps = 60
-- quadraticSteps = 20
-- version = "1-1-few-"
-- version = "4-3-few-"

--some
linearSteps = 60
quadraticSteps = 40
-- version = "1-1-some-"
version = "4-3-some-"


-- lots
-- linearSteps = 100
-- quadraticSteps = 60
-- version = "4-3-lots-"
-- version = "1-1-lots-"
-- version = "5-2-some-"

-- P is a parameterisation of the subspace of D^2 where p is constant
-- the format is φ, s, t, where tan (φ/2) is ratio of the αs, s is the signed arc length of α. ditto t and β.
data P = P !Double !Double !Double
    deriving (Show, Read, Eq)

p2DD ∷ P → DD
p2DD (P φ s t) = DD (p2DD' φ s) (p2DD' (2 * atan (p/tan (φ/2))) t)

p2DD' ∷ Double → Double → Complex Double
p2DD' φ s
    | φ =~ (pi/2) = 0 :+ s
    | φ < (pi/2) = (x0 - δx) :+ y
    | φ > (pi/2) = (x0 + δx) :+ y
    where
        r = abs $ tan φ
        f = s / r
        x0 = 1 / cos φ
        δx = r*cos f
        y = r*sin f

p2Trip ∷ P → (Double, Double, Double)
p2Trip (P φ s t) = (φ,s,t)

b2DD ∷ Boundary → DD
b2DD (SD α β) = DD (toC α) β
b2DD (DS α β) = DD α (toC β)

dd2P ∷ DD → P
dd2P (DD α β) = P φ (dd2P' u α) (dd2P' (p/u) β)
    where
        φ = 2* atan u
        u = norm (1-α)/norm(1+α)

-- if |1-α|/|1+α| = u, how far along that arc is the particular α?
dd2P' ∷ Double → Complex Double → Double
dd2P' u (x:+y)
    | u =~ 1 = y
    | otherwise = s
    where
        r = abs $ 2*u/(1-u^2)
        f = asin (y/r)
        s = r*f

b2P = dd2P . b2DD

ppSwap ∷ P → P
ppSwap (P φ s t) = P (2 * atan (p/tan (φ/2))) t s

boundaries = [boundaryArc (p,q) True ++ boundaryArc (p,q) False | q <- qs]

isPinDisc ∷ P → Bool
isPinDisc = inDisc . p2DD

pPts ∷ [P]
pPts = filter isPinDisc [P φ s t | φ <- φrange, s <- arcRange φ, t <- arcRange φ ]
    where
        φrange = [0,(pi/linearSteps)..pi]
        sMax φ
            | φ =~ (pi/2) = offset
            | otherwise = offset * (tan φ * (abs $ pi/2 - φ))
        steps = quadraticSteps
        offset = 0.99
        arcRange φ = map (\k → sMax φ * (2*k/steps-1)) [0..steps]

--annotated points
annoP ∷ [(P,DD,Double)]
annoP = map (\p → let dd = p2DD p in (p, dd, cond2 dd)) pPts

save = writeOut ("pBalls/" ++ version) goodAnnoP'

goodAnnoP' =
    id
    -- . filter (\(p,dd,c) → nearInteger 0.05 (c*p'))
    . filter (\(p,dd,c) → not $ f1Infinite 0.01 dd)
    $ annoP

goodAnnoP ∷ IO [(P,DD,Double)]
goodAnnoP = readIn ("pBalls/" ++ version) 200

findPiece ∷ Double → [(P,DD,Double)] → [(P,DD,Double)]
findPiece q = filter (\(p,dd,c) → abs (c-q) < 0.05)

findPieceP ∷ Double → [(P,DD,Double)] → [P]
findPieceP q pts = pPts ++ (crop . map b2P $ bound)
    where
        bound = boundaryArc (p,q) True ++ boundaryArc (p,q) False
        pPts' = map fst3 (findPiece q pts)
        pPts = pPts' ++ map ppSwap pPts'

findPieceDD ∷ Double → [(P,DD,Double)] → [DD]
findPieceDD q pts = map snd3 (findPiece q pts) ++ map b2DD bound
    where
        bound = boundaryArc (p,q) True ++ boundaryArc (p,q) False

crop = filter ((\(x,y,z) → abs z < 3).p2Trip)

kε ∷ Double
kε = 0.2

findPiecesK ∷ [[P]] → [[P]]
findPiecesK pts = groupedPts
    where
        pts' = concat pts
        -- labelledPts ∷ [(P,Double)]
        labelledPts = zip pts' . map (kSort . realPart . modulus . p2DD) $ pts'
        sortedPts = sortBy (compare `on` snd) labelledPts
        groupedPts = map (map fst) . groupBy ((==) `on` snd) $ sortedPts
        kSort ∷ Double → Integer
        kSort k = floor (k/kε)

-- modulus (DD a b) = fromReal $ (-d+f)/(d+f)
--     where
--         d = norm (a-b)
--         f = norm (1 - conjugate a * b)


αout ∷ DD → DD
αout (DD α β) = DD (α /. normsq' α) β
βout (DD α β) = DD α (β /. normsq' β)
αβout (DD α β) = DD (α /. normsq' α) (β /. normsq' β)

expand ∷ [DD] → [DD]
-- expand ps = ps ++ [ f $ pt | pt <- ps, f <- [αout,βout,αβout] ]
expand ps = filter inBigishBallDD $ ps ++ [ f pt | pt <- ps, f <- [αout,βout,αβout] ]

inBigishBallDD ∷ DD → Bool
inBigishBallDD (DD α β) = normsq' α < 100 && normsq' β < 100

inBigishBallP ∷ P → Bool
inBigishBallP p = let (x,y,z) = p2Trip p in 0 <= x && x <= pi && -pi <= y && y <= pi && -pi <= z && z <= pi










pDiags ∷ [P]
pDiags = filter isPinDisc $ [P φ s s | s <- arcRange]
    where
        φ = 2*atan (sqrt p)
        arcRange = [-1,-0.99..1]

pDelauney ∷ [P]
pDelauney = filter isPinDisc $ [P φ s (-s) | φ <- φRange, s <- arcRange]
    where
        φRange = [0,(pi/60)..pi]
        arcRange = [-1,-0.99..1]

pConformal ∷ [P]
pConformal = map dd2P $ [DD 0 β | β <- discPts 20] ++ [DD α 0 | α <- discPts 20]

nearInteger eps x = d' < eps || d_ < eps
    where
        x' = fromInteger $ ceiling x
        d' = x' - x -- x'>x so this is always pos
        x_ = fromInteger $ floor x
        d_ = x - x_  -- x_<x so this is always pos too

inRange l u x = l < x && x < u

fst3 (x,_,_) = x
snd3 (_,y,_) = y

cond2 ∷ DD → Double
cond2 dd = p*gamma_2_minus dd - gamma_2_plus dd


-- CUT POINTS
f1Infinite eps (DD (x:+y) (z:+w)) = abs (((z-1)^2 + w^2)*y - ((x-1)^2 + y^2)*w) < eps

f1InfPts = filter (\(p,dd,c) → f1Infinite 0.01 dd) $ annoP
f1InfPtsP = map fst3 f1InfPts
f1InfPtsDD = map snd3 f1InfPts












---- file utils

chunkify _ [] = []
chunkify n xs = y : chunkify n z
    where (y,z) = splitAt n xs

writeOut :: Show a => Text → [a] → IO ()
writeOut s pts= do
    let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") ([1..] :: [Integer])
    let dir = directory . head $ fileNames
    makeFolderIfNeeded dir
    let ptsChunks = chunkify 5000 pts
    let files = zipWith writeFile fileNames (map show ptsChunks)
    sequence_ files

extractFile :: Read a => FilePath → IO [a]
extractFile s = do
    bool <- F.isFile s
    let yup = do {
        text <- readFile s;
        return (read text)}
    if bool then yup else return []

readIn :: Read a => Text → Integer → IO [a]
readIn s n = do
    let fileNames = map (\n → FP.fromText $ s ++ show n ++ ".pts") ([1..]::[Integer])
    let files = take n fileNames
    liftM concat . mapM extractFile $ files

makeFolderIfNeeded :: FilePath → IO ()
makeFolderIfNeeded path = do
    bool <- F.isDirectory path
    if bool then return () else F.createTree path

--- graph that shit




mainReadAndGraph :: Text → Integer → IO Bool
mainReadAndGraph s n = do
    moduliPts <- readIn s n
    mainGraph moduliPts

mainGraph :: [DD] → IO Bool
mainGraph pts = plot' [Interactive] X11
        [
        -- Data3D [Color White, Style Dots] [] (map projby pts)
        Data3D [Color Yellow, Style Dots] [] (map projbx pts)
        -- , Data3D [Color White, Style Dots] [] (map projby pts)
        -- , Data3D [Color Green, Style Points] [] (map projby [startPt])
        -- , Data3D [Color Blue, Style Dots] [] (map projbx boundaryPts)
        -- , Data3D [Color White, Style Dots] [] (map projbx symPts)
        -- , Data3D [Color White, Style Dots] [] (map projby pts)
        -- , Data3D [Color Green, Style Points] [] (map projay [startPt])
        -- , Data3D [Color Blue, Style Dots] [] (map projay boundaryPts)
        -- , Data3D [Color Green, Style Dots] [] (map projay conformalPts)
        -- , Data3D [Color White, Style Dots] [] (map projay singularPts)
        ]

colours = cycle [White, Yellow, Orange, LightGreen, LightBlue, LightRed, Green]
mainGraphs :: (a → (Double,Double,Double)) → [[a]] → IO Bool
mainGraphs proj pts = do
    let zipper c ps = Data3D [Color c, Style Dots] stdOptions (map proj ps)
    plot' [Interactive] X11 $
        zipWith zipper colours pts
