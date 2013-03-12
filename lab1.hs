import Data.List
import Data.Char
import Data.List.Split
import Numeric.LinearAlgebra
import qualified Data.Map as M

import System.Environment
import System.IO
import System.Random

-- Data definition --------------------------------------

data Value = StrVal String | IntVal Int deriving (Show)
type ValueList = [Value]

data Dataset = Dataset {
    names   :: [String],
    values  :: Matrix Double
} deriving (Show)

-- Preview functions -------------------------------------

disp :: Matrix Double -> IO ()
disp = putStrLn . dispf 2

latex m = putStrLn $ latexFormat "bmatrix" (dispf 2 m)

printParams :: (Vector Double, Matrix Double) -> IO ()
printParams (meanVal, covMatrix) = do
    -- Calculate muML
    putStrLn "Mean value:"
    disp $ fromRows [meanVal]
    latex $ fromRows [meanVal]

    -- Calculate sigmaML
    putStrLn "Covariation matrix:"
    disp covMatrix
    latex covMatrix

-- Parsing functions -------------------------------------

extractData :: String -> Dataset
extractData s = Dataset names values
    where
        (header : rest) = splitOn "\n" s
        names  = init $ words header
        values = fromLists $ foldr addRow [] (init rest)

addRow :: String -> [[Double]] -> [[Double]] 
addRow s vals = newRow : vals
    where newRow = map strToDouble (init $ words s)

strToDouble :: String -> Double
strToDouble s = read s :: Double

-- Calculation functions ----------------------------------

getMeanVal :: Matrix Double -> Vector Double
getMeanVal m = mapVector (/n) rowSum
    where
        rowSum = constant 1 (rows m) <> m
        n      = fromIntegral $ rows m

getCovMatrix :: Matrix Double -> Vector Double -> Matrix Double
getCovMatrix m mu = foldl' procRow zeroSquareM (toRows m) / n
    where
        n               = fromIntegral $ rows m
        zeroSquareM     = zeroMatrix (cols m) (cols m)
        procRow res row = res + (fromColumns [d] <> fromRows [d])
            where d = row - mu

getParams :: Matrix Double -> (Vector Double, Matrix Double)        
getParams valM = (muML, getCovMatrix valM muML)
    where
        muML = getMeanVal valM

tryRandom :: Int -> Int -> [Int] -> Matrix Double -> [(Vector Double, Matrix Double)]
tryRandom n m rands matrix = foldr addResult [] randsSet
    where
        addResult rs acc = getParams (extractRows rs matrix) : acc

        randsSet = fst $ foldr takeN ([], rands) [1..m]
        takeN _ (ret, rs) = (take n rs : ret, drop n rs) 

-- For mean value estimator
getParamsMeanValEst :: [Vector Double] -> (Vector Double, Vector Double)
getParamsMeanValEst vs = (mv, var)
    where
        matrix = fromRows vs
        n      = fromIntegral $ rows matrix
        mv     = (constant 1.0 (rows matrix) <> matrix) / n

        zeroRow = constant 0.0 (cols matrix)
        var     = foldr (\v acc -> acc + (v - mv)^2) zeroRow vs / n

-- For covariation matrix estimator
getParamsCovMatrixEst :: [Matrix Double] -> (Matrix Double, Matrix Double)
getParamsCovMatrixEst ms = (mv, var)
    where
        n = fromIntegral $ length ms
        zeroSqM = zeroMatrix (rows $ head ms) (cols $ head ms)
        mv = foldr sumMx zeroSqM ms / n
            where sumMx m acc = acc + m

        var = foldr (\m acc -> acc + (m - mv)^2) zeroSqM ms / n

-- Utility functions -------------------------------------

zeroMatrix :: Int -> Int -> Matrix Double
zeroMatrix m n = reshape n $ constant 0.0 (n * m)

extractColumns :: Element t => [Int] -> Matrix t -> Matrix t
extractColumns cs = trans . extractRows cs . trans

getRandoms :: Int -> StdGen -> [Int]
getRandoms hi = map (`mod` hi) . randoms

-- Main function ------------------------------------------

tryRandomIO :: Int -> Int -> [Int] -> Matrix Double -> IO ()
tryRandomIO n m rands matrix = do
    let test = tryRandom n m rands matrix

    let (mvEstMv, mvEstVar) = getParamsMeanValEst $ map fst test
    putStrLn "MVE mean value: "
    disp $ fromRows [mvEstMv]
    latex $ fromRows [mvEstMv]

    putStrLn "MVE variance: "
    disp $ fromRows [mvEstVar]
    latex $ fromRows [mvEstVar]

    let (covEstMv, covEstVar) = getParamsCovMatrixEst $ map snd test
    putStrLn "covE mean value: "
    disp covEstMv
    latex covEstMv

    putStrLn "covE variance: "
    disp covEstVar
    latex covEstVar

main = do 
    -- Read learning set from file
    inHandle <- openFile "vehicleSilhouettes.txt" ReadMode
    contents <- hGetContents inHandle

    let dataset = extractData contents
    let valMatrix = values dataset
    putStrLn $ "Cols / number of features: " ++ show (cols valMatrix)
    putStrLn $ "Rows / number of samples:  " ++ show (rows valMatrix) ++ "\n"

    let chosenCols = [0, 1, 2, 3, 4]
    let reducedM = extractColumns chosenCols valMatrix
    putStrLn $ "Chosen columns: " ++ show chosenCols

    -- Initialize random
    randomGen <- getStdGen
    let randomNums = getRandoms (rows valMatrix) randomGen

    -- First experiment -----------------------------------------------------------
    putStrLn "Calculating params from the whole dataset:"
    putStrLn "---------------------------------------------------------------------"

    printParams $ getParams reducedM

    -- Second experiment ----------------------------------------------------------
    putStrLn "Calculating parameters from 100 random chosen rows:"
    putStrLn "---------------------------------------------------------------------"
    
    randomGen <- getStdGen
    let chosenRows = take 100 randomNums
    let reducedM2 = extractRows chosenRows reducedM
    printParams $ getParams reducedM2

    -- Third experiment -----------------------------------------------------------
    putStrLn "100 random rows - 50 times"
    putStrLn "---------------------------------------------------------------------"

    let randomNums' = drop 100 randomNums 
    tryRandomIO 100 50 randomNums' reducedM

    -- Fourth experiment ------------------------------------------------------------
    putStrLn "200 random rows - 50 times"
    putStrLn "---------------------------------------------------------------------"

    let randomNums'' = drop (50*100) randomNums'
    tryRandomIO 200 50 randomNums'' reducedM

    -- Fifth experiment --------------------------------------------------------------
    putStrLn "Calculating params from the whole dataset: DISTANCE_CIRCULARITY and MAJOR_AXIS_DISTANCE_CIRCULARITY"
    putStrLn "---------------------------------------------------------------------"

    -- disp $ extractColumns [2, 18] valMatrix

    let newCols = [0, 1, 2, 3, 18]
    let newReducedM = extractColumns newCols valMatrix

    let ex5Res@(ex5mv, ex5cv) = getParams newReducedM
    printParams ex5Res

    putStrLn "Determinant of covMatrix: "
    print $ det ex5cv

    hClose inHandle

        
-- For testing --
testM = fromLists [ [1,2,3], [4,5,6], [7,8,9] ] :: Matrix Double
