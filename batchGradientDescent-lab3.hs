import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M

import Text.Printf
import System.IO

{---------------------------------------------------------------------------
-                             Data definition                             -
---------------------------------------------------------------------------}

-- Example ---------------------
-- Saved in sparse format, only words which occur in document

type Example = M.Map FMark FVal

-- Feature mark: number of word in dictionary
-- Feature value: tf x idf
type FMark  = Int
type FVal   = Double

-- Labeled set: list of (label, example) pairs
type Label      = Int
type LabeledSetSparse = [(Label, Example)]

type LabeledSet = [(Label, Vector)]

-- Vector ---------------------
-- Used for displaying weights, xs, ...
type Vector = [FVal]

-- Probability ----------------
type Probability = Double

-- Dimension ------------------
type Dim = Int

-- Regularization factor ------
type RegFactor = Double

-- Error value ----------------
type ErrorVal = Double

-- Eta value ------------------
type EtaVal = Double

-- Epsilon value --------------
type EpsVal = Double

-- Percentage value -----------
type Percentage = Double

{---------------------------------------------------------------------------
-                             Show functions                              -
---------------------------------------------------------------------------}

printVector :: (PrintfArg a, IsChar b) => [a] -> [b]
printVector = concatMap (printf "%.3f\n")

{---------------------------------------------------------------------------
-                            Loading from file                            -
---------------------------------------------------------------------------}

-- Converts example from sparse map record to list
-- Also expands with 1
-- n is final dimension of EXPANDED vector
example2Vector :: Example -> Dim -> Vector
example2Vector ex n = 1 : map getVal [0 .. n-2]
    where getVal idx = M.findWithDefault 0 idx ex

fromSparse :: Dim -> LabeledSetSparse -> LabeledSet
fromSparse dim = map (\(l, ex) -> (l, example2Vector ex dim))

-- Load labeled set from file
loadLSet :: Dim -> String -> LabeledSet
loadLSet dim = fromSparse dim . foldr addExample [] . init . splitOn "\n"

-- Adds (label, example) to labeled set
addExample :: String -> LabeledSetSparse -> LabeledSetSparse
addExample row lSet = (label, example) : lSet
    where (labelS : feats) = words row
          label   = read labelS :: Int
          example = foldr addFeature M.empty feats

-- Adds feature to example
addFeature :: String -> Example -> Example
addFeature s = M.insert (read fMarkS :: FMark) (read fValS :: FVal)
    where (fMarkS : [fValS]) = splitOn ":" s

-- Load dictionary -------------------------------

loadDict :: String -> [String]
loadDict = map (head . words) . init . splitOn "\n"

{---------------------------------------------------------------------------
-                         Batch gradient descent                          -
---------------------------------------------------------------------------}
         
-- Gradient optimized
-- Calculates L2 gradient at given point
getL2Grad :: Vector -> RegFactor -> LabeledSet -> Vector
getL2Grad w rf lSet = w0 : wL2
    where (w0 : ws)  = getGrad w lSet
          wL2        = zipWith (+) ws (map (*rf) $ tail w)

-- Calculates gradient at given point
getGrad :: Vector -> LabeledSet -> Vector
getGrad w = foldr addAddend w0 
    where w0 = replicate (length w) 0
          addAddend lex = zipWith (+) (gradAddend w lex)

-- Calculates contribution to gradient for given example
gradAddend :: Vector -> (Label, Vector) -> Vector
gradAddend w (y, x) = map (*diff) x
    where diff  = h w x - fromIntegral y


-- Logical regression hypothesis (sigmoidal)
h :: Vector -> Vector -> Probability
h w x = 1 / (1 + exp(-wx))
    where wx = sum $ zipWith (*) w x

-- Line search ---------------------------

-- Finds optimal eta parameter using line search algorithm
-- Returns new weight vector
lineSearch :: Vector -> Vector -> EtaVal -> (Vector -> ErrorVal) -> Vector
lineSearch w grad dEta errF = findEta w (errF w)
    where 
        findEta currW currVal
            | nextVal >= currVal = currW
            | otherwise          = findEta nextW nextVal

            where nextW   = zipWith (-) currW (map (*dEta) grad)
                  nextVal = errF nextW
          
-- Batch gradient descent -----------------

-- bgdDebug :: Vector -> EpsVal -> EtaVal -> RegFactor -> LabeledSet -> (Vector, Int)
bgdDebug w0 eps dEta rf lSet = findMinArg w0 (errorF lSet rf w0) 0
    where
        findMinArg w errVal cnt
            | abs(errValNew - errVal) < eps = do
                                        putStrLn $ (show errVal) ++ " " ++ (show errValNew)
                                        putStrLn "gotov sam"

                                        return (nextW, cnt)
            | otherwise = do 
                        putStrLn $ show cnt ++ " greska: " ++ show errVal
                        findMinArg nextW errValNew (cnt + 1)

            where grad       = getL2Grad w rf lSet
                  nextW      = lineSearch w grad dEta $ errorF lSet rf
                  errValNew  = errorF lSet rf nextW



bgd :: Vector -> EpsVal -> EtaVal -> RegFactor -> LabeledSet -> (Vector, Int)
bgd w0 eps dEta rf lSet = findMinArg w0 (errorF lSet rf w0) 0
    where
        findMinArg w errVal cnt
            | abs(errValNew - errVal) < eps = (nextW, cnt)
            | otherwise                     = findMinArg nextW errValNew (cnt + 1)


            where grad       = getL2Grad w rf lSet
                  nextW      = lineSearch w grad dEta $ errorF lSet rf
                  errValNew  = errorF lSet rf nextW


{---------------------------------------------------------------------------
-                             Error function                              -
---------------------------------------------------------------------------}

-- Calculates error for given weights and labeled set
errorF :: LabeledSet -> RegFactor -> Vector -> ErrorVal
errorF lSet rf w = (sum $ map (errorPerExample w) lSet) + regAddend
    where regAddend = (rf / 2) * ww
          ww = sum $ zipWith (*) w w

-- Calculates error contribution of given example
-- Zbog log baca infinity, zanemarit cemo zasada, mozda nece smetati
errorPerExample :: Vector -> (Label, Vector) -> ErrorVal
errorPerExample w (y, x) = if y == 1 then - log (h w x) else - log (1 - h w x)


{---------------------------------------------------------------------------
-                             Classification                              -
---------------------------------------------------------------------------}

classify :: (Vector -> Probability) -> Vector -> Label
classify h x = if h x > 0.5 then 1 else 0

testOnSet :: (Vector -> Label) -> LabeledSet -> Percentage
testOnSet cl lSet = wrong / totalNum
    where wrong    = fromIntegral $ length $ filter (== False) $ map (\(y, x) -> cl x == y) lSet
          totalNum = fromIntegral $ length lSet

{---------------------------------------------------------------------------
-                                 Testing                                 -
---------------------------------------------------------------------------}

tryLambdas :: [RegFactor] -> LabeledSet -> LabeledSet -> [(RegFactor, Percentage, Int)]
tryLambdas rfs learnSet valSet = map getParams rfs
    where 
        dim  = 5692
        w0   = replicate dim 0
        eps  = 0.0001
        dEta = 0.1

        getParams rf = (rf, perc, iterNum) 
            where (wOpt, iterNum) = bgd w0 eps dEta rf learnSet
                  cl   = classify (h wOpt)
                  perc = testOnSet cl valSet

printParams :: [(RegFactor, Percentage, Int)] -> String
printParams = concatMap printParam
    where printParam (rf, perc, iterNum) = show rf ++ " & " ++ show perc ++ " & " ++ show iterNum ++ " \\\\ \\hline\n"

{---------------------------------------------------------------------------
-                                  Utils                                  -
---------------------------------------------------------------------------}

writeVector :: Handle -> Vector -> IO ()
writeVector _ [] = return ()
writeVector h (v : vs) = do
                hPutStrLn h $ printf "%.6f" v
                writeVector h vs

getSignificant :: Int -> Vector -> [String] -> ([String], [FVal])
getSignificant n (w0 : w) dict = ( map (dict !!) idxs, map (w !!) idxs )
    where idxs = map fst $ take n $ sortBy (\a b -> compare (snd b) (snd a)) $ zip [0..] w

{---------------------------------------------------------------------------
-                                  Main                                   -
---------------------------------------------------------------------------}

main2 = do
    inHandle <- openFile "train-set.txt" ReadMode
    str <- hGetContents inHandle
    
    -- Learning set -----------------------------
    
    let dim = 5692
    let w0 = replicate dim 0
    let eps = 0.0001
    let dEta = 0.1
    let rf = 0

    -- Testing on test set ------------------------

    let learningSet = loadLSet dim str

    let (wOpt, cnt) = bgd w0 eps dEta rf learningSet
    putStrLn $ printVector wOpt

    print $ errorF learningSet rf wOpt

    outHandle <- openFile "tezine1.txt" WriteMode
    writeVector outHandle wOpt

    hClose inHandle
    hClose outHandle

main = do
    inHandle <- openFile "train-set.txt" ReadMode
    str <- hGetContents inHandle

    inHandle2 <- openFile "validation-set.txt" ReadMode
    str2 <- hGetContents inHandle2

    inHandle4 <- openFile "test-set.txt" ReadMode
    str4 <- hGetContents inHandle4

    -- Learning set -----------------------------
    
    let dim = 5692
    let w0 = replicate dim 0
    let eps = 0.0001
    let dEta = 0.1
    let rf = 3.23

    let learningSet = loadLSet dim str

    -- Test on validation set --------------------

    let validationSet = loadLSet dim str2

    let rfs = sort [0, 0.5, 0.7, 1, 2,
               2.13, 3.56, 3.23, 0.9, 2.1,
               1.87, 0.89, 3.55, 2.22, 2.87,
               50, 100, 200, 1000, 10000]
    let params = tryLambdas rfs learningSet validationSet
    putStrLn $ printParams params

    -- Learn on both sets -------------------------

    let bigSet = learningSet ++ validationSet
    let (wOpt, _) = bgd w0 eps dEta rf bigSet

    -- Testing on test set ------------------------

    let testSet = loadLSet dim str4

    let cl   = classify (h wOpt)
    let perc = testOnSet cl testSet
    putStrLn $ "Greska je: " ++ show perc

    inHandle3 <- openFile "tezine2.txt" WriteMode
    --writeVector inHandle3 wOpt
    
    -- Get 20 most positive words ------------------

    inHandleDict <- openFile "rjecnik.txt" ReadMode
    strDict <- hGetContents inHandleDict
    let dict = loadDict strDict

    let (sig, vals) = getSignificant 20 wOpt dict
    -- print sig
    -- print "-------------------------------"
    -- print vals

    hClose inHandle
    hClose inHandle2
    hClose inHandle3
    hClose inHandle4
