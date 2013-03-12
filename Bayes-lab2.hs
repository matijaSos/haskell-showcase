import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M

import System.IO

-- Data definition -----------------------------

type LabeledSet = M.Map Class [Sample]

type Sample = M.Map FeatureName FeatureVal
type Class  = String

type FeatureVal  = String
type FeatureName = String

data Factor = Factor {
        left    :: [FeatureName],
        right   :: [FeatureName]
} deriving (Ord, Eq)

data FactorVal = FactorVal {
        leftVal    :: Sample,
        rightVal   :: Sample
} deriving (Ord, Eq)

type Probability = Double
type OccNum = Int

type Apriori = M.Map Class Probability
type Lhood   = M.Map (Factor, FactorVal, Class) Probability

data Params = Params {
    apriori :: Apriori,
    lhood   :: Lhood
}

type MutualInfo = M.Map (FeatureName, FeatureName) Probability

-- Show functions --------------------------------------------------

showLhood :: Lhood -> String
showLhood = M.foldWithKey showOne ""
    where showOne (_, fVal, cl) prob acc = "p" ++ "(" ++ show fVal ++ " " ++ show cl ++ ") = " 
                                            ++ show prob ++ "\n" ++ acc

showSample :: Sample -> String
showSample = M.foldWithKey (\k val acc -> show k ++ ":" ++ val ++  " " ++ acc) ""

instance Show FactorVal where
    show fVal = showSample (leftVal fVal) ++ "|" ++ showSample (rightVal fVal)

instance Show Factor where
    show f = show (left f) ++ " | " ++ show (right f)

showMutualInfoAll :: MutualInfo -> String
showMutualInfoAll = M.foldWithKey showOne ""
    where showOne (fn1, fn2) p acc = "I (" ++ fn1 ++ ", " ++ fn2 ++ " $|$ C) = " ++ show p ++ "\n" ++ acc

showGraph :: Graph -> String
showGraph = M.foldWithKey (\node nbrs acc -> node ++ " -> " ++ show nbrs ++ "\n" ++ acc) ""

-- Create training set from file -----------------------------------

getLabeledSet :: String -> LabeledSet
getLabeledSet s = foldr (addSample names) M.empty (init rest)
    where
        (header : rest) = splitOn "\n" s
        names = init $ words header

addSample :: [String] -> String -> LabeledSet -> LabeledSet    
addSample names line = M.insertWith (++) c [sample]
    where (c : xs) = reverse $ words line
          sample   = M.fromList $ zip names (reverse xs)

-- Training - get parameters --------------------------------------

trainBayes :: LabeledSet -> [Factor] -> Params
trainBayes lSet fs = Params (getApriori lSet) (getLhood lSet fs)

getApriori :: LabeledSet -> Apriori
getApriori lSet = M.foldWithKey addApriori M.empty lSet
    where addApriori cl ss = M.insert cl $ fromIntegral (length ss) / fromIntegral sc
          sc = M.fold (\ss acc -> acc + length ss) 0 lSet

getLhood :: LabeledSet -> [Factor] -> Lhood
getLhood lSet fs = M.fromList $ map genMapEntry allTriples
    where genMapEntry key = (key, getProb lSet key)
          allTriples     = concat [ [ (f, comb, cl) | comb <- allCombs f ] | f <- fs, cl <- classList ]

getProb :: LabeledSet -> (Factor, FactorVal, Class) -> Probability 
getProb lSet (f, fVal, cl) = prob
    where   (Just samples) = M.lookup cl lSet
            (lr, r)        = foldr (checkSample fVal) (0, 0) samples

            prob = if r == 0 then 0 else fromIntegral lr / fromIntegral r

            -- Only for naive Bayes
            k       = length $ getPossibleVals $ head $ left f
            probMLE = if r == 0 then 0 else fromIntegral (lr + 1) / fromIntegral (r + k)

checkSample :: FactorVal -> Sample -> (OccNum, OccNum) -> (OccNum, OccNum)
checkSample fVal s (lr, r) = (lr', r')
    where r'  = if rightVal fVal `M.isSubmapOf` s then r + 1 else r 
          lr' = if M.union (rightVal fVal) (leftVal fVal) `M.isSubmapOf` s then lr + 1 else lr

allCombs :: Factor -> [FactorVal]
allCombs f = map (createFactorVal f) $ sequence $ l ++ r
    where r = map getPossibleVals (right f)
          l = map getPossibleVals (left f)

createFactorVal :: Factor -> [FeatureVal] -> FactorVal
createFactorVal f vals = FactorVal l r
    where l = M.fromList $ zip (left f) vals
          r = M.fromList $ zip (right f) (drop (length $ left f) vals)

getPossibleVals :: String -> [String]
getPossibleVals name = fromJust $ M.lookup name possibleValues

-- Factor generators --------------------------------------------------

naive :: M.Map FeatureName [FeatureVal] -> [Factor]
naive = map (\k -> Factor [k] []) . M.keys 

tanBayes :: Int -> MutualInfo -> [Factor]
tanBayes n = getFactorsFromDirGraph . directGraph . createGraph n . getBestPairs 

getBestPairs :: MutualInfo -> [(FeatureName, FeatureName)]
getBestPairs mInfo = map fst $ sortBy sortMI $ M.toList mInfo

sortMI (pair1, p1) (pair2, p2)
    | p1 < p2 = GT
    | p1 > p2 = LT
    | otherwise = compare pair1 pair2

-- Graph manipulation for TAN -------------------------------

type Graph = M.Map FeatureName [FeatureName]

getFactorsFromDirGraph :: Graph -> [Factor]
getFactorsFromDirGraph g = map createFactor (M.toList g)
    where createFactor (fn, nbrs) = Factor [fn] nbrs

reverseGraph :: Graph -> Graph
reverseGraph g = foldr addToGraph M.empty (concatMap createBackLinks (M.toList g))
    where createBackLinks (parent, kids) = map (\k -> (k, parent)) kids
          addToGraph (src, dest) g = removeLinkOne (insertLink g src dest) dest src

directGraph :: Graph -> Graph
directGraph g = reverseGraph $ directGraph' g (fst $ head $ M.toList g)
    where 
        directGraph' g node = foldr (\nbr acc -> directGraph' acc nbr) g' nbrs 
            where nbrs = fromJust $ M.lookup node g
                  g'   = foldr (\nbr acc -> removeLinkOne acc nbr node) g nbrs

createGraph :: Int -> [(FeatureName, FeatureName)] -> Graph
createGraph n = createGraph' 0 M.empty 
    where 
        createGraph' cnt g ((fn1, fn2) : rest)
            | cnt == n - 1         = g 
            | makesCycle fn1 fn2 g = createGraph' cnt g rest
            | otherwise            = createGraph' (cnt + 1) (insertLink g fn1 fn2) rest

makesCycle :: FeatureName -> FeatureName -> Graph -> Bool
makesCycle fn1 fn2 g = hasCycleFrom fn1 $ insertLink g fn1 fn2

hasCycleFrom :: FeatureName -> Graph -> Bool
hasCycleFrom fn g = fn `elem` getDeepNbrs g fn

getDeepNbrs :: Graph -> FeatureName -> [FeatureName]
getDeepNbrs g fn = (concatMap (\(n, g) -> getDeepNbrs g n) $ zip nbrs gs) ++ nbrs
    where nbrs = fromJust $ M.lookup fn g
          gs   = map (removeLink g fn) nbrs

removeLinkOne :: Graph -> FeatureName -> FeatureName -> Graph
removeLinkOne g src dest = g'
    where g' = M.adjust (filter (/= dest)) src g

removeLink :: Graph -> FeatureName -> FeatureName -> Graph
removeLink g fn1 fn2 = g2
    where g1 = M.adjust (filter (/= fn2)) fn1 g
          g2 = M.adjust (filter (/= fn1)) fn2 g1

insertLink :: Graph -> FeatureName -> FeatureName -> Graph
insertLink g fn1 fn2 = g2
    where g1 = M.adjust (fn2 :) fn1 ( if fn1 `M.member` g then g else M.insert fn1 [] g )
          g2 = M.adjust (fn1 :) fn2 ( if fn2 `M.member` g1 then g1 else M.insert fn2 [] g1 )

-- Mutual information tools -------------------------------------------

mutInfoAll :: LabeledSet -> [FeatureName] -> MutualInfo
mutInfoAll lSet fnames = foldr insToMap M.empty [ (fn1, fn2) | fn1 <- fnames, fn2 <- fnames, fn1 < fn2 ]
    where insToMap k@(fn1, fn2) = M.insert k (mutInfo lSet fn1 fn2)

mutInfo :: LabeledSet -> FeatureName -> FeatureName -> Probability
mutInfo lSet f1 f2 = sum $ map (calcPart lSet) [ (fVal, cl) | fVal <- allCombs f, cl <- classList ]
    where f = Factor [f1, f2] []

calcPart :: LabeledSet -> (FactorVal, Class) -> Probability
calcPart lSet (fVal, cl) = if px == 0 || py == 0  || pxy == 0 || pxyc == 0 then 0 else pxyc * log (pxy / (px * py))
    where
        pxyc = tripleP lSet fVal cl

        pxy = condFactP lSet (fVal, cl)
        px  = condFactP lSet (xVal, cl)
        py  = condFactP lSet (yVal, cl)

        xVal = FactorVal (M.fromList $ init (M.toList $ leftVal fVal)) (M.fromList [])
        yVal = FactorVal (M.fromList $ tail (M.toList $ leftVal fVal)) (M.fromList [])


tripleP :: LabeledSet -> FactorVal -> Class -> Probability
tripleP lSet fVal cl = fromIntegral occNum / fromIntegral totalSampleNum
    where (Just samples) = M.lookup cl lSet
          occNum         = foldr (\s acc -> if leftVal fVal `M.isSubmapOf` s then acc + 1 else acc) 0 samples
          totalSampleNum = sum $ map (length . snd) $ M.toList lSet

condFactP :: LabeledSet -> (FactorVal, Class) -> Probability 
condFactP lSet (fVal, cl) = prob
    where   (Just samples) = M.lookup cl lSet
            (lr, r)        = foldr (checkSample fVal) (0, 0) samples
            prob = if r == 0 then 0 else fromIntegral lr / fromIntegral r

-- Classification -----------------------------------------------------

bayesClassify :: (Params, [Factor], [Class]) -> Sample -> Class
bayesClassify (ps, fs, cls)  = fst . maximumBy (\a b -> (snd a) `compare` (snd b)) . calcAposterior (ps, fs, cls)

calcAposterior :: (Params, [Factor], [Class]) -> Sample -> [(Class, Probability)]
calcAposterior (ps, fs, cls) s = map divWithP clJoint
    where 
        divWithP (cl, jP) = if p == 0 then (cl, 0) else (cl, jP / p) 
    
        clJoint = map calcP cls
        p = sum $ map snd clJoint

        calcP cl = (cl, ap * p)  
            where ap = fromJust $ M.lookup cl (apriori ps)
                  p  = foldr (addFactor cl) 1 fs

        addFactor cl f sum = sum * p
            where fVal = createFVal f s
                  p    = fromJust $ M.lookup (f, fVal, cl) (lhood ps)

createFVal :: Factor -> Sample -> FactorVal
createFVal f s = FactorVal (addVals $ left f) (addVals $ right f)
    where
          addVals names = M.fromList $ map (\n -> (n, fromJust $ M.lookup n s)) names

-- Testing classifier ----------------------------------------------

testClassifier :: (Sample -> Class) -> LabeledSet -> Double
testClassifier clasif lSet = fromIntegral totalWrong / fromIntegral totalNum

    where testClass (cl, ss) = foldr (checkError cl) (0, 0) ss 
          checkError cl s (w, cnt) = (if cl /= clasif s then w + 1 else w, cnt + 1)

          resList = map testClass $ M.toList lSet

          totalWrong = sum (map fst resList)
          totalNum   = sum (map snd resList)


-- Main ------------------------------------------

main = do
    inHandle <- openFile "CarEvaluation-Train.txt" ReadMode
    learnContents <- hGetContents inHandle

    inHandle2 <- openFile "CarEvaluation-Test.txt" ReadMode
    testContents <- hGetContents inHandle2

    -- Acquiring datasets

    let learningSet = getLabeledSet learnContents
    let testingSet  = getLabeledSet testContents

    -- Training

    let factors = naive possibleValues
    let params = trainBayes learningSet factors
   
    putStrLn $ showLhood (lhood params)
    print $ apriori params

    -- Classification
    
    let aposterior = calcAposterior (params, factors, classList) sample1
    print aposterior

    let res = bayesClassify (params, factors, classList) sample1
    print res

    -- Create classifier

    let bayesClassif = bayesClassify (params, factors, classList)

    -- Testing classifier
    let empError = testClassifier bayesClassif learningSet
    putStrLn ("emp greska: " ++ show empError)

    let genError = testClassifier bayesClassif testingSet
    putStrLn ("gen greska: " ++ show genError)

    putStrLn "gotov"
    hClose inHandle
    hClose inHandle2

main2 = do
    inHandle <- openFile "CarEvaluation-Train.txt" ReadMode
    learnContents <- hGetContents inHandle

    inHandle2 <- openFile "CarEvaluation-Test.txt" ReadMode
    testContents <- hGetContents inHandle2

    -- Acquiring datasets

    let learningSet = getLabeledSet learnContents
    let testingSet  = getLabeledSet testContents

    -- Mutual info

    let iAll = mutInfoAll learningSet (M.keys possibleValues)
    putStrLn $ showMutualInfoAll iAll

    -- Train TAN classifier

    let factors = tanBayes 6 iAll
    print factors

    let params = trainBayes learningSet factors

    let bayesTANClassif = bayesClassify (params, factors, classList)

    -- Testing classifier
    let empError = testClassifier bayesTANClassif learningSet
    putStrLn ("emp greska: " ++ show empError)

    let genError = testClassifier bayesTANClassif testingSet
    putStrLn ("gen greska: " ++ show genError)

    hClose inHandle
    hClose inHandle2

-- Test data -------------------------------------

sample1 = M.fromList [ ("buying_price", "high"), ("maintenance_costs", "vhigh"), ("number_of_doors", "3"), 
                       ("person_capacity", "2"), ("luggage_boot", "big"), ("safety_estimation", "med") ]

buyingPrice = ["vhigh", "high", "med", "low"]
maintenanceCosts = ["vhigh", "high", "med", "low"]
numberOfDoors = ["2", "3", "4", "5more"]
personCapacity = ["2", "4", "more"]
luggageBoot = ["small", "med", "big"]
safetyEstimation = ["low", "med", "high"]

possibleValues :: M.Map FeatureName [FeatureVal]
possibleValues = M.fromList [("buying_price", buyingPrice), ("maintenance_costs", maintenanceCosts),
                             ("number_of_doors", numberOfDoors), ("person_capacity", personCapacity),
                             ("luggage_boot", luggageBoot), ("safety_estimation", safetyEstimation)]

classList = ["unacc", "acc", "good", "vgood"]

factor1 = Factor {
    left = ["buying_price"],
    right = []
}

factor2 = Factor {
    left = ["luggage_boot"],
    right = []
}
