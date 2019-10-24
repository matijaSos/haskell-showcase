-- Matija Sosic
-- 0036452499
--
-- Umjetna inteligencija - 4. laboratorijska vjezba.

import Data.List
import Data.List.Split
import Text.Printf

import System.IO
import System.Random

-- Neural network definition. ---------------------------------------------------------

printList :: [Value] -> String
printList xs = intercalate " | " $ map (printf "%.4f") xs

type Value       = Double
type DataSet = [(Value, Value)]

-- Every perceptron consists of:
--  list of weights
--  transfer function
data Perceptron = Perceptron {
    weights :: [Value],
    f       :: Value -> Value
}

instance Show Perceptron where
    show p = (intercalate " | " $ map (printf "%.4f") $ weights p) ++ "\n"

evaluateP :: Perceptron -> [Value] -> Value
evaluateP p = f p . sum . zipWith (*) (weights p) . (1 : )

crossP :: Perceptron -> Perceptron -> Perceptron
crossP p1 p2 = Perceptron newWeights (f p1)
    where
          newWeights = zipWith (\w1 w2 -> (w1 + w2) / 2) (weights p1) (weights p2)

mutateWeight :: Double -> Value -> Value -> StdGen -> (Value, StdGen)
mutateWeight p k w gen = (w', gen3)
    where (randNum, gen1) = randomR (0, 1) gen
          (diff, gen2)   = randomR (-k, k) gen1
          (w', gen3)     = if randNum < p then (w + diff, gen2) else (w, gen1)

mutateP :: Double -> Value -> Perceptron -> StdGen -> (Perceptron, StdGen)
mutateP p k perc gen = (perc {weights = newWeights}, gen')
    where (newWeights, gen') = doWithRandLs (map (mutateWeight p k) $ weights perc) gen

-- Neural network with specific architecture 1 x n x 1
data NeuralNetwork = NeuralNetwork {
    inLayer     :: Perceptron,
    hiddenLayer :: [Perceptron],
    outLayer    :: Perceptron
}

instance Show NeuralNetwork where
    show nn = "\n-----------------\n" ++
              "\n-- input layer --\n" ++ (show $ inLayer nn) ++
              "-- hidden layer -- \n" ++ (show $ hiddenLayer nn) ++
              "-- out Layer --    \n" ++ (show $ outLayer nn)

-- Get output of network for given input
--
evaluateNN :: NeuralNetwork -> Value -> Value
evaluateNN nn v = evaluateP (outLayer nn) resHid
    where 
          resIn  = evaluateP (inLayer nn) [v] : []
          resHid = map (\p -> evaluateP p resIn) (hiddenLayer nn) 

-- Crossing of two neural networks
crossNN :: NeuralNetwork -> NeuralNetwork -> NeuralNetwork
crossNN nn1 nn2 = NeuralNetwork newIn newHidden newOut
    where
          newIn     = crossP (inLayer nn1) (inLayer nn2)
          newHidden = zipWith crossP (hiddenLayer nn1) (hiddenLayer nn2)
          newOut    = crossP (outLayer nn1) (outLayer nn2)

-- Mutating neural network
mutateNN :: Double -> Value -> NeuralNetwork -> StdGen -> (NeuralNetwork, StdGen)
mutateNN p k nn gen = (NeuralNetwork newInL newHidL newOutL, gen3)

    where (newInL, gen1)  = mutateP p k (inLayer nn) gen 
          (newHidL, gen2) = doWithRandLs (map (mutateP p k) (hiddenLayer nn)) gen1
          (newOutL, gen3) = mutateP p k (outLayer nn) gen2

-- Get deviation compared to learning set
--
middleSquareDeviation :: NeuralNetwork -> DataSet -> Value
middleSquareDeviation nn lSet =  (sum $ map squareDev lSet) / fromIntegral (length lSet)
    where
          squareDev (x, fx) = (evaluateNN nn x - fx)^2


instance Taughtable NeuralNetwork where
    fitness nn ds = (middleSquareDeviation nn ds)
    cross = crossNN 
    mutate p k a gen = mutateNN p k a gen


-- Learning algorithm ----------------------------------------------------------------

class Taughtable a where
    fitness :: a -> DataSet -> Value
    cross   :: a -> a -> a
    mutate  :: Double -> Value -> a -> StdGen -> (a, StdGen)

data (Taughtable a) => Population a = Population {
    ts :: [a]
} deriving (Show)

-- Choose parent randomly (but proportionally to fitness)
chooseParent :: [(a, Value)] -> StdGen -> (a, StdGen)
chooseParent ts gen = (parent, gen')
    where 
          (randNum, gen') = randomR (0, maxVal) gen
          maxVal = sum $ map ((1 /) . snd) ts
          parent = getFirstLarger randNum ts 0

          getFirstLarger _ [t] _ = fst t
          getFirstLarger n (t:ts) sum
            | (1 / (snd t)) + sum > n   = fst t
            | otherwise = getFirstLarger n ts (sum + 1 / (snd t))  

-- Returns wanted number of parents.
chooseParents :: [(a, Value)] -> Int -> StdGen -> ([a], StdGen)
chooseParents ts n gen = doWithRand (chooseParent ts) n gen

-- Do mutations.
doMutations :: (Taughtable a) => Double -> Value -> [a] -> StdGen -> ([a], StdGen)
doMutations p k ts gen = doWithRandLs (map (mutate p k) ts) gen

getElite :: (Taughtable a) => [(a, Value)] -> a
getElite fitList = fst $ minimumBy (\t1 t2 -> compare (snd t1) (snd t2)) fitList

getFitList :: (Taughtable a) => Population a -> DataSet -> [(a, Value)]
getFitList pop dataSet = map (\t -> (t, fitness t dataSet)) (ts pop)

-- Do crossing and mutating
getNextGeneration :: (Taughtable a) => Population a -> DataSet -> Double -> Value -> StdGen -> (Population a, a, StdGen)
getNextGeneration pop dataSet p k gen = (pop', realElite, gen''')
    where 
          pop' = Population  (elite : mutatedChildren)
          realElite = getElite $ getFitList pop' dataSet
          
          elite = fst $ minimumBy (\t1 t2 -> compare (snd t1) (snd t2)) fitList
          (mothers, gen')  = chooseParents fitList (length fitList - 1) gen
          (fathers, gen'') = chooseParents fitList (length fitList - 1) gen'
          children = zipWith cross mothers fathers

          (mutatedChildren, gen''') = doMutations p k children gen''

          fitList = map (\t -> (t, fitness t dataSet)) (ts pop)


train :: (Taughtable a) => Population a -> DataSet -> Double -> Value -> Int -> Value -> StdGen -> ([Value], a, StdGen)
train pop dataSet p k n minFitness gen
    | n == 1                    = ([currFitness], elite, gen')
    | currFitness <= minFitness = ([currFitness], elite, gen')
    | otherwise                 = ((currFitness : resultsTrain), eliteTrain, genTrain)

    where (nextGenPop, elite, gen') = getNextGeneration pop dataSet p k gen
          (resultsTrain, eliteTrain, genTrain) = train nextGenPop dataSet p k (n - 1) minFitness gen'  
          currFitness = fitness elite dataSet

-- Testing -----------------------------------------------------------------------------

-- Instantiate network

-- Input layer
inLayerP :: Perceptron
inLayerP = Perceptron [2, 1] id 

-- Hidden layer
e :: Double
e = 2.71828

a :: Double
a = 1

sigmoidalF :: Double -> Double
sigmoidalF net = 1 / (1 + e**(a * net))

midLayerP :: Perceptron
midLayerP = Perceptron [3, 1] sigmoidalF

-- Outer layer
outLayerP :: Perceptron
outLayerP = Perceptron [4, 1, 1, 1, 1] id

-- Sample neural network.
sampleNN = NeuralNetwork inLayerP (replicate 4 midLayerP) outLayerP

-- Sample population.
samplePop = Population (replicate 50 sampleNN)

doWithRand :: (StdGen -> (b, StdGen)) -> Int -> StdGen -> ([b], StdGen)
doWithRand _ 0 gen = ([], gen)
doWithRand f n gen = ((val : vals), gen'')
    where (val, gen') = f gen
          (vals, gen'') = doWithRand f (n - 1) gen'

doWithRandLs :: [StdGen -> (b, StdGen)] -> StdGen -> ([b], StdGen)
doWithRandLs [] gen = ([], gen)
doWithRandLs (f:fs) gen = ((val : vals), gen'')
    where (val, gen')   = f gen
          (vals, gen'') = doWithRandLs fs gen'

-- Create random Perceptron
createRandP :: Int -> (Value -> Value) -> StdGen -> (Perceptron, StdGen)
createRandP n f gen = ((Perceptron randNums f), gen')
    where (randNums, gen') = doWithRand (randomR (-1, 1)) n gen
          
-- Create random Neural network
createRandNN :: StdGen -> (NeuralNetwork, StdGen)
createRandNN gen = (NeuralNetwork inL hidL outL, gen3)
    where (inL, gen1) =  createRandP 2 id gen
          (hidL, gen2) = doWithRand (createRandP 2 sigmoidalF) 4 gen1
          (outL, gen3) = createRandP 5 id gen2

getNums :: StdGen -> [Double]
getNums gen = fst $ doWithRand random 5 gen


-- Read from file ----
-- 
parseValue :: String -> (Double, Double)
parseValue line = (read $ head nums, read $ last nums)
    where nums = words line

readValues :: Handle -> [(Double, Double)] -> IO [(Double, Double)]
readValues h vals = 
    do inEOF <- hIsEOF h
       if inEOF
        then return vals
        else do line <- hGetLine h
                let val = parseValue line
                readValues h (val : vals)

main :: IO()
main = do
    -- Read learning set from file
    learningSetFile <- openFile "train.txt" ReadMode
    lSet <- readValues learningSetFile []
    
    putStrLn $ show lSet

    randGen <- getStdGen

    
    let (p, k) = (0.3, 0.8)    

    let (randomNNs, randGen') = doWithRand (createRandNN) 50 randGen 
    let initPop = Population randomNNs
    -- putStrLn $ show initPop

    let listaFit = getFitList initPop lSet
    putStr "lista Fitova: "
    putStrLn $ printList $ map (snd) listaFit

    let elita = getElite listaFit
    putStr "elita: "
    putStrLn $ printf ("%.4f") $ fitness elita lSet
    
    let (mothers, randGen1) = chooseParents listaFit (length listaFit - 1) randGen'
    let (fathers, randGen2) = chooseParents listaFit (length listaFit - 1) randGen1

    let mVals = map (\m -> fitness m lSet) mothers
    putStr "mothers:   "
    putStrLn $ printList mVals

    let fVals = map (\m -> fitness m lSet) fathers
    putStr "fathers:   "
    putStrLn $ printList fVals

    let children = zipWith cross mothers fathers
    let cVals = map (\m -> fitness m lSet) children
    putStr "children:  "
    putStrLn $ printList (cVals)

    let (mutatedChildren, randGen3) = doMutations p k children randGen2
    let mcVals = map (\m -> fitness m lSet) mutatedChildren
    putStr "mchildren: "
    putStrLn $ printList (mcVals)

    putStrLn "\n\n"
     
    let (nextPop, elite1, randGen1) = getNextGeneration initPop lSet p k randGen'
    putStrLn $ "fitness: "  ++ ( show $ fitness elite1 lSet)
    putStrLn $ printList $ map (snd) $ getFitList nextPop lSet

    let (nextPop2, elite2, randGen2) = getNextGeneration nextPop lSet p k randGen1
    putStrLn $ "fitness: "  ++ ( show $ fitness elite2 lSet)
    putStrLn $ printList $ map (snd) $ getFitList nextPop2 lSet
    
    let (nextPop3, elite3, randGen3) = getNextGeneration nextPop2 lSet p k randGen2
    putStrLn $ "fitness: "  ++ ( show $ fitness elite3 lSet)
    putStrLn $ printList $ map (snd) $ getFitList nextPop3 lSet

    let (nextPop4, elite4, randGen4) = getNextGeneration nextPop3 lSet p k randGen3
    putStrLn $ "fitness: "  ++ ( show $ fitness elite4 lSet)
    putStrLn $ printList $ map (snd) $ getFitList nextPop4 lSet
    
    let (nextPop5, elite5, randGen5) = getNextGeneration nextPop4 lSet p k randGen4
    putStrLn $ "fitness: "  ++ ( show $ fitness elite5 lSet)
    putStrLn $ printList $ map (snd) $ getFitList nextPop5 lSet

    let (nextPop6, elite6, randGen6) = getNextGeneration nextPop5 lSet p k randGen5
    putStrLn $ "fitness: "  ++ ( show $ fitness elite6 lSet) 
    putStrLn $ printList $ map (snd) $ getFitList nextPop6 lSet
    

    
    let (results, elite, randGen'') = train initPop lSet p k 200 0.01 randGen'

    --putStrLn $ show elite
    
    putStrLn $ "Broj iteracija: " ++ (show $ length results)
    putStrLn $ printList results

    putStrLn "gotov"
