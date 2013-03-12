-- Matija Sosic
-- 0036452499
-- Umjetna inteligencija - 3. laboratorijska vjezba

import Data.List
import Data.List.Split
import Text.Regex.Posix
import Data.Maybe
import qualified Data.Map as M

import System.IO

-- Data structures -----------------------------------------------------

-- Structure that represents variable ------------------------
data Variable = Variable {
    name    :: String,            -- Name of the variable
    domain  :: [VarValue]         -- List of possible values
} deriving (Ord, Eq)
type VarValue = String

instance Show Variable
    where show (Variable n d) = n

--------------------------------------------------------------

-- Structure that represents rule ----------------------------
data Rule = Rule {
    ruleName :: String,
    salience :: Int,
    index    :: Int,               -- Order in list of rules
    lhs      :: [Condition],
    rhs      :: [Action]
} 
data Condition = Condition Variable [VarValue]                       -- E.g. atr1 = v1 | v2 | v3 ...
data Action    = Action Variable VarValue      deriving (Ord, Eq)   -- E.g  atr1 = v1

instance Show Action
    where show (Action var val) = name var ++ " = " ++ val 

instance Show Condition
    where show (Condition var vals) = name var ++ " == " ++ intercalate "|" vals 

instance Show Rule
    where show (Rule n s i l r) = n -- ++ "\nSalience: " ++ show s ++ "\nIndex: " ++ show i ++
                                  --"\nIF " ++ (intercalate " & " $ map (show) l) ++
                                  --" THEN " ++ (intercalate " & " $ map (show) r)

listToStr :: (Show a) => String -> [a] -> String
listToStr sep = intercalate sep . map (show)

--------------------------------------------------------------

-- Structure that represents knowledge base ------------------
data KnowledgeDB = KDB {
    factsKDB :: M.Map Variable VarValue,
    rulesKDB :: [Rule]
} deriving (Show)

getVarValueFromKDB :: Variable -> KnowledgeDB -> Maybe VarValue
getVarValueFromKDB var (KDB facts rules) = M.lookup var facts

-- Parsing ------------------------------------------------------------

trim :: String -> String
trim = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')  

parseVariable :: String -> Variable
parseVariable str = Variable varName varDomain
    where (varName : values : _) = map trim $ splitOn " = " str
          varDomain              = map trim $ splitOn " | " values

readVariables :: Handle -> [Variable] -> IO [Variable]
readVariables h vars = 
    do inEOF <- hIsEOF h
       if inEOF 
        then return vars
        else do line <- hGetLine h
                let var = parseVariable line
                readVariables h (var : vars) 

varsMapFromList :: [Variable] -> M.Map String Variable
varsMapFromList vars = M.fromList $ map (\var -> (name var, var)) vars

parseRule :: [Variable] -> String -> String -> Int -> Rule
parseRule vars header body ind = Rule name sal ind left right
          
    where (name : salStr : _) = tail $ concat (header =~ "RULE ([a-zA-Z0-9]+) SALIENCE ([0-9]+)" :: [[String]])
          sal = read salStr

          (lhs : rhs : _) = tail $ concat (body =~ "IF (.*) THEN (.*)" :: [[String]])

          left  = parseLhs lhs vars
          right = parseRhs rhs vars

parseRhs :: String -> [Variable] -> [Action]
parseRhs s vars = map (parseAction vars) actions
    where actions = map trim $ splitOn " & " s

parseAction :: [Variable] -> String -> Action
parseAction vars actStr = Action var val
    where varMap = varsMapFromList vars
          (varName : val : _) = splitOn " = " actStr
          var = fromJust $ M.lookup varName varMap

parseLhs :: String -> [Variable] -> [Condition]
parseLhs s vars = map (parseCondition vars) conditions
    where conditions = map trim $ splitOn " & " s

parseCondition :: [Variable] -> String -> Condition
parseCondition vars condStr = Condition var vals
    where varMap = varsMapFromList vars
          (varName : valsStr : _) = splitOn " = " condStr
          vals = splitOn "|" valsStr
          var = fromJust $ M.lookup varName varMap

readRules :: Handle -> [Rule] -> [Variable] -> Int -> IO [Rule]
readRules h rules vars idx = 
    do inEOF <- hIsEOF h
       if inEOF 
        then return rules
        else do line1 <- hGetLine h
                line2 <- hGetLine h

                let rule = parseRule vars line1 line2 idx
                readRules h (rule : rules) vars (idx + 1)
    
main :: IO()
main = do
       -- Read variables from file
       inh  <- openFile "varsAnimals.txt" ReadMode
       vars <- readVariables inh []
       -- putStrLn $ listToStr "\n" vars

       -- Read rules from file
       inh1  <- openFile "rulesAnimals.txt" ReadMode
       rules <- readRules inh1 [] vars 0
       -- putStrLn $ listToStr "\n" rules

       putStr "Enter the variable you would like to know about: "
       varName <- getLine

       let varMap = varsMapFromList vars

       if (M.notMember varName varMap) 
            then putStrLn "Selected variable does not exist!"
            else do let initKDB = KDB M.empty rules
                    let var = fromJust $ M.lookup varName varMap
                    (KDB finalFacts _) <- conclude var initKDB
                    if (M.member var finalFacts)
                        then putStrLn $ "Value is: " ++ show (fromJust $ M.lookup var finalFacts)
                        else putStrLn "Variable cannot be determined!" 

       hClose inh
       hClose inh1

printStatus :: KnowledgeDB -> [Rule] -> IO()
printStatus (KDB facts _) rules = do putStrLn "-- Conflicting rules: ------------------------------------"
                                     putStrLn $ listToStr "\n" rules
                                     putStrLn "\n-- Known variables: ------------------------------------"
                                     putStrLn $ listToStr "\n" $ M.toList facts
       

-- Algorithms ----------------------------------------------------------

-- Wrapper function called by main. Concludes value of the given variable.
-- @param query     Variable which value is to be discovered 
-- @param kDB       Knowledge database
-- @return          Knowledge database containing given variable
conclude :: Variable -> KnowledgeDB -> IO KnowledgeDB
conclude query kDB@(KDB facts rules)
    | M.member query facts = return kDB
    | null possibleParents = askForVar query kDB
    | otherwise            = do printStatus kDB possibleParents 
                                loopOverRules kDB possibleParents
    where
        possibleParents = getPossibleParents query kDB

askForVar :: Variable -> KnowledgeDB -> IO KnowledgeDB
askForVar var kDB = 
    do putStrLn $ "Please enter variable value for " ++ name var ++ ": " ++ listToStr " | " (domain var) 
       val <- getLine
       let notValid = val `notElem` (domain var)
       if notValid
        then do putStr "Value is not in variable's domain: " 
                putStrLn $ listToStr " | " (domain var)
                askForVar var kDB     
        else return (updateVar var val kDB)

    where updateVar var val (KDB facts rules) =  
            KDB (M.insert var val facts) rules  
        
                        
-- Gets rules that assign value to var in rhs
-- Sorts them in order of salience and index
-- @param var   Variable whose parents I am looking for
-- @retun       Rules that assign value to var in lhr
getPossibleParents :: Variable -> KnowledgeDB -> [Rule]
getPossibleParents var (KDB _ rules) = sortBy compRules $ filter hasVar rules
    where hasVar rule = any (\(Action rhsVar _) -> var == rhsVar) (rhs rule)
          compRules (Rule _ sal1 ind1 _ _) (Rule _ sal2 ind2 _ _) = if sal1 /= sal2 then compare sal1 sal2
                                                                    else compare ind1 ind2

-- Searches over given rules and tries to conclude given variable
-- Permanently updates knowledge database during this process
-- @param kDB       Knowledge database - holds current knowledge
-- @param pRules    Rules to loop over - parent rules
-- @return          Updated knowledge database
loopOverRules :: KnowledgeDB -> [Rule] -> IO KnowledgeDB
loopOverRules kDB [] = return kDB
loopOverRules kDB@(KDB facts rules) (r:rs)
    | isNotSatisfied r kDB = loopOverRules kDB rs           -- Ako nesto nije dobro, preskoci
    | null undefVars       = return $ updateKDB r kDB       -- Ako je sve izracunato i ok, nadopuni bazu i vrati
    | otherwise            = do (kDB', allGood) <- ioResult
                                if allGood
                                    then do putStrLn $ "Rule that fired: " ++ show r 
                                            return $ updateKDB r kDB'
                                    else loopOverRules kDB' rs 

    where undefVars = getUndefVars kDB r
          ioResult  = tryUndefs undefVars r kDB

-- Checks each undefined variable
-- Returns true if all are contained in rule with appropriate values
tryUndefs :: [Variable] -> Rule -> KnowledgeDB -> IO(KnowledgeDB, Bool)

tryUndefs vars rule kDB = tryUndefsInner vars rule kDB True
    where tryUndefsInner _ _ kDB False = return (kDB, False) 
          tryUndefsInner [] _ kDB allGood = return (kDB, allGood)

          tryUndefsInner (v:vs) rule kDB allGood = 
            do kDB' <- conclude v kDB
               let facts' = factsKDB kDB'
               let val    = fromJust $ M.lookup v facts'
               if (M.member v facts' && isInRuleLhs (v, val) rule)
                then tryUndefsInner vs rule kDB' allGood
                else tryUndefsInner vs rule kDB' False


-- Checks if var is assigned value val in rhs of given rule
isInRuleLhs :: (Variable, VarValue) -> Rule -> Bool
isInRuleLhs (var, val) rule = foldl' checkCondition False (lhs rule)
    where checkCondition acc (Condition condVar condVals)
            | condVar == var && val `elem` condVals = True
            | otherwise                             = acc

-- Extracts undefined variables from kDB    
getUndefVars :: KnowledgeDB -> Rule -> [Variable]
getUndefVars (KDB facts _) = map (\(Condition var _) -> var) . 
                                 filter (\(Condition var _) -> M.notMember var facts) . lhs

-- Adds right side of rule to knowledge database
updateKDB :: Rule -> KnowledgeDB -> KnowledgeDB
updateKDB rule (KDB facts rules) = KDB facts' rules 
    where facts' = foldl' (\acc (Action var val) -> M.insert var val acc) facts (rhs rule)


-- Checks if rule is not satisfied according to given knowledge base
-- @param rule  Rule to be checked
-- @param kDB   Knowledge database
-- @return      True if not satisfied, False otherwise (even if we dont know yet)
isNotSatisfied :: Rule -> KnowledgeDB -> Bool
isNotSatisfied rule (KDB facts _) = any checkCond (lhs rule)
    where
        checkCond (Condition var values) = M.member var facts && all (\val -> valInKDB /= val) values
            where valInKDB = fromJust $ M.lookup var facts
