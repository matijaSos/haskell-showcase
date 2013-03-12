-- Matija Sosic
-- 0036452499
-- Umjetna inteligencija - 2. laboratorijska vjezba

import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Control.Monad

-- Data structures -----------------------------------------------------------------

data FormulaTree = AND FormulaTree FormulaTree
                 | OR  FormulaTree FormulaTree 
                 | IMP FormulaTree FormulaTree
                 | EQI FormulaTree FormulaTree
                 | NOT FormulaTree
                 | Atom Char                    -- Tree leaf                                 
                 deriving (Eq)

instance Show FormulaTree where
    show (AND left right) = "(" ++ show left ++ " & " ++ show right ++ ")"
    show (OR left right)  = "(" ++ show left ++ " | " ++ show right ++ ")"
    show (IMP left right) = "(" ++ show left ++ " -> " ++ show right ++ ")"
    show (EQI left right) = "(" ++ show left ++ " <-> " ++ show right ++ ")"
    show (NOT right)      = "(~" ++ show right ++ ")"
    show (Atom c)         = [c]

    
-- Formula parsing ------------------------------------------------------------------
--
-- Braces around single atom are not allowed, e.g. (A)
-- Braces around main formula not allowed, e.g. (P -> Q)    

badFormula = error "Formula not valid!"

-- Takes a string containing formula and constructs a tree holding it
parseFormula :: String -> FormulaTree
parseFormula []     = badFormula
parseFormula (c:[]) = Atom c 
parseFormula s
    | op == '&' = AND (parseFormula left) (parseFormula right)
    | op == '|' = OR  (parseFormula left) (parseFormula right)
    | op == '-' = IMP (parseFormula left) (parseFormula right)
    | op == '<' = EQI (parseFormula left) (parseFormula right)
    | op == '~' = NOT (parseFormula right)
    | otherwise = error "Non existing operator"

    where (op, l, r)    = divideByOp $ rmBrackets s
          (left, right) = (trim l, trim r) 

-- Divides formula by most outer operator
-- Returns (operator, leftPart, rightPart)
divideByOp :: String -> (Char, String, String)
divideByOp s  = rec 0 "" s
    where  rec _ _ (x:[]) = badFormula
           rec level left (x:xs)
            | x == '('                             = rec (level+1) (x:left) xs
            | x == ')'                             = rec (level-1) (x:left) xs
            | level == 0 && x `elem` ['&' , '|','~'] = (x, reverse left, xs)
            | level == 0 && x == '-'               = (x, reverse left, drop 1 xs)
            | level == 0 && x == '<'               = (x, reverse left, drop 2 xs)
            | otherwise                            = rec level (x:left) xs

-- Removes leading and trailing spaces
trim :: String -> String
trim = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')

-- Removes outer brackets
rmBrackets :: String -> String
rmBrackets s
    | first /= '(' || end /= ')' = s   -- Don't remove brackets if they aren't there
    | canRmBracks 0 rm            = rm
    | otherwise                   = s
    where  
          canRmBracks lvl [] = lvl == 0
          canRmBracks lvl (x:xs)
            | lvl < 0   = False
            | x == '('  = canRmBracks (lvl+1) xs 
            | x == ')'  = canRmBracks (lvl-1) xs
            | otherwise = canRmBracks lvl xs
          rm = reverse $ drop 1 $ reverse $ drop 1 s    -- Remove brackets
          first = head s
          end   = last s

-- Converting formula tree to CNF (still tree) -------------------------------------------------------

-- Applies given function further to nodes of the tree
pushDown :: (FormulaTree -> FormulaTree) -> FormulaTree -> FormulaTree
pushDown _ (Atom c)         = Atom c
pushDown f (AND left right) = (AND (f left) (f right))
pushDown f (OR left right)  = (OR (f left) (f right))
pushDown f (IMP left right) = (IMP (f left) (f right))
pushDown f (EQI left right) = (EQI (f left) (f right))
pushDown f (NOT right)      = (NOT (f right))

-- Step 1 & 2
-- Remove implication
-- Remove equivalence
rmImpEq :: FormulaTree -> FormulaTree
rmImpEq (IMP left right) = rmImpEq $ OR (NOT left) right
rmImpEq (EQI left right) = rmImpEq $ AND (OR (NOT left) right) (OR (NOT right) left)

rmImpEq t = pushDown rmImpEq t -- Default case, just push deeper

-- Step 3
-- Move NOT closer to atom
notToAtom :: FormulaTree -> FormulaTree
notToAtom (NOT (NOT right))      = notToAtom right -- Invocation
notToAtom (NOT (OR left right))  = applyFac $ notToAtom (AND (NOT left) (NOT right))
notToAtom (NOT (AND left right)) = applyFac $ notToAtom (OR (NOT left) (NOT right))

notToAtom t = applyFac $ pushDown notToAtom t -- Default case

-- Factorization
-- Removes only matching Atoms so far
applyFac :: FormulaTree -> FormulaTree
applyFac t@(AND (Atom c) (Atom d)) -- G & G
    | c == d    = Atom c
    | otherwise = t

applyFac t@(OR (Atom c) (Atom d))  -- G | G
    | c == d    = Atom c
    | otherwise = t

applyFac t = t

-- Step 4
-- Apply distributivity
applyDsbOnce :: FormulaTree -> FormulaTree
applyDsbOnce (OR a (AND b c)) = applyDsb $ AND (applyFac $ OR a b) (applyFac $ OR a c)
applyDsbOnce (OR (AND b c) a) = applyDsb $ AND (applyFac $ OR a b) (applyFac $ OR a c)
applyDsbOnce t = t

applyDsb :: FormulaTree -> FormulaTree
applyDsb = applyDsbOnce . pushDown applyDsb

-- cnfTree function
-- Converts tree to CNF
treeToCNF :: FormulaTree -> FormulaTree
treeToCNF = applyDsb . notToAtom . rmImpEq 

-- Convert CNF to the set of clauses ---------------------------------------------------------

data Literal = Tru Char | Neg Char deriving (Ord, Eq)

instance Show Literal where
    show (Tru c) = [c]
    show (Neg c) = "~" ++ [c]

data Clause  = Clause {
    idx     :: Int,                     -- Redni broj
    litSet  :: (S.Set Literal),         -- Skup literala
    parents :: Maybe(Clause, Clause)    -- Roditelji | Nothing
    } 
             | NIL {
    parents :: Maybe(Clause, Clause)
    }

instance Show Clause where
    show (NIL _) = "NIL"
    show c       = "(" ++ show (idx c) ++ " " ++ show (litSet c) ++ ")"

instance Eq Clause where
    (NIL _) == (NIL _) = True
    (NIL _) == _       = False
    _       == (NIL _) = False
    c1      == c2      = litSet c1 == litSet c2   -- Clauses are equal if their literal sets are equal

instance Ord Clause where
    (NIL _) <= _ = False
    _ <= (NIL _) = True
    c1 <= c2 = litSet c1 <= litSet c2 

-- Takes tree in CNF
-- Returns set of clauses
cnfToClauses :: FormulaTree -> S.Set Clause 
cnfToClauses (AND left right) = S.union (cnfToClauses left) (cnfToClauses right) 

cnfToClauses (OR left right)  = S.singleton setOfLiterals
    where setOfLiterals = Clause 0 (S.union (collect left) (collect right)) Nothing
          collect (NOT (Atom c))  = S.singleton (Neg c)
          collect (Atom c)        = S.singleton (Tru c)
          collect (OR left right) = S.union (collect left) (collect right)
          collect (AND left right) = error $ "naisao na end : " ++ show left ++ "   " ++ show right 

cnfToClauses (NOT (Atom c)) = S.singleton singleLiteral
    where singleLiteral = Clause 0 (S.singleton (Neg c)) Nothing

cnfToClauses (Atom c) = S.singleton singleLiteral
    where singleLiteral = Clause 0 (S.singleton (Tru c)) Nothing

-- Enumerates clauses in set
-- Int -> starting number
enumClauses :: Int -> S.Set Clause -> S.Set Clause 
enumClauses count = fst . S.fold addIdx (S.empty, count)
    where addIdx clause (currSet, count) = (S.insert (clause {idx = count}) currSet, count+1)

-- Main function
-- Takes formula, turns it to CNF
-- Returns set of clauses
cnfConvert :: FormulaTree -> S.Set Clause
cnfConvert = cnfToClauses . treeToCNF

-- Proving theorem ---------------------------------------------------------------------------------

-- Takes two literals
-- Returns True if first = NOT second
isNegated :: Literal -> Literal -> Bool
isNegated (Tru c1) (Neg c2) = c1 == c2
isNegated (Neg c1) (Tru c2) = c1 == c2
isNegated l1 l2             = False

-- Takes two clauses
-- Returns True if NIL is deducted
deductedNIL :: Clause -> Clause -> Bool
deductedNIL (Clause _ set1 _) (Clause _ set2 _)
    | (S.size set1 == 1) && (S.size set2 == 1) = lit1 `isNegated` lit2
    | otherwise = False
    where lit1 = S.findMin set1
          lit2 = S.findMin set2

-- Checks if set contains NIL clause
-- If it does, set will always contain only one element
hasNIL :: S.Set Clause -> Bool
hasNIL = S.member (NIL Nothing) -- Doesn't look at parents when comparing, so I put nothing

-- Takes two clauses
-- Returns set of new deducted clauses
plResolve :: Clause -> Clause -> S.Set Clause
plResolve c1@(Clause _ set1 _) c2@(Clause _ set2 _)
    | deductedNIL c1 c2 = S.singleton $ NIL (Just(c1,c2)) -- Return NIL if deducted
    | otherwise         = S.fold check S.empty set1       -- Otherwise apply resolution
    where check lit1 newClauses
            | (S.member (negLit lit1) set2) = S.insert newClause newClauses
            | otherwise                     = newClauses
            where newLitSet = S.union (S.delete lit1 set1) (S.delete (negLit lit1) set2) 
                  newClause = Clause 0 newLitSet (Just(c1, c2))


-- Parameters:
--  set of clauses to update
--  set of clauses to add
--  queue to update
-- Returns:
--  (updated set of clauses, updated queue)
updateSetAndQ :: S.Set Clause -> S.Set Clause -> Q.Seq(Clause, Clause) -> (S.Set Clause, Q.Seq(Clause, Clause))
updateSetAndQ setOld setToAdd qOld = S.fold update (setOld, qOld) setToAdd  
    where update clause (currSet, currQ) = (newSet, newQ)

            where newSet = S.insert clause currSet
                  newQ   = S.fold (\c q -> q Q.|> (c, clause)) currQ currSet

-- Simplification ------------------------------------------------------------------------------------

-- Checks if first clause is subset of second
isProperSubsetOf :: Clause -> Clause -> Bool
isProperSubsetOf (Clause _ set1 _) (Clause _ set2 _) = set1 `S.isProperSubsetOf` set2

-- Parameters:
--  allC (all clauses)      - newly constructed set in which I check for overlaps 
--  dedC (deducted clauses) - possible overlappers
-- Returns:
--  first set in simplified form (longer overlapping clause is deleted, and shorter added)
-- Precondition:
--  allC is already simplified
simplify :: S.Set Clause -> S.Set Clause -> S.Set Clause
simplify allC dedC = S.fold eachDedC allC dedC
    where eachDedC clauseDed currAll = S.fold eachAllC currAll currAll

            where eachAllC clauseAll currAll'
                    | clauseDed `isProperSubsetOf` clauseAll = S.delete clauseAll currAll' 
                    | clauseAll `isProperSubsetOf` clauseDed = S.delete clauseDed currAll'
                    | otherwise                              = currAll'
                  

-- Tautology check ------------------------------------------------------------------------------------

-- Checks if given clause is a tautology
-- Clause is tautology if it contains negated pair of literals
negLit :: Literal -> Literal
negLit (Tru c) = Neg c
negLit (Neg c) = Tru c

isTautology :: Clause -> Bool
isTautology (NIL _) = False
isTautology (Clause _ set _) = S.fold findNeg False set
    where findNeg lit sol
            | S.member (negLit lit) set = True
            | otherwise                 = sol


literali = S.fromList [Tru 'a', Tru 'b', Neg 'a']
taut     = Clause 0 literali Nothing

-- Main function ----------------------------------------------------------------------------------------

-- Returns how process of deduction went
-- Triples (parent, parent, child)
backtrack :: Clause -> S.Set Clause
backtrack c@(NIL Nothing) = S.singleton c
backtrack c@(Clause _ _ Nothing) = S.singleton c
backtrack c = S.insert c (backtrack p1 `S.union` backtrack p2)
    where (p1, p2) = fromJust $ parents c

-- arg1 - F set of clauses
-- arg2 - (not G) set of clauses
-- Returns True if F -> G
plResolution :: S.Set Clause -> S.Set Clause -> Int -> Int -> Bool -> Maybe(S.Set Clause, Int, Int, S.Set Clause, S.Set Clause)
plResolution f ng stepLimit sizeLimit simpleMode = helper initClauseSet' initQ initCounter 1 (S.size initClauseSet')
          -- Setting initial conditions
    where f'  = S.filter (not . isTautology) f
          f'' = simplify f' f'
          fInit
            | simpleMode = f''
            | otherwise = f

          ng'  = S.filter (not . isTautology) $ S.difference ng f''
          ng'' = simplify ng' ng'
          ngInit
            | simpleMode = ng''
            | otherwise  = S.difference ng fInit

          (initClauseSet, initQ) = updateSetAndQ fInit ngInit Q.empty   
          initClauseSet'
            | simpleMode = simplify initClauseSet ng''
            | otherwise  = initClauseSet 

          initCounter = S.size f + S.size ng
          -- Function that does step
          helper clauseSet q counter stepNum maxSize
            | stepNum > stepLimit = error $ "Step limit reached " ++ show stepNum
            | maxSize > sizeLimit = error $ "Size limit reached " ++ show maxSize
            | Q.null q          = Nothing
            | hasNIL newClauses = Just (backtrack $ S.findMax newClauses, stepNum, maxSize, fInit, ngInit)
            | otherwise         = helper clauseSet'' q'' counter' stepNum' maxSize' 
            where
                -- Take new clause pair from queue
                -- Check if job is still valid
                (c1, c2) Q.:< q' = Q.viewl q 
                validJob = S.member c1 clauseSet && S.member c2 clauseSet
                
                -- Get deducted clauses from this pair
                -- Take only those which are not in clauseSet already
                -- Remove tautologies
                newClauses
                    | validJob && simpleMode       = S.filter (not . isTautology) $ S.difference (plResolve c1 c2) clauseSet
                    | validJob && (not simpleMode) = S.difference (plResolve c1 c2) clauseSet
                    | otherwise                    = S.empty

                -- Update set and q with new clauses
                -- Simplify newly acquired set of clauses
                (clauseSet', q'') = updateSetAndQ clauseSet (enumClauses counter newClauses) q'
                clauseSet''
                    | simpleMode = simplify clauseSet' newClauses
                    | otherwise  = clauseSet'

                -- Update counter stepNum and maxSize
                counter' = counter + S.size newClauses
                maxSize' = max maxSize (S.size clauseSet'')
                stepNum'
                    | validJob  = stepNum + 1
                    | otherwise = stepNum

-- Converts input to clauses
-- Class plResolution
negateFormula :: FormulaTree -> FormulaTree
negateFormula f = NOT f

plResolutionWrapper :: [String] -> String -> Int -> Int -> Bool -> Maybe(S.Set Clause, Int, Int, S.Set Clause, S.Set Clause)
plResolutionWrapper fsStr gStr simpleMode = plResolution f ng simpleMode
    where f  = enumClauses 0 $ S.unions $ map (cnfConvert . parseFormula) fsStr        -- Translate from strings to set of clauses 
          ng = enumClauses (S.size f) $ cnfConvert $ negateFormula $ parseFormula gStr -- From string to negated clause


-- Output -----------------------------------------------------------------------------------------            

showLitSet :: S.Set Literal -> String
showLitSet = dropLast3 . S.fold (\l s -> s ++ show l ++ " v ") ""
    where dropLast3 = init . init . init

showClauseSet :: S.Set Clause -> String
showClauseSet = concatMap showClause . sortBy sortaj . S.toList
    where showClause (Clause num lits (Just(p1,p2))) = 
            show num ++ ". " ++ showLitSet lits ++ "\t(" ++ (show $ idx p1) ++ ", " ++ (show $ idx p2) ++ ")\n"

          showClause (Clause num lits Nothing) = 
            show num ++ ". " ++ showLitSet lits ++ "\t(-, -)\n"
          
          showClause (NIL (Just(p1,p2))) =
            "   NIL \t(" ++ (show $ (idx p1)) ++ ", " ++ (show $ (idx p2)) ++ ")\n"

          sortaj (Clause num1 _ _) (Clause num2 _ _) = compare num1 num2
          sortaj c1 c2 = compare c1 c2

fp = ["(((~P) & Q) <-> (R|S)) | ((~P) -> S)"]
gp = "(((~S) & R) -> (Q & P)) | ((P & R) | Q)"

-- Main --------------------------------------------------------------------------------------------

main :: IO()
main = do
    putStrLn "Enter number of premises: "
    premiseNum <- getLine
    premises <- replicateM ((read premiseNum)::Int) getLine

    putStrLn "Enter the goal formula: "
    goalFormula <- getLine

    putStrLn "Enter maximum number of steps: "
    stepLimitStr <- getLine
    let stepLimit = (read stepLimitStr) :: Int

    putStrLn "Enter size limit: "
    sizeLimitStr <- getLine
    let sizeLimit = (read sizeLimitStr) :: Int

    putStrLn "Choose the strategy: 0/1 - simplification OFF/ON"
    tmp <- getLine
    let simpleMode = if tmp == "1" then True else False

    -- Call functions
    let retValue = plResolutionWrapper premises goalFormula stepLimit sizeLimit simpleMode

    if (retValue == Nothing) 
        then putStrLn "Goal formula cannot be proved!"
        else do
            let Just(path, stepNum, maxSize, f, ng) = retValue
    
            putStrLn $ "\nNumber of steps: " ++ show stepNum
            putStrLn $ "Maximum clause set size: " ++ show maxSize
            putStrLn "--------------------------"

            putStr $ showClauseSet f
            putStrLn "- - - - - - - - - - - - - -"
            putStr $ showClauseSet ng
            putStrLn "---------------------------"
            putStr $ showClauseSet $ S.filter ((/= Nothing) . parents) path

