-- Matija Sosic
-- 0036452499
-- Umjetna inteligencija - 1. laboratorijska vjezba

module Astar(astarSearch, ExpandState(..)) where

import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.PSQueue as PS
import qualified Data.MultiSet as MS
import qualified Data.Map as Map

-- Podatkovna struktura za cvor
-- Omotac za stanje u stablu pretrazivanja
data Node a = Node {
    state  :: a,
    parent :: Maybe (Node a),
    f      :: Int,
    cost   :: Int } deriving (Eq, Show)

-- Cvorovi se mogu medusobno usporedivati, da se mogu sortirati u PQ
instance (Eq a, ExpandState a) => Ord (Node a) where
    n1 <= n2 = (cost n1 + (f n1)) <= (cost n2 + (f n2)) 

-- Cvor zna dohvatiti susjedna stanja i cijene do njih
class ExpandState a where
    actions   :: a -> [a -> a]  -- Uzima stanje, vraca niz funkcija koje ga prevode u susjedno
    pathCost  :: [a] -> Int    -- Cijena puta za niz stanja
    stepCost  :: a -> a -> Int -- Cijena pojedinog prijelaza

    stepCost _ _ = 1
    pathCost sts = sum $ zipWith stepCost (init sts) (tail sts) -- Je li ok, treba li obratno?

-- Pretrazivanje Astar
-- @param a - pocetno stanje
-- @param (a -> Bool) - funkcija koja provjerava je li stanje ciljno
-- @return Maybe[a] - Nothing ako nema rjesenja, inace lista stanja od pocetnog do ciljnog stanja
astarSearch :: (Show a, Eq a, ExpandState a, Ord a) => a -> (a -> Bool) -> (a -> Int) -> Maybe ([a], Int, Int)
astarSearch initState isGoal heuristic = helper initFrontier Map.empty 0
    where -- Pocetna fronta sadrzi samo pocetno stanje
        initFrontier = PS.singleton initState (Node initState Nothing (heuristic initState) 0)
            
        -- Repno rekurzivna pomocna funkcija
        helper front visited expd
            | PS.null front     = Nothing   -- Ako je fronta prazna, nema rjesenja
            | otherwise         = if (isGoal currState) then
                                    Just ((reverse $ retPath currState p), expd, c)
                                  else
                                    helper front'' visited' (expd + 1)
            where
                -- Izvuci stanje s minimalnom cijenom iz fronte
                -- Izvuci novu frontu bez min stanja
                Just (currState PS.:-> currNode@(Node _ p _ c), front') = PS.minView front
                
                -- Dodaj min stanje u listu posjecenih
                visited' = Map.insert currState c visited

                -- Expandaj cvor
                nodesToAdd = filter (not . isBetterInVisited visited') $ filter (not . isBetterInFront front') $ expandNode currNode heuristic

                -- Ubaci stanja koja treba (nova i updatana)
                front'' = foldl (\q node -> PS.insert (state node) node q) front' nodesToAdd -- Treba izbaciti stare, PSQ pazi na to

                -- Rekonstrukcija puta
                retPath _ Nothing      = [initState]
                retPath s (Just p) = s : retPath (state p) (parent p)

isBetterInFront :: (Ord a, ExpandState a) => PS.PSQ a (Node a) -> Node a -> Bool
isBetterInFront front node@(Node s _ _ c)
    | fromPQ == Nothing = False
    | otherwise         = cost (fromJust fromPQ) <= c 
    where fromPQ = PS.lookup s front

isBetterInVisited :: (Ord a, ExpandState a) => Map.Map a Int -> Node a -> Bool     
isBetterInVisited vis node@(Node s _ _ c)
    | fromVis == Nothing = False
    | otherwise          = fromJust fromVis <= c
    where fromVis = Map.lookup s vis

expandNode :: ExpandState a => Node a -> (a -> Int) -> [Node a]
expandNode node@(Node state _ _ cost) heuristic = children
    where nextStates = map (\f -> f state) $ actions state
          children   = map (\s -> (Node s (Just node) (heuristic s) (cost + stepCost state s))) $ nextStates
