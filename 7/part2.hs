import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Char
import Data.Ord

type DepMap = Map.Map Char (Set.Set Char)
type TimeMap = Map.Map Int (Set.Set Char)

parseDigits digits = read (reverse digits) :: Int

getOrDefault :: Ord k => k -> a -> Map.Map k a -> a
getOrDefault key def m = case Map.lookup key m of
    Just val -> val
    Nothing -> def

addToValSet :: (Ord k, Ord a) => k -> a -> Map.Map k (Set.Set a) -> Map.Map k (Set.Set a)
addToValSet key val m = let exist = getOrDefault key Set.empty m in
    Map.insert key (Set.insert val exist) m

addRequire :: Char -> Char -> DepMap -> DepMap
addRequire before after reqs = addToValSet after before reqs

--notRequired :: Char -> DepMap -> Bool
--notRequired node reqs = Map.null $ Map.filter (Set.member node) reqs

unfettered :: DepMap -> Char -> Bool
unfettered reqs node = Set.null $ getOrDefault node Set.empty reqs

removeFromReqs :: DepMap -> Char -> DepMap
removeFromReqs reqs node = Map.map (Set.delete node) reqs

activateFirstUnfettered :: Set.Set Char -> Set.Set Char -> TimeMap -> Int -> (Set.Set Char, TimeMap)
activateFirstUnfettered canRemove pending active currTime =
    let node = Set.elemAt 0 canRemove
        newNodes = Set.delete node pending
        endTime = currTime + 61 + ((ord node) - (ord 'A'))
        --endTime = currTime + 1 + ((ord node) - (ord 'A'))
        newActive = addToValSet endTime node active in
    (newNodes, newActive)

completeFirstActive :: DepMap -> TimeMap -> (DepMap, TimeMap, [Char], Int)
completeFirstActive reqs active =
    let (endTime, completedSet) = Map.elemAt 0 active
        newActive = Map.delete endTime active
        completedList = Set.toList completedSet
        newReqs = Set.foldl removeFromReqs reqs completedSet in
    (newReqs, newActive, completedList, endTime)

processDependencies :: DepMap -> Set.Set Char -> TimeMap -> Int -> [Char] -> ([Char], Int)
processDependencies reqs pending active currTime processed = 
    if (Set.null pending) && (Map.null active) then (processed, currTime)
    else
        let canRemove = Set.filter (unfettered reqs) pending in
        if length active < 5 && not (null canRemove) then
        --if length active < 2 && not (null canRemove) then
            let (nextPending, nextActive) = activateFirstUnfettered canRemove pending active currTime in
            processDependencies reqs nextPending nextActive currTime processed
        else 
            let (nextReqs, nextActive, completed, nextCurrTime) = completeFirstActive reqs active in
            processDependencies nextReqs pending nextActive nextCurrTime (processed ++ completed)

main = do
    contents <- getContents
    let l = lines contents
        depList = map (\line -> (line !! 5,line !! 36)) l
        reqs = List.foldl (\m (before, after) -> addRequire before after m) Map.empty depList
        nodes = List.foldl (\s x -> Set.insert x s) Set.empty $ depList >>= \(a, b) -> [a, b]
        (answer, endTime) = processDependencies reqs nodes Map.empty 0 []
    print $ reqs
    print nodes
    print answer
    print endTime
