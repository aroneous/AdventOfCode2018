import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Char
import Data.Ord

type DepMap = Map.Map Char (Set.Set Char)

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

removeFirstUnfettered :: DepMap -> Set.Set Char -> (Char, DepMap, Set.Set Char)
removeFirstUnfettered reqs nodes =
    let canRemove = Set.filter (unfettered reqs) nodes
        node = Set.elemAt 0 canRemove
        newReqs = removeFromReqs reqs node
        newNodes = Set.delete node nodes in
    (node, newReqs, newNodes)

processDependencies :: DepMap -> Set.Set Char -> [Char] -> [Char]
processDependencies reqs nodes processed = 
    if Set.null nodes then processed
    else let (node, nextReqs, nextNodes) = removeFirstUnfettered reqs nodes in
        processDependencies nextReqs nextNodes (node : processed)

main = do
    contents <- getContents
    let l = lines contents
        depList = map (\line -> (line !! 5,line !! 36)) l
        reqs = List.foldl (\m (before, after) -> addRequire before after m) Map.empty depList
        nodes = List.foldl (\s x -> Set.insert x s) Set.empty $ depList >>= \(a, b) -> [a, b]
        answer = reverse $ processDependencies reqs nodes []
    print $ reqs
    print nodes
    print answer
