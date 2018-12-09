import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Char
import Data.Ord

-- Map from 1-based child num to child's sum
type ChildSums = Map.Map Int Int

getChild sums childNum = case Map.lookup childNum sums of
    Just sum -> sum
    Nothing -> 0

countMetadata :: [Int] -> ([Int], Int)
countMetadata (0:nm:next) =
    let thissum = sum (take nm next) in
    (drop nm next, thissum)
countMetadata (nc:nm:next) =
    let (after, childsums) =
            List.foldl (\(nxt, cs) childNum -> let (after, subcnt) = countMetadata nxt in (after, Map.insert childNum subcnt cs))
                (next, Map.empty) [1..nc]
        refs = take nm after
        thissum = sum $ map (getChild childsums) refs in
    (drop nm after, thissum)

main = do
    contents <- getContents
    let strs = Split.splitOn " " contents
        lic = map (\s -> read s :: Int) strs
        (_, metasum) = countMetadata lic
    print metasum
