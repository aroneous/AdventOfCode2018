import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Char
import Data.Ord

countMetadata :: [Int] -> ([Int], Int)
countMetadata (nc:nm:next) =
    let (after, childsum) = List.foldl (\(nxt, cnt) _ -> let (after, subcnt) = countMetadata nxt in (after, cnt + subcnt)) (next, 0) [1..nc]
        thissum = sum (take nm after) in
    (drop nm after, thissum + childsum)

main = do
    contents <- getContents
    let strs = Split.splitOn " " contents
        lic = map (\s -> read s :: Int) strs
        (_, metasum) = countMetadata lic
    print metasum
