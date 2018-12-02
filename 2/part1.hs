import qualified Data.List as List

hasN :: Ord a => Integer -> a -> Integer -> [a] -> Bool
hasN target currVal count [] = count == target
hasN target currVal count (val : xs) = if val == currVal
    then hasN target currVal (count + 1) xs
    else if count == target
        then True
        else hasN target val 1 xs

main = do
    contents <- getContents
    let l = lines contents
        sorted = map List.sort l
        numtwos = length (filter (hasN 2 '0' 0) sorted)
        numthrees = length (filter (hasN 3 '0' 0) sorted)
        res = numtwos * numthrees
    print res
