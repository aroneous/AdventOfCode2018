import qualified Data.List as List

getSame :: Eq a => [a] -> [a] -> [a]
getSame [] [] = []
getSame (x : xs) (y : ys) = let rest = getSame xs ys in
    if x == y
        then (x : rest)
        else rest

isMatch x same = length same == (length x) - 1

testPair :: Eq a => [a] -> [a] -> Maybe [a]
testPair x y = let same = getSame x y in
    if isMatch x same
        then Just same
        else Nothing

testAllInList :: Eq a => [a] -> [[a]] -> Maybe [a]
testAllInList x [] = Nothing
testAllInList x (y : ys) = case testPair x y of
    Nothing -> testAllInList x ys
    Just answer -> Just answer

testPairs :: Eq a => [[a]] -> Maybe [a]
testPairs [] = Nothing
testPairs [x] = Nothing
testPairs (x : xs) = case testAllInList x xs of
    Nothing -> testAllInList x (tail xs)
    Just answer -> Just answer

testAll :: Eq a => [[a]] -> Maybe [a]
testAll [] = Nothing
testAll x = case testPairs x of
    Nothing -> testAll (tail x)
    Just answer -> Just answer

main = do
    contents <- getContents
    let l = lines contents
        res = case testAll l of
            Nothing -> "not found"
            Just answer -> answer
    print res
