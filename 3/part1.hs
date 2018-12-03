import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Char as Char

-- Reverse a string of digits and parse to an integer
parseDigits digits = read (reverse digits) :: Integer

-- Find integers in a string with arbitrary non-digit separators, and return an array of them (in reverse order)
processNumber :: String -> String -> [Integer] -> [Integer]
processNumber [] [] res = res
processNumber [] digits res = let newNum = parseDigits digits in newNum : res
processNumber (x : xs) digits res = if Char.isDigit x
    then processNumber xs (x : digits) res
    else case digits of
        [] -> processNumber xs digits res
        _ -> let newNum = parseDigits digits in processNumber xs [] (newNum : res)

-- Return array of integers corresponding to integer values in the provided string
findNumbers :: String -> [Integer]
findNumbers s = reverse (processNumber s [] [])

recordRow :: Integer -> Integer -> Integer -> Map.Map (Integer, Integer) Integer -> Map.Map (Integer, Integer) Integer
recordRow x y 0 board = board
recordRow x y w board = recordRow (x + 1) y (w - 1) (Map.insertWith (+) (x, y) 1 board)

-- Record an entry into the map.
-- Map.Map is from (x, y) coordinate to number of entries present there
recordEntry :: Integer -> Integer -> Integer -> Integer -> Map.Map (Integer, Integer) Integer -> Map.Map (Integer, Integer) Integer
recordEntry x y w 0 board = board
recordEntry x y w h board = recordEntry x (y + 1) w (h - 1) (recordRow x y w board)

countMultiples :: Map.Map (Integer, Integer) Integer -> Integer
countMultiples board = Map.foldl (\count a -> if a > 1 then count + 1 else count) 0 board

recordEntry' board entry = let [_,x,y,w,h] = entry in recordEntry x y w h board

main = do
    contents <- getContents
    let l = lines contents
        entries = map findNumbers l
        board = foldl recordEntry' Map.empty entries
        multiples = countMultiples board
    print multiples
