import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Ord

-- Reverse a string of digits and parse to an integer
parseDigits digits = read (reverse digits) :: Integer

-- Input string -> digits -> parsed integer
parseDateInternal :: String -> String -> Integer
parseDateInternal [] digits = parseDigits digits
parseDateInternal (x:xs) digits = if x == ']'
    then parseDigits digits
    else if Char.isDigit x
        then parseDateInternal xs (x:digits)
        else parseDateInternal xs digits

parseDate :: String -> Integer
parseDate s = parseDateInternal s []

data Action = NewGuard Integer | Sleep | Wake deriving Show

parseAction :: String -> Action
parseAction s | Just rest <- List.stripPrefix "Guard #" s = NewGuard (parseDate rest) -- ID is not really a date, but will work
parseAction "wakes up" = Wake
parseAction "falls asleep" = Sleep

parseRecord :: String -> (Integer, Action)
parseRecord record = (parseDate record, parseAction (drop 19 record))

getOrDefault :: Ord k => k -> a -> Map.Map k a -> a
getOrDefault key def m = case Map.lookup key m of
    Just val -> val
    Nothing -> def

-- Process wake/sleep records in the context of a guard. Guard ID -> Action list -> Map from guard ID to wake/sleep periods
processForGuard :: Integer -> [(Integer, Action)] -> Map.Map Integer [(Integer, Integer)] -> Map.Map Integer [(Integer, Integer)]
processForGuard id [] m = m
processForGuard id ((_,NewGuard newId):rest) m = processForGuard newId rest m
processForGuard id ((start,Sleep):(end,Wake):rest) m =
    let existing = getOrDefault id [] m
        newMap = Map.insert id ((start,end):existing) m in
    processForGuard id rest newMap

-- Take list of sorted records. Generate map from guard ID to list of sleeping times tuples of start and end timestamps
makeSleepTimes :: [(Integer, Action)] -> Map.Map Integer [(Integer, Integer)]
makeSleepTimes actions = processForGuard (-1) actions Map.empty -- Pass invalid guard ID. first record should be NewGuard

totalSleepTime :: [(Integer, Integer)] -> Integer
totalSleepTime periods = List.foldl (\acc p -> acc + ((snd p) - (fst p))) 0 periods

-- Last 4 digits of timestamp are time within day
minsInDay = 10000 -- 24 * 60

-- Make range of minutes within day [start, end) from period
periodToRange p = [((fst p) `mod` minsInDay)..(((snd p) `mod` minsInDay) - 1)]

addPeriodToMap :: Map.Map Integer Integer -> (Integer, Integer) -> Map.Map Integer Integer
addPeriodToMap m p = List.foldl (\m' min -> let curr = getOrDefault min 0 m in Map.insert min (curr + 1) m') m (periodToRange p)

-- From sleep periods, make map from minute to number of times asleep
makeMinuteMap :: [(Integer, Integer)] -> Map.Map Integer Integer
makeMinuteMap = List.foldl addPeriodToMap Map.empty

main = do
    contents <- getContents
    let l = lines contents
        entries = map parseRecord l
        sorted = List.sortOn fst entries
        sleepTimes = makeSleepTimes sorted
        minuteMaps = Map.map makeMinuteMap sleepTimes
        sleepiestMinutes = Map.map (\minuteMap -> List.maximumBy (Data.Ord.comparing snd) (Map.toList minuteMap)) minuteMaps
        mostMinutesGuard = List.maximumBy (comparing (snd.snd)) (Map.toList sleepiestMinutes)
    print ((fst mostMinutesGuard) * ((fst.snd) mostMinutesGuard))
