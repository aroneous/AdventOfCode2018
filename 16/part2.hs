import qualified Data.Map as Map
import Data.Map (Map, insert, fromList, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Sequence (Seq, index)
import Data.Foldable (toList)
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import Data.Bits ((.|.),(.&.))
import Data.Char
import Data.Ord
import Debug.Trace (trace)
import Text.Regex.TDFA

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
    deriving (Show, Eq, Ord)

data Inst = Inst {
    op :: Int,
    a :: Int,
    b :: Int,
    c :: Int
} deriving Show

data Sample = Sample {
    inst :: Inst,
    before :: [Int],
    after :: [Int]
} deriving Show

-- Validation of instruction args

-- Valid register ID?
vr :: Int -> Bool
vr r = r <= 3

-- Valid (register, register) args?
validRR :: Inst -> Bool
validRR (Inst _ ar br _) = vr ar && vr br

-- Valid (register, immediate) args?
validRI :: Inst -> Bool
validRI (Inst _ ar _ _) = vr ar

-- Valid (immediate, register) args?
validIR :: Inst -> Bool
validIR (Inst _ _ br _) = vr br

-- Operations

-- Perform a 'register register' operation
dorr :: (Int -> Int -> Int) -> Inst -> [Int] -> [Int]
dorr op (Inst _ ar br cr) args =
    let sarg = S.fromList args
        ai = sarg `index` ar
        bi = sarg `index` br
        sarg' = S.update cr (op ai bi) sarg
    in toList sarg'

-- Perform a 'register immediate' operation
dori :: (Int -> Int -> Int) -> Inst -> [Int] -> [Int]
dori op (Inst _ ar bi cr) args =
    let sarg = S.fromList args
        ai = sarg `index` ar
        sarg' = S.update cr (op ai bi) sarg
    in toList sarg'

-- Perform a 'immediate register' operation
doir :: (Int -> Int -> Int) -> Inst -> [Int] -> [Int]
doir op (Inst _ ai br cr) args =
    let sarg = S.fromList args
        bi = sarg `index` br
        sarg' = S.update cr (op ai bi) sarg
    in toList sarg'

addr :: Inst -> [Int] -> [Int]
addr = dorr (+)
addi :: Inst -> [Int] -> [Int]
addi = dori (+)
mulr :: Inst -> [Int] -> [Int]
mulr = dorr (*)
muli :: Inst -> [Int] -> [Int]
muli = dori (*)
banr :: Inst -> [Int] -> [Int]
banr = dorr (.&.)
bani :: Inst -> [Int] -> [Int]
bani = dori (.&.)
borr :: Inst -> [Int] -> [Int]
borr = dorr (.|.)
bori :: Inst -> [Int] -> [Int]
bori = dori (.|.)
setr :: Inst -> [Int] -> [Int]
setr = dori (\a _ -> a)

seti :: Inst -> [Int] -> [Int]
seti (Inst _ ai _ cr) args =
    let sarg = S.fromList args
        sarg' = S.update cr ai sarg
    in toList sarg'

gtOp :: Ord a => a -> a -> Int
gtOp a b = if a > b then 1 else 0

gtir :: Inst -> [Int] -> [Int]
gtir = doir gtOp
gtri :: Inst -> [Int] -> [Int]
gtri = dori gtOp
gtrr :: Inst -> [Int] -> [Int]
gtrr = dorr gtOp

eqOp :: Ord a => a -> a -> Int
eqOp a b = if a == b then 1 else 0

eqir :: Inst -> [Int] -> [Int]
eqir = doir eqOp
eqri :: Inst -> [Int] -> [Int]
eqri = dori eqOp
eqrr :: Inst -> [Int] -> [Int]
eqrr = dorr eqOp

-- Aggregate an operations validation and implementation
data ValidationAndOp = ValidationAndOp {
    opId :: Op,
    validate :: Inst -> Bool,
    apply :: Inst -> [Int] -> [Int]
}

-- Because that's too much typing...
vao :: Op -> (Inst -> Bool) -> (Inst -> [Int] -> [Int]) -> ValidationAndOp
vao = ValidationAndOp

operations :: [ValidationAndOp]
operations = [
    vao Addr validRR addr, vao Addi validRI addi,
    vao Mulr validRR mulr, vao Muli validRI muli,
    vao Banr validRR banr, vao Bani validRI bani,
    vao Borr validRR borr, vao Bori validRI bori,
    vao Setr validRI setr, vao Seti (\_ -> True) seti,
    vao Gtir validIR gtir, vao Gtri validRI gtri, vao Gtrr validRR gtrr,
    vao Eqir validIR eqir, vao Eqri validRI eqri, vao Eqrr validRR eqrr]

opsByCode :: Map Op ValidationAndOp
opsByCode = List.foldl (\m v -> Map.insert (opId v) v m) Map.empty operations

operationMatches :: ValidationAndOp -> Inst -> [Int] -> [Int] -> Bool
operationMatches op inst args expected =
    case (validate op) inst of
        False -> False
        True ->
            let result = (apply op) inst args
            in result == expected

canBeAtLeastThree :: Inst -> [Int] -> [Int] -> Bool
canBeAtLeastThree inst args expected =
    let results = filter (\op -> operationMatches op inst args expected) operations
        upToThree = take 3 results
    in length upToThree >= 3

--allOps = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

--getOptSet :: Int -> Map Int (Set Op)

-- Improve our knowledge of what operations a code can map to
refinePossibleOps :: Map Int (Set Op) -> Inst -> [Int] -> [Int] -> Map Int (Set Op)
refinePossibleOps opMap inst args expected =
    let possibilities = filter (\op -> operationMatches op inst args expected) operations
        possibleOpIds = map opId possibilities
        --opMap' = trace ((show (op inst)) ++ ": " ++ (show possibleOpIds)) $ Map.insertWith Set.intersection (op inst) (Set.fromList possibleOpIds) opMap
        opMap' = Map.insertWith Set.intersection (op inst) (Set.fromList possibleOpIds) opMap
    in opMap'

-- An iteration of removing known op codes from sets of other possibilities
pruneOpCodes :: Map Int (Set Op) -> Map Int (Set Op)
pruneOpCodes opsMap =
    let knownMap = Map.filter (\s -> Set.size s == 1) opsMap
        knownCodes = map snd $ Map.toList (Map.map Set.findMin knownMap)
        knownCodesSet = Set.fromList knownCodes
        opsMap' = Map.map (\s -> if Set.size s == 1 then s else Set.difference s knownCodesSet) opsMap
    in opsMap'

deduceOpCodes :: Map Int (Set Op) -> Map Int (Set Op)
deduceOpCodes opsMap =
    let opsMap' = pruneOpCodes opsMap
    in if opsMap == opsMap'
        then opsMap'
        else deduceOpCodes opsMap'

findOpCodes :: [Sample] -> Map Int Op
findOpCodes samples =
    let opsMap = List.foldl (\om (Sample inst args expected) -> refinePossibleOps om inst args expected) Map.empty samples
        opsMap' = deduceOpCodes opsMap
        opMap = Map.map (\opSet -> if Set.size opSet == 1 then Set.findMin opSet else error ("Wrong number of options: " ++ (show opSet)))
                opsMap'
    in opMap

checkSample :: Sample -> Bool
checkSample (Sample inst args expected) = canBeAtLeastThree inst args expected

parseBefore :: String -> Maybe [Int]
parseBefore line =
    let matches :: [[String]]
        matches = line =~ "Before: (.*)"
    in if null matches
        then Nothing
        else
            let match = matches !! 0
            in Just (read (match !! 1) :: [Int])

parseInstruction :: String -> Inst
parseInstruction line =
    let matches :: [[String]]
        matches = line =~ "([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+)"
        match = matches !! 0
    in Inst (read (match !! 1) :: Int) (read (match !! 2) :: Int) (read (match !! 3) :: Int) (read (match !! 4) :: Int)

parseAfter :: String -> [Int]
parseAfter line =
    let matches :: [[String]]
        matches = line =~ "After: (.*)"
        match = matches !! 0
    in read (match !! 1) :: [Int]

parseSample :: [String] -> Maybe (Sample, [String])
parseSample [] = Nothing
parseSample (l:ls) =
    case parseBefore l of
        Nothing -> Nothing
        Just before ->
            let (i:a:_:ls') = ls -- Eat blank line after sample entry as well
                inst = parseInstruction i
                after = parseAfter a
            in Just (Sample inst before after, ls')

parseSamples :: [String] -> ([Sample], [String])
parseSamples lines =
    case parseSample lines of
        Nothing -> ([], lines)
        Just (sample, rest) ->
            let (samples, rest') = parseSamples (rest)
            in (sample:samples, rest')

main = do
    contents <- getContents
    let l = lines contents
        (samples, _) = parseSamples l
        --canBeThree = filter checkSample samples
        opCodes = findOpCodes samples
    --print $ samples !! 0
    --print $ canBeThree
    --print $ length canBeThree
    print $ opCodes
