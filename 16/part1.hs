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

data OpCode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving Show

data Inst = Inst {
    --op :: OpCode,
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
    validate :: Inst -> Bool,
    apply :: Inst -> [Int] -> [Int]
}

-- Because that's too much typing...
vao :: (Inst -> Bool) -> (Inst -> [Int] -> [Int]) -> ValidationAndOp
vao = ValidationAndOp

operations :: [ValidationAndOp]
operations = [
    vao validRR addr, vao validRI addi,
    vao validRR mulr, vao validRI muli,
    vao validRR banr, vao validRI bani,
    vao validRR borr, vao validRI bori,
    vao validRI setr, vao (\_ -> True) seti,
    vao validIR gtir, vao validRI gtri, vao validRR gtrr,
    vao validIR eqir, vao validRI eqri, vao validRR eqrr]

operationMatches :: ValidationAndOp -> Inst -> [Int] -> [Int] -> Bool
operationMatches op inst args expected =
    case (validate op) inst of
        False -> False
        True ->
            let result = (apply op) inst args
            in result == expected

canBeAtLeastThree :: Inst -> [Int] -> [Int] -> Bool
canBeAtLeastThree  inst args expected =
    let results = filter (\op -> operationMatches op inst args expected) operations
        upToThree = take 3 results
    in length upToThree >= 3

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
        canBeThree = filter checkSample samples
    --print $ samples !! 0
    --print $ canBeThree
    print $ length canBeThree
