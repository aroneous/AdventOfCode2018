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
    deriving (Show, Read, Eq, Ord)

data Inst = Inst {
    op :: Op,
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
vr r = r <= 5

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
dorr :: (Int -> Int -> Int) -> Inst -> Seq Int -> Seq Int
dorr op (Inst _ ar br cr) args =
    let ai = args `index` ar
        bi = args `index` br
        args' = S.update cr (op ai bi) args
    in args'

-- Perform a 'register immediate' operation
dori :: (Int -> Int -> Int) -> Inst -> Seq Int -> Seq Int
dori op (Inst _ ar bi cr) args =
    let ai = args `index` ar
        args' = S.update cr (op ai bi) args
    in args'

-- Perform a 'immediate register' operation
doir :: (Int -> Int -> Int) -> Inst -> Seq Int -> Seq Int
doir op (Inst _ ai br cr) args =
    let bi = args `index` br
        args' = S.update cr (op ai bi) args
    in args'

addr :: Inst -> Seq Int -> Seq Int
addr = dorr (+)
addi :: Inst -> Seq Int -> Seq Int
addi = dori (+)
mulr :: Inst -> Seq Int -> Seq Int
mulr = dorr (*)
muli :: Inst -> Seq Int -> Seq Int
muli = dori (*)
banr :: Inst -> Seq Int -> Seq Int
banr = dorr (.&.)
bani :: Inst -> Seq Int -> Seq Int
bani = dori (.&.)
borr :: Inst -> Seq Int -> Seq Int
borr = dorr (.|.)
bori :: Inst -> Seq Int -> Seq Int
bori = dori (.|.)
setr :: Inst -> Seq Int -> Seq Int
setr = dori (\a _ -> a)

seti :: Inst -> Seq Int -> Seq Int
seti (Inst _ ai _ cr) args =
    let args' = S.update cr ai args
    in args'

gtOp :: Ord a => a -> a -> Int
gtOp a b = if a > b then 1 else 0

gtir :: Inst -> Seq Int -> Seq Int
gtir = doir gtOp
gtri :: Inst -> Seq Int -> Seq Int
gtri = dori gtOp
gtrr :: Inst -> Seq Int -> Seq Int
gtrr = dorr gtOp

eqOp :: Ord a => a -> a -> Int
eqOp a b = if a == b then 1 else 0

eqir :: Inst -> Seq Int -> Seq Int
eqir = doir eqOp
eqri :: Inst -> Seq Int -> Seq Int
eqri = dori eqOp
eqrr :: Inst -> Seq Int -> Seq Int
eqrr = dorr eqOp

-- Aggregate an operations validation and implementation
data ValidationAndOp = ValidationAndOp {
    opId :: Op,
    validate :: Inst -> Bool,
    apply :: Inst -> Seq Int -> Seq Int
}

-- Because that's too much typing...
vao :: Op -> (Inst -> Bool) -> (Inst -> Seq Int -> Seq Int) -> ValidationAndOp
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

runInst :: Inst -> Seq Int -> Seq Int
runInst inst regs =
    --let opCode = trace ((show inst) ++ " " ++ (show regs)) op inst
    let opCode = op inst
        vao = opsByCode ! opCode
    in (apply vao) inst regs

--runProgram :: [Inst] -> [Int]
--runProgram insts = List.foldl (\reg inst -> runInst inst reg) [0,0,0,0,0,0] insts

runProgram :: [Inst] -> Int -> Int -> Seq Int -> Seq Int
runProgram insts pcReg pc regs =
    if pc >= length insts || pc < 0
    then regs -- Terminate
    else
        let regs' = S.update pcReg pc regs -- Write PC to bound register
            inst = insts !! pc -- Findt the instruction at the PC
            --regs'' = trace ((show pc) ++ ": " ++ (show inst) ++ " " ++ (show regs)) (runInst inst regs') -- Run the instruction
            regs'' = runInst inst regs' -- Run the instruction
            pc' = trace ((show pc) ++ ": " ++ (show inst) ++ " " ++ (show regs') ++ " -> " ++ (show regs''))
                (regs'' `index` pcReg) + 1 -- Update PC from bound register, then increment
            --pc' = (regs'' `index` pcReg) + 1 -- Update PC from bound register, then increment
        in runProgram insts pcReg pc' regs'' -- Recurse

-- find the op code register
parseDirective :: String -> Int
parseDirective dir = read [dir !! 4]

parseInstruction :: String -> Inst
parseInstruction line =
    let matches :: [[String]]
        matches = line =~ "([[:alpha:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+)"
        match = matches !! 0
        (opHead:opRest) = match !! 1
        opName = (toUpper opHead) : opRest
    in Inst (read opName :: Op) (read (match !! 2) :: Int) (read (match !! 3) :: Int) (read (match !! 4) :: Int)

main = do
    contents <- getContents
    let l = lines contents
        (directive:insts) = l
        pcReg = parseDirective directive
        program = map parseInstruction insts
        finalState = runProgram program pcReg 0 $ S.fromList [0,0,0,0,0,0]
    --print $ samples !! 0
    --print $ canBeThree
    --print $ length canBeThree
    --print pcReg
    --print $ program
    print finalState

-- Solved by studying trace output and finding the value R0 was first being compared to in the only instruction
-- referencing it - the 'eqrr'. That's the answer for part 1.
