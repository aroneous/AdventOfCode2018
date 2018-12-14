import Data.Map (Map, insert, fromList, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.ByteString as B
import Data.Monoid (mappend)
import Data.Char
import Data.Ord
import Data.Word
import Codec.BMP
import Debug.Trace (trace)

data Direction = N | S | W | E deriving Show

data Cell = EW | NS | SW_NE | NW_SE | X deriving Show

data Turn = Left | Straight | Right deriving Show

data Cart = Cart {
    direction :: Direction,
    nextTurn :: Turn
} deriving Show

type Grid = Map (Int, Int) Cell

type Carts = Map (Int, Int) Cart

turnDir :: Direction -> Turn -> Direction
turnDir dir turn =
    case turn of
        Straight -> dir
        Main.Left -> case dir of
                E -> N
                W -> S
                N -> W
                S -> E
        Main.Right -> case dir of
                E -> S
                W -> N
                N -> E
                S -> W

calcNextTurn :: Turn -> Turn
calcNextTurn turn =
    case turn of
        Main.Left -> Straight
        Straight -> Main.Right
        Main.Right -> Main.Left

calcNewDir :: Cell -> Cart -> (Direction, Turn)
calcNewDir cell (Cart dir nextTurn) =
    let newDir =
            case cell of
                EW -> dir -- Straight
                NS -> dir -- Straight
                SW_NE -> case dir of -- /
                            E -> N
                            W -> S
                            S -> W
                            N -> E
                NW_SE -> case dir of -- \
                            E -> S
                            W -> N
                            S -> E
                            N -> W
                X -> turnDir dir nextTurn
        newTurn =
            case cell of
                X -> calcNextTurn nextTurn
                _ -> nextTurn
    in (newDir, newTurn)

newCoord :: (Int, Int) -> Direction -> (Int, Int)
newCoord (y, x) dir =
    case dir of
        E -> (y, x+1)
        W -> (y, x-1)
        N -> (y-1, x)
        S -> (y+1, x)

parseCell :: Char -> Maybe (Cell, Maybe Cart)
parseCell c =
    case c of
        ' ' -> Nothing
        '-' -> Just (EW, Nothing)
        '|' -> Just (NS, Nothing)
        '/' -> Just (SW_NE, Nothing)
        '\\' -> Just (NW_SE, Nothing)
        '+' -> Just (X, Nothing)
        'v' -> Just (NS, Just (Cart S Main.Left))
        '^' -> Just (NS, Just (Cart N Main.Left))
        '>' -> Just (EW, Just (Cart E Main.Left))
        '<' -> Just (EW, Just (Cart W Main.Left))
        '\n' -> Nothing

-- Coordinate should be in (y, x) form to take advantage of natural ordering to result in top-to-bottom, left-to-right
addCellEntry :: Grid -> Carts -> Char -> (Int, Int) -> (Grid, Carts)
addCellEntry grid carts c coord =
    case parseCell c of
        Nothing -> (grid, carts)
        Just (cell, Nothing) -> (insert coord cell grid, carts)
        Just (cell, Just cart) -> (insert coord cell grid, insert coord cart carts)

parseRow :: Grid -> Carts -> Int -> String -> (Grid, Carts)
parseRow grid carts y s = List.foldl (\(grid', carts') (x, c) -> addCellEntry grid' carts' c (y, x)) (grid, carts) (zip [0..] s)

(!-) m k = case Map.lookup k m of
    Just a -> a
    Nothing -> error $ "Not found: " ++ (show k)

processCart :: Grid -> Carts -> (Int, Int) -> Cart -> Carts
processCart grid carts (y, x) cart =
    if Map.member (y, x) carts
    then
        let (y', x') = newCoord (y, x) (direction cart)
            newCell = grid !- (y', x')
            --(newDir, newTurn) = trace ("Move from " ++ (show (y, x)) ++ " to " ++ (show (y', x')) ++ ": " ++ (show newCell) ++ " - " ++ (show cart)) (calcNewDir newCell cart)
            (newDir, newTurn) = calcNewDir newCell cart
            collision = Map.member (y', x') carts
            carts' = Map.delete (y, x) carts
--            carts'' = if not collision then Map.insert (y', x') (Cart newDir newTurn) carts' else trace ("Collision at " ++ (show (y', x'))) (Map.delete (y', x') carts')
            carts'' = if not collision then Map.insert (y', x') (Cart newDir newTurn) carts' else Map.delete (y', x') carts'
        in carts''
    else
--        trace ("Cart " ++ (show (y,x)) ++ " already gone") carts
        carts

-- Move the carts one step within the grid. Return coordinates of collision if one occurs
-- Cart map should be in desired top-to-bottom, left-to-right order
iterateGrid :: Grid -> Carts -> Carts
iterateGrid grid carts = Map.foldlWithKey (\carts' coord cart -> processCart grid carts' coord cart) carts carts

findCollision :: Grid -> Carts -> (Int, Int)
findCollision grid carts =
    let carts' = iterateGrid grid carts
    in case Map.size carts' of
        0 -> error "No carts left!"
        1 -> fst $ head (Map.toList carts')
        _ -> findCollision grid carts'

main = do
    contents <- getContents
    let l = lines contents
        (grid, carts) = List.foldl (\(grid, carts) (y, row) -> parseRow grid carts y row) (Map.empty, Map.empty) (zip [0..] l)
--    print carts
--        (carts', collision) = iterateGrid grid carts
    let (y,x) = findCollision grid carts
        collision = (x,y)
--    print grid
    print collision
