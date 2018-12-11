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

data Point = Point {
    x :: Int,
    y :: Int,
    dx :: Int,
    dy :: Int
} deriving Show

readNum :: Int -> String -> Int
readNum digits (sign:num) = let n = read (take digits num) :: Int in
    case sign of
        '-' -> -n
        _ -> n

readPoint :: String -> Point
readPoint line =
    let x = (readNum 5 $ drop 10 line)
        y = (readNum 5 $ drop 18 line)
        dx = (readNum 1 $ drop 36 line)
        dy = (readNum 1 $ drop 40 line)
    in Point x y dx dy

addTime :: Int -> Point -> Point
addTime secs (Point x y dx dy) = Point (x + (secs * dx)) (y + (secs * dy)) dx dy

imgSize :: [Point] -> (Int, Int)
imgSize points =
    let minx = x $ List.minimumBy (comparing x) points
        maxx = x $ List.maximumBy (comparing x) points
        miny = y $ List.minimumBy (comparing y) points
        maxy = y $ List.maximumBy (comparing y) points
    in (maxx - minx, maxy - miny)

imgBounds :: [Point] -> (Int, Int, Int, Int)
imgBounds points =
    let minx = x $ List.minimumBy (comparing x) points
        maxx = x $ List.maximumBy (comparing x) points
        miny = y $ List.minimumBy (comparing y) points
        maxy = y $ List.maximumBy (comparing y) points
    in (minx, miny, maxx, maxy)

minimizeImg :: [Point] -> ([Point], Int)
minimizeImg points =
    let (w, h) = imgSize points
        points' = map (addTime 1) points
        (w', h') = imgSize points'
        (next, nextTime) = minimizeImg points'
    in if w' < w || h' < h then (next, nextTime + 1) else (points, 0)

pixelOff :: [Word8]
pixelOff = map fromIntegral [0xff, 0xff, 0xff, 0xff]

pixelOn :: [Word8]
pixelOn = map fromIntegral [0, 0, 0, 0xff]

sortPoints :: Point -> Point -> Ordering
sortPoints = comparing y `mappend` comparing x

setPixels :: [(Int, Int)] -> [(Int, Int)] -> [[Word8]]
--setPixels [] _ = []
setPixels [] [] = []
setPixels [] x = error $ "Points left over" ++ show x
setPixels (i:is) [] = pixelOff : (setPixels is [])
setPixels ((ix,iy):is) ((py, px):ps) =
    if ix == px && iy == py
    then pixelOn : (setPixels is ps)
    else pixelOff : (setPixels is ((py, px):ps))

makeImage :: [Point] -> BMP
makeImage points =
    let setPoints = Set.fromList $ map (\p -> (-(y p), x p)) points -- Use (y,x) to get desired sorting by default
--        sortedPoints = List.sortBy sortPoints points
        (minx, miny, maxx, maxy) = imgBounds points
        indexes = [(x,y) | y <- [(-maxy)..(-miny)], x <- [minx..maxx]]
        pixels = setPixels indexes (Set.toList setPoints)
        stream  = pixels >>= (\a -> a)
        rgba = B.pack stream
    in packRGBA32ToBMP24 (maxx - minx + 1) (maxy - miny + 1) rgba

main = do
    contents <- getContents
    let l = lines contents
        points = map readPoint l
        later = map (addTime 10000) points
        (optimal, time) = minimizeImg later
        realTime = 10000 + time
--        first = map (addTime (-10)) optimal
--        pointses = map (\idx -> map (addTime idx) first) [0..19]
--        images = map makeImage pointses
--        image = makeImage optimal
        --dims = bmpDimensions image
        --sortedPoints = map (\p -> (x p, y p)) $ List.sortBy sortPoints optimal
--    print dims
--    print sortedPoints
--    writeBMP "image.bmp" image
--        bestBounds = imgBounds optimal
--        sortedOptimal = List.sortBy sortPoints optimal
--    print sortedOptimal
--    print bestBounds
--    sequence (map (\(img, idx) -> writeBMP ("image" ++ show idx ++ ".bmp") img) $ zip images [1..])
--    writeBMP "image.bmp" image
    print realTime
