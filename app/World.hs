module World where

import Data.Semigroup (stimes)
import Graphics.Gloss hiding (Point, scale)
import Data.Maybe
import System.Random
import Control.Monad (foldM)
import Numeric.Noise.Perlin

import Point
import Cell

type Row = [Cell]

data World = World {
    cells        :: [Row],              -- 2D array of Cells
    width        :: Int,                -- number of cells horizontally
    height       :: Int,                -- number of cells vertically
    cellSize     :: Float,              -- the width/height of each cell
    scale        :: Float,
    seeds        :: [Int]
}    

-- Implementation of Show, so Worlds can be displayed on console (Debug)
instance Show World where
    -- show :: World -> String
    show world = "World of size (" ++ show (width world) ++ "," ++ show (height world) ++ ")"

getHeight :: World -> Float
getHeight world = (fromIntegral $ height world) * (cellSize world)
getWidth :: World -> Float
getWidth world = (fromIntegral $ width world) * (cellSize world)


-- Calculate the maximum width/height of any cell based on the window size
getMaxCellSize :: (Int, Int) -> (Int, Int) -> Float 
getMaxCellSize (w,h) (screenW, screenH) = min maxCellH maxCellW
    where
        maxCellH = fromIntegral screenH / fromIntegral (h+1) - 1
        maxCellW = fromIntegral screenW / fromIntegral (w+1) - 1


generateWorld :: (Int, Int) -> (Int, Int) -> Float -> [Int] -> World 
generateWorld (w,h) (screenW, screenH) scale seeds = World { 
    width        = w,
    height       = h,
    cellSize     = getMaxCellSize (w,h) (screenW, screenH),
    cells        = generateCells (w,h) seeds scale,
    scale        = scale,
    seeds        = seeds
 }


-- Convert the World to a drawable Picture
worldToPic :: World -> Picture
worldToPic world = Pictures . mconcat $ (uncurry $ rowToPic world) <$> pairs
    where 
        rows = cells world
        pairs = zip [0..length rows - 1] rows  
           

-- Convert a given row to a drawable Picture
rowToPic :: World -> Int -> Row -> [Picture]
rowToPic world y row = (uncurry $ cellToPic world) <$> pairs
    where 
        pts = [(x, y) :: Point | x <- [0..length row - 1]]
        pairs = zip pts row
                                    

-- Convert a given cell to a drawable Picture
cellToPic :: World -> Point -> Cell -> Picture
cellToPic world (x,y) cell = translate x' y' $ color colour rect
    where
        size = cellSize world
        rect = rectangleSolid size size
        colour = getColor cell
        (x',y') = getPicPosition (x,y) size
        

-- Calculates the position of the given world cell in the window
getPicPosition :: Point -> Float -> (Float, Float)
getPicPosition (x,y) size = (x', y')
    where
        x' = size * fromIntegral x
        y' = -size * fromIntegral y


-- If the given cell exists, returns the type in a Just; otherwise, returns Nothing
queryCell :: World -> Point -> Maybe Cell
queryCell world (x,y) | validPos world (x,y) = Just $ (cells world !! y) !! x
queryCell world _                            = Nothing

-- Returns true if the given Point is in the World bounds; false otherwise
validPos :: World -> Point -> Bool
validPos world (x,y) = x < w && x >= 0 && y < h && y >= 0
    where (w,h) = (width world, height world)


{-- World MODIFICATION --}

-- Changes a list of cells to be the given type
modifyCells :: World -> [Point] -> Cell -> IO World
modifyCells world [] _ = pure world
modifyCells world pts cell = foldM (\m -> \p -> modifyCell m p cell) world pts

-- Changes a single cell to the given type
modifyCell :: World -> Point -> Cell -> IO World
modifyCell world (x,y) newCell
    | not $ validPos world (x,y)                = pure world                           -- cannot modify Cells outside World
    | otherwise                                 = pure world { cells = newRows }
        where
            oldRows = cells world
            (ls, rs) = splitAt y oldRows
            newRow = modifyRow (head rs) x newCell
            rs' = newRow : tail rs 
            newRows = ls ++ rs'

-- Changes the xth cell in this row to be the given Cell
modifyRow :: Row -> Int -> Cell -> Row
modifyRow row x newCell = ls ++ rs'
    where
        (ls, rs) = splitAt x row
        rs' = newCell : tail rs


-- Gets Perlin Noise in range (-1,1) at the given coordinate
getNoise :: Point -> Point -> Perlin -> Double
getNoise (w,h) (x,y) perlinNoise = noiseValue perlinNoise (x',y',0)
    where
        x' = fromIntegral x :: Double
        y' = fromIntegral y :: Double


-- Normalised noise to within the range (0,1)
getNormalNoise :: Point -> Point -> Perlin -> Double
getNormalNoise (w,h) pt = \pn -> (getNoise (w,h) pt pn + 1) / 2  


generateCell :: Point -> Point -> [Perlin] -> Cell
generateCell (w,h) (x,y) noises = conditionsToCell elevation rain temp
    where
        elevation = getNormalNoise (w,h) (x,y) (noises !! 0)
        rain = getNormalNoise (w,h) (x,y) (noises !! 1)
        polar = 2 * (fromIntegral y / fromIntegral h) - 1   -- between -1 and 1
        polar' = 1 - polar * polar * polar * polar          -- between  1 and 0
        temp = getNormalNoise (w,h) (x,y) (noises !! 2) -- * polar'

generateCells :: (Int, Int) -> [Int] -> Float -> [Row]
generateCells (w,h) seeds scale = [[generateCell (w,h) (x,y) perlinNoises | x <- [0..w]] | y <- [0..h]]
    where
        --octaves     = 5             -- controls smoothness (smaller is smoother, larger is rougher)
        --persistance = 0.5           -- controls smoothness (effect of each octave)
        -- perlin seed octaves scale_safe persistance)
        scale_safe  = realToFrac $ min 0.09 $ max (scale) 0.01

        elevationNoise = perlin (seeds !! 0) 5 scale_safe 0.5
        rainNoise = perlin (seeds !! 1) 1 scale_safe 0.5
        tempNoise = perlin (seeds !! 2) 5 scale_safe 0.5
        perlinNoises = [elevationNoise, rainNoise, tempNoise]