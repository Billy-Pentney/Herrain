module Main where

import Graphics.Gloss hiding (Point, scale)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, scale)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Data.Semigroup (stimes)
import Control.Monad (foldM)

import System.Random (randomRIO)        -- Monadic Random Number Generation
import GHC.Float.RealFracMethods        -- Converting Float to Int

import World
import Point
import Cell



{-- Global Program Constants --}

screenDims = (1500,750) :: (Int,Int)            -- dimensions of the window (width x height)
worldDims = (250,125) :: (Int,Int)              -- dimensions of the grid (width x height)
windowTitle = "Herrain" :: String
fps = 10 :: Int                                 -- number of updates per second
bkg = black :: Color                            -- background colour of the display window
win = InWindow windowTitle screenDims (0,0) :: Display



-- Main Entry point to the program
main :: IO ()
main = do
            world <- createNewWorld worldDims 0.03
            putStrLn $ "Generated a " ++ show world
            playIO win bkg fps world getPicture handleKeys (\f -> pure)



-- Converts the given Grid to a drawable Picture, relative to the centre of the window
getPicture :: World -> IO Picture
getPicture world = pure $ translate tX tY $ worldToPic world
    where
        tX = -getWidth world / 2
        tY = getHeight world / 2



-- Creates a new empty World, of the standard size
createNewWorld :: (Int, Int) -> Float -> IO World
createNewWorld (w,h) scale = do
                            seeds <- getRandSeeds 3
                            pure $ generateWorld (w,h) screenDims scale seeds


-- Regenerate the world with the same seeds but a new scale
recreateWorld :: World -> Float -> IO World
recreateWorld world scale = pure $ generateWorld (w,h) screenDims scale oldSeeds
    where
        (w,h) = (width world, height world)
        oldSeeds = seeds world


-- Generate a single random seed and prefix it to the given array
getRandSeed :: [Int] -> IO [Int]
getRandSeed xs = randomRIO (1,1000) >>= (\x -> pure (x:xs))


-- Get an array of n random seeds
getRandSeeds :: Int -> IO [Int]
getRandSeeds n | n < 0 = pure $ []
getRandSeeds n = foldM (\b -> \a -> getRandSeed b) [] [1..n] 


-- Handles user input in form of keys
handleKeys :: Event -> World -> IO World
handleKeys (EventKey (Char 'r') Down _ _) world = createNewWorld worldDims (scale world)
handleKeys (EventKey (Char '<') Down _ _) world = recreateWorld world (scale world + 0.01)
handleKeys (EventKey (Char '>') Down _ _) world = recreateWorld world (scale world - 0.01)
handleKeys _ world = pure $ world


-- Called when world should be redrawn. Since world is fixed, no changes necessary
update :: Float -> World -> IO World
update _ = pure
 