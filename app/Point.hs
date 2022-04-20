module Point where

import System.Random (randomRIO)

type Point = (Int, Int)

-- Adds two points, component-wise
addPoints :: Point -> Point -> Point
addPoints (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- Operator Form for convenience
(<+>) :: Point -> Point -> Point
(<+>) = addPoints

-- Pick a random point within the given array; if none exist, return nothing
pickRandOf :: [Point] -> IO (Maybe Point)
pickRandOf []   = pure $ Nothing
pickRandOf [pt] = pure $ Just pt
pickRandOf pts  = do 
                      r <- randomRIO (0, length pts - 1)
                      pure $ Just (pts !! r)