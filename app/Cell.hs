module Cell where

import Graphics.Gloss.Data.Color

data Cell = Ocean | Water | Coast | Grass | Sand | Desert | Trees | Tundra | Ice | Rocky
    deriving (Eq, Show)

conditionsToCell :: Double -> Double -> Double -> Cell
conditionsToCell elevation rain temp = if temp < 0.15
                                            then decideCold elevation rain temp
                                       else if elevation < 0.6        
                                            then decideWater elevation rain temp
                                       else if elevation < 0.97 
                                            then decideLand elevation rain temp
                                       else if elevation < 0.995 || temp > 0.3
                                            then Rocky
                                       else             
                                            Tundra

decideCold :: Double -> Double -> Double -> Cell
decideCold elevation rain temp = if elevation < 0.65 then 
                                    Ice
                                 else
                                    Tundra

decideWater :: Double -> Double -> Double -> Cell
decideWater elevation rain temp = if elevation < 0.1         
                                    then Ocean
                                  else if  elevation < 0.35 
                                    then Water
                                  else if  elevation < 0.5          
                                    then Coast
                                  else if rain < 0.5    
                                    then Sand
                                  else
                                    Grass  

decideLand :: Double -> Double -> Double -> Cell
decideLand elevation rain temp = if rain < 0.2 && temp > 0.6 
                                    then Desert
                                 else if rain < 0.4 && temp > 0.5
                                    then Sand
                                 else if rain < 0.7 && temp < 0.7            
                                    then Grass
                                 else
                                    Trees

getColor :: Cell -> Color
getColor Ocean  = veryDark blue
getColor Ice    = veryLight azure
getColor Coast  = azure
getColor Desert = makeColorI 255 180 40 255
getColor Tundra = white
getColor Water  = blue
getColor Grass  = green  
getColor Sand   = light yellow
getColor Trees  = makeColorI 34 163 34 255
getColor Rocky  = greyN 0.6
getColor _      = red                    -- catches errors if cell types are not fulfilled

veryDark :: Color -> Color
veryDark = dark . dark

veryLight :: Color -> Color
veryLight = light . light