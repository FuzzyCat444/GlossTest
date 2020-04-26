module Main where

import Graphics.Gloss

import Render

window :: Display
window = InWindow "Nice Window" (800, 600) (100, 100)

background :: Color
background = white

drawing :: Picture
drawing = circle 200.1

main :: IO ()
main = do
           testBMP <- loadBMP "test.bmp" 
           play window background 60 testBMP drawWorld inputWorld updateWorld
           where drawWorld world         = Pictures [shear ShearY 1 1 $ scale 10 10 world]
                 inputWorld event world  = world
                 updateWorld delta world = world
           
