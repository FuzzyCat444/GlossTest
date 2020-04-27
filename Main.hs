module Main where

import Graphics.Gloss

import Render

window :: Display
window = InWindow "Nice Window" (800, 600) (100, 100)

background :: Color
background = black

drawing :: Picture
drawing = circle 200.1

main :: IO ()
main = do
    grassTop <- loadBMP "grass_top.bmp"
    grassSide <- loadBMP "grass_side.bmp"
    furnaceSide <- loadBMP "furnace_side.bmp"
    furnaceFront <- loadBMP "furnace_front.bmp"
    furnaceTop <- loadBMP "furnace_top.bmp"
    play window background 60 (scale scl scl grassTop, 
                               scale scl scl grassSide,
                               scale scl scl furnaceTop, 
                               scale scl scl furnaceFront, 
                               scale scl scl furnaceSide,
                               0) 
                               drawWorld inputWorld updateWorld
    where drawWorld world = Pictures [translate (-150) 0 $ renderCube mat1 or1, translate 150 0 $ renderCube mat2 or2]
              where mat1 = CubeMaterial size grassTop grassSide grassSide grassSide grassSide
                    mat2 = CubeMaterial size furnaceTop furnaceFront furnaceSide furnaceSide furnaceSide
                    or1 = orientationFromAngles angle (45 + 20 * sin (angle / 100))
                    or2 = orientationFromAngles angle (45 + 20 * sin (angle / 100))
                    (grassTop, grassSide, furnaceTop, furnaceFront, furnaceSide, angle) = world
          inputWorld event world  = world
          updateWorld delta (a, b, c, d, e, angle) = (a, b, c, d, e, angle + delta * 60)
          size = 100
          scl = size / 16
