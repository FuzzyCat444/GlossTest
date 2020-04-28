module Render where

import Data.Fixed
import Graphics.Gloss

import Math

data ShearAxis = ShearX | ShearY

shear :: ShearAxis -> Float -> Float -> Picture -> Picture
shear axis d h picture = case axis of
    ShearX -> (rotate angle . scale 1 s . rotate 45 . scale b t) picture
    ShearY -> (rotate (-angle) . scale s 1 . rotate (-45) . scale t b) picture
    where hypsq = d^2 + h^2
          dhyp = d * sqrt hypsq
          s = sqrt ((hypsq - dhyp) / (hypsq + dhyp))
          den = 1 + s^2
          t = sqrt (2 * hypsq / den) / h
          b = sqrt (2 / den)
          angle = - deg (atan s)
          
data CubeMaterial = CubeMaterial { size :: Float
                                 , top :: Picture
                                 , front :: Picture
                                 , left :: Picture
                                 , back :: Picture
                                 , right :: Picture
                                 }
                                 
data CubeOrientation = CubeOrientation { yaw :: Float
                                       , cosYaw :: Float
                                       , sinYaw :: Float
                                       , cosPitch :: Float
                                       , sinPitch :: Float
                                       }
                                       
orientationFromAngles :: Float -> Float -> CubeOrientation
orientationFromAngles yaw pitch = CubeOrientation (yaw `mod'` 360) (cos rYaw) (sin rYaw) (cos rPitch) (sin rPitch)
    where rYaw = rad (yaw `mod'` 90)
          rPitch = rad pitch
          
renderCube :: CubeMaterial -> CubeOrientation -> Picture
renderCube mat or = Pictures [topPicture, leftPicture, rightPicture]
    where topPicture = (translate 0 (b/2) 
                      . scale 1 sinPitch 
                      . rotate (-yaw)) 
                        actualTop
          leftPicture = (translate (l/2 - r/2) (b/2 - t/2) 
                       . shear ShearY (-tu) l 
                       . scale sinYaw cosPitch 
                       . translate (-size/2) (-size/2)) 
                         actualLeft
          rightPicture = (translate (l/2 - r/2) (b/2 - t/2) 
                        . shear ShearY (tl) r 
                        . scale cosYaw cosPitch 
                        . translate (size/2) (-size/2)) 
                          actualRight
          
          tu = size * cosYaw * sinPitch
          tl = size * sinYaw * sinPitch
          t = tu + tl
          b = size * cosPitch
          l = size * sinYaw
          r = size * cosYaw
          
          
          shading amt = color (withAlpha amt black) $ rectangleSolid size size
          shadedTop = Pictures [top, shading 0.0]
          shadedFront = Pictures [front, shading 0.2]
          shadedLeft = Pictures [left, shading 0.4]
          shadedBack = Pictures [back, shading 0.6]
          shadedRight = Pictures [right, shading 0.4]
          
          actualTop = shadedTop
          
          actualLeft | yaw < 90 = shadedRight
                     | yaw < 180 = shadedBack
                     | yaw < 270 = shadedLeft
                     | otherwise = shadedFront
                     
          actualRight | yaw < 90 = shadedFront
                      | yaw < 180 = shadedRight
                      | yaw < 270 = shadedBack
                      | otherwise = shadedLeft
          
          CubeMaterial size top front left back right = mat
          CubeOrientation yaw cosYaw sinYaw cosPitch sinPitch = or