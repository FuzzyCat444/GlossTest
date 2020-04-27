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
                        top 
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
          
          actualLeft | yaw < 90 = right
                     | yaw < 180 = back
                     | yaw < 270 = left
                     | otherwise = front
                     
          actualRight | yaw < 90 = front
                      | yaw < 180 = right
                      | yaw < 270 = back
                      | otherwise = left
          
          CubeMaterial size top front left back right = mat
          CubeOrientation yaw cosYaw sinYaw cosPitch sinPitch = or