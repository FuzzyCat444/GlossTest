module Render where

import Graphics.Gloss

data ShearAxis = ShearX | ShearY

shear :: ShearAxis -> Float -> Float -> Picture -> Picture
shear axis d h picture = case axis of
    ShearX -> rotate angle $ scale 1 s $ rotate 45 $ scale b t picture
    ShearY -> rotate (-angle) $ scale s 1 $ rotate (-45) $ scale t b picture
    where hypsq = d^2 + h^2
          dhyp  = d * sqrt hypsq
          s     = sqrt $ (hypsq - dhyp) / (hypsq + dhyp)
          den   = 1 + s^2
          t     = (sqrt $ 2 * hypsq / den) / h
          b     = sqrt $ 2 / den
          angle = -180 / pi * atan s