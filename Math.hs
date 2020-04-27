module Math where

deg :: Floating a => a -> a
deg r = r * 180 / pi

rad :: Floating a => a -> a
rad d = d * pi / 180