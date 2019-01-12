-- Data shared by the themes

module Graphics.Rendering.Chart.Themes where

import Data.Functor

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

cmToPt :: Double -> Int
cmToPt x = round $ 72 * x / 2.54

-- approx 1.62
goldenRatio :: Double
goldenRatio = (1 + (sqrt 5)) / 2

-- approx 1.32
plasticNumber :: Double
plasticNumber = ((108 + 12 * (sqrt 69))**(1 / 3) + (108 - 12 * (sqrt 69))**(1 / 3)) / 6
