-- Data shared by the themes

module Graphics.Rendering.Chart.Themes where

import Data.Functor

-- This module provides its own Default class, hiding defs and Data.Default.Class which are used by
-- Charts
class Default a where
  def :: a

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- multiplies the first element of a pair by a given number
mFst :: Num a => a -> Pair a -> Pair a
mFst n (Pair x y) = Pair (n * x) y

-- multiplies the first element of a pair by a given number
mSnd :: Num a => a -> Pair a -> Pair a
mSnd n (Pair x y) = Pair x (n * y)

cmToPt :: Double -> Int
cmToPt x = round $ 72 * x / 2.54

-- approximately 1.62
goldenRatio :: Double
goldenRatio = (1 + (sqrt 5)) / 2

-- approximately 1.32
plasticNumber :: Double
plasticNumber = ((108 + 12 * (sqrt 69))**(1 / 3) + (108 - 12 * (sqrt 69))**(1 / 3)) / 6

{--data AxisData' x = AxisData' {
    _axis_visibility :: AxisVisibility,
    _axis_viewport :: Range -> x -> Double,
    _axis_tropweiv :: Range -> Double -> x,
    -- | The list of methods to draw the ticks, and the ticks positions at which the ticks should
    --   be drawn.
    _axis_ticks    :: [(Method, [x])],
    _axis_labels   :: [[(x, String)]],
    _axis_grid     :: [ x ]
}
--}
