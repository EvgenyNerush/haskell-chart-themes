-- Theme appropriate for many journals devoted to physics, e.g. PRL, PoP, PPCF.

module Graphics.Rendering.Chart.Themes.Plasma where

import Data.Colour
import Data.Colour.Names
import Control.Lens
import qualified Data.Default.Class as D
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.Floating
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Plot.Annotation
import Graphics.Rendering.Chart.Plot.Lines
import Graphics.Rendering.Chart.Plot.FillBetween
import Graphics.Rendering.Chart.Plot.Points
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Drawing

import Graphics.Rendering.Chart.Themes

-- typical column width in double-column journals, in cm; used here as a width of one-column
-- figures
columnWidth :: Double
columnWidth = 8.6

-- box with width-to-height ratio of 1 : 1
squareFigBox :: Pair Int
squareFigBox = fmap cmToPt $ Pair columnWidth $ columnWidth

-- box with width-to-height ratio of about 1 : 0.76
plasticFigBox :: Pair Int
plasticFigBox = fmap cmToPt $ Pair columnWidth $ columnWidth / plasticNumber

-- box with width-to-height ratio of about 1 : 0.62
goldenFigBox :: Pair Int
goldenFigBox = fmap cmToPt $ Pair columnWidth $ columnWidth / goldenRatio

-- plasticFigBox with twice enlarged height
plasticFigBox' :: Pair Int
plasticFigBox' = mSnd 2 plasticFigBox

-- goldenFigBox with twice enlarged height
goldenFigBox' :: Pair Int
goldenFigBox' = mSnd 2 goldenFigBox

-- box with width-to-height ratio of 1 : 0.5
halfFigBox :: Pair Int
halfFigBox = fmap cmToPt $ Pair columnWidth $ columnWidth / 2

fileOptions :: Pair Int -> FileOptions
fileOptions (Pair x y) = FileOptions (x, y) PDF

thinLineWidth :: Double
thinLineWidth = 0.7 -- device units, i.e. pt for pdf (?)

normalLineWidth :: Double
normalLineWidth = plasticNumber * thinLineWidth

thickLineWidth :: Double
thickLineWidth = plasticNumber * normalLineWidth

fontSize :: Double
fontSize = 9

fontName :: String
fontName = "Linux Libertine"

titleFontStyle :: FontStyle
titleFontStyle = font_name .~ fontName
               $ font_size .~ fontSize
               $ D.def

axisTitleFontStyle = font_slant .~ FontSlantNormal
                   $ titleFontStyle

tickLabelFontStyle = font_color .~ opaque black
                   $ titleFontStyle

axisLineStyle :: LineStyle
axisLineStyle = line_width .~ thinLineWidth
              $ line_color .~ opaque gray
              $ D.def

plotLineStyle :: LineStyle
plotLineStyle = line_color .~ opaque black
              $ line_width .~ normalLineWidth
              $ D.def

axisGridStyle = line_color .~ opaque lightgray
              $ line_dashes .~ [2 * thinLineWidth, 6 * thinLineWidth]
              $ axisLineStyle

dashedLineStyle = line_dashes .~ [6 * thinLineWidth, 2 * thinLineWidth]
                $ plotLineStyle

dottedLineStyle = line_dashes .~ [thinLineWidth, thinLineWidth]
                $ plotLineStyle

axisStyle :: AxisStyle
axisStyle = axis_line_style .~ axisLineStyle
          $ axis_grid_style .~ axisGridStyle
          $ axis_label_style .~ tickLabelFontStyle
          $ axis_label_gap .~ 2.5
          $ D.def

axisXStyle = axisStyle
axisYStyle = axis_label_gap .~ 4 $ axisStyle

minorTickLength :: Double
minorTickLength = (0.5 + goldenRatio) * thinLineWidth

majorTickLength :: Double
majorTickLength = -minorTickLength


-- | this function sets length of major and minor ticks
setTickLength :: PlotValue x => ([x] -> AxisData x) -> [x] -> AxisData x
setTickLength axis_fn xs = axis_ticks .~ majors ++ minors
             $ axis
  where axis = axis_fn xs
        ticks = _axis_ticks axis
        meanL = (foldl (\x (_, y) -> x + y) 0 ticks) / (fromIntegral $ length ticks)
        minors = map (\(x, y) -> (x, minorTickLength))
          $ filter (\(_, y) -> y <= meanL) ticks
        majors = map (\(x, y) -> (x, majorTickLength))
          $ filter (\(_, y) -> y >= meanL) ticks

autoAxis' :: PlotValue x => [x] -> AxisData x
autoAxis' = setTickLength autoAxis

-- splits the given interval by the given number of parts
split :: RealFloat x => (x, x) -> Int -> [x]
split (minV, maxV) n = [minV + dx * fromIntegral i | i <- [0..n]]
  where dx = (maxV - minV) / fromIntegral n

simpleScaledAxis :: (RealFloat x, Show x, PlotValue x) => [x] -> [x] -> [x] -> AxisData x
simpleScaledAxis majorTickP minorTickP _ = makeAxis' realToFrac realToFrac
                                            labelf (labelvs, tickvs, gridvs)
  where labelf = (map cleanup) . (_la_labelf D.def)
        labelvs = majorTickP -- position of labels and major ticks
        tickvs  = minorTickP -- position of minor ticks
        gridvs  = majorTickP -- position of grid lines
        -- drops long tails, e.g. converts "0.15000000000000002" to "0.15"
        cleanup cs = lhs ++ go [] rhs
          where (lhs, rhs) = span (/= '.') cs
                go acc [] = reverse acc
                go acc rs@(c:cs)
                  | length (takeWhile (== '0') rs) >= 6 = go acc []
                  | otherwise                           = go (c:acc) cs

scaledAxis' :: (RealFloat x, Show x, PlotValue x) => [x] -> [x] -> [x] -> AxisData x
scaledAxis' majorTickP minorTickP = setTickLength (simpleScaledAxis majorTickP minorTickP)

defLayout :: (PlotValue x, PlotValue y) => Layout x y
defLayout = layout_title_style .~ titleFontStyle
          $ layout_margin .~ 2
          $ layout_x_axis . laxis_title_style .~ axisTitleFontStyle
          $ layout_x_axis . laxis_style .~ axisXStyle
          $ layout_x_axis . laxis_generate .~ autoAxis'
          $ layout_y_axis . laxis_title_style .~ axisTitleFontStyle
          $ layout_y_axis . laxis_style .~ axisYStyle
          $ layout_y_axis . laxis_generate .~ autoAxis'
          $ D.def

instance (PlotValue x, PlotValue y) => Default (Layout x y) where
  def = defLayout

instance Default FileOptions where
  def = fileOptions goldenFigBox

instance Default (PlotLines x y) where
  def = plot_lines_style .~ plotLineStyle $ D.def

instance Default (PlotFillBetween x y) where
  def = plot_fillbetween_style .~ (FillStyleSolid $ opaque lightgray) $ D.def

instance Default (PlotPoints x y) where
  def = plot_points_style .~ filledPolygon 2.7 3 False (opaque gray) $ D.def

instance Default (PlotAnnotation x y) where
  def = plot_annotation_style .~ axisTitleFontStyle
      $ D.def

instance (Show a, RealFloat a) => Default (LinearAxisParams a) where
  def = D.def
