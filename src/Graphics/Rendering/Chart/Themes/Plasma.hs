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

axisGridStyle = line_color .~ opaque lightgray
              $ line_dashes .~ [d, 3 * d]
              $ axisLineStyle
  where d = thinLineWidth * 2

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
autoAxis' :: PlotValue x => [x] -> AxisData x
autoAxis' xs = axis_ticks .~ majors ++ minors
             $ axis
  where axis = autoAxis xs
        ticks = _axis_ticks axis
        meanL = (foldl (\x (_, y) -> x + y) 0 ticks) / (fromIntegral $ length ticks)
        minors = map (\(x, y) -> (x, minorTickLength))
          $ filter (\(_, y) -> y <= meanL) ticks
        majors = map (\(x, y) -> (x, majorTickLength))
          $ filter (\(_, y) -> y >= meanL) ticks

{--
majorTickHeight = fontSize / (goldenRatio**2)

axisGenerate :: (PlotValue x, RealFloat x, Show x) => [x] -> AxisFn x
axisGenerate labelpos _ = axis_ticks .~ (map (\(x, h) -> (x, majorTickHeight)) $ _axis_ticks ma)
                        $ ma
  where ma = makeAxis (_la_labelf d) (labelpos, tickpos, gridpos)
        tickpos = tail labelpos
        gridpos = tail labelpos
        d :: (RealFloat a, Show a) => LinearAxisParams a
        d = D.def

defPlotAnnotation :: PlotAnnotation x y
defPlotAnnotation = plot_annotation_style .~ titleFontStyle
                  $ D.def
                  --}

defLayout :: (PlotValue x, PlotValue y) => Layout x y
defLayout = layout_title_style .~ titleFontStyle
          $ layout_margin .~ 0
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
  def = plot_lines_style .~ (line_color .~ opaque black $ axisLineStyle) $ D.def
