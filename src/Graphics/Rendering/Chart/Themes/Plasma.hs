-- Theme appropriate for many journals devoted to physics, e.g. PRL, PoP, PPCF.

module Graphics.Rendering.Chart.Themes.Plasma where

import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.Floating
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Plot.Annotation

import Graphics.Rendering.Chart.Themes

-- typical column width in double-column journals, in cm
columnWidth :: Double
columnWidth = 8.6

-- box with width-to-height ratio of about 1.6 : 1
goldenFigBox :: Pair Int
goldenFigBox = fmap cmToPt $ Pair columnWidth $ columnWidth / goldenRatio

goldenFigBox' :: Pair Int
goldenFigBox' = fmap cmToPt $ Pair columnWidth $ columnWidth * goldenRatio

-- box with width-to-height ratio of about 1.6 : 1
plasticFigBox :: Pair Int
plasticFigBox = fmap cmToPt $ Pair columnWidth $ columnWidth / plasticNumber

plasticFigBox' :: Pair Int
plasticFigBox' = fmap cmToPt $ Pair columnWidth $ columnWidth * plasticNumber

-- qwe
doubleFigBox' = fmap cmToPt $ Pair columnWidth $ 2 * columnWidth

-- box with width-to-height ratio of 1 : 1
squareFigBox :: Pair Int
squareFigBox = fmap cmToPt $ Pair columnWidth $ columnWidth

fileOptions :: Pair Int -> FileOptions
fileOptions (Pair x y) = FileOptions (x, y) PDF

thinLineWidth :: Double
thinLineWidth = 0.7 -- device units, i.e. pt?

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
               $ def

axisTitleFontStyle = font_slant .~ FontSlantNormal --FontSlantItalic
                   $ titleFontStyle

tickLabelFontStyle = font_color .~ opaque dimgray
                   $ titleFontStyle

axisLineStyle :: LineStyle
axisLineStyle = line_width .~ thinLineWidth
              $ def

axisGridStyle = line_color .~ opaque gray
              $ line_dashes .~ [1, 3]
              $ axisLineStyle

axisStyle :: AxisStyle
axisStyle = axis_line_style .~ axisLineStyle
          $ axis_grid_style .~ axisGridStyle
          $ axis_label_style .~ tickLabelFontStyle
          $ axis_label_gap .~ 3
          $ def

majorTickHeight = fontSize / (goldenRatio**2)

axisGenerate :: (PlotValue x, RealFloat x, Show x) => [x] -> AxisFn x
axisGenerate labelpos _ = axis_ticks .~ (map (\(x, h) -> (x, majorTickHeight)) $ _axis_ticks ma)
                        $ ma
  where ma = makeAxis (_la_labelf d) (labelpos, tickpos, gridpos)
        tickpos = tail labelpos
        gridpos = tail labelpos
        d :: (RealFloat a, Show a) => LinearAxisParams a
        d = def

defPlotAnnotation :: PlotAnnotation x y
defPlotAnnotation = plot_annotation_style .~ titleFontStyle
                  $ def

defLayout :: (PlotValue x, PlotValue y) => Layout x y
defLayout = layout_title_style .~ titleFontStyle
          $ layout_margin .~ 3
          $ layout_x_axis . laxis_title_style .~ axisTitleFontStyle
          $ layout_x_axis . laxis_style .~ axisStyle
          $ layout_y_axis . laxis_title_style .~ axisTitleFontStyle
          $ layout_y_axis . laxis_style .~ axisStyle
          $ def

{--
myAutoAxis :: AxisFn Double
myAutoAxis = autoAxis

getGrid :: (Double, Double) -> Int -> [Double]
getGrid (x0, x1) nGridLines = [x0 + fromIntegral i * (x1 - x0) / fromIntegral nGridLines | i <- [1..nGridLines]]

{-myAxisFn :: Int -> AxisFn Double -- i.e., [Double] -> AxisData Double
myAxisFn nGridLines x = axis_grid .~ (getGrid (minimum x, maximum x) nGridLines)
           $ myAutoAxis x
           -}

myAxisFn :: [Double] -> Int -> (Double -> String) -> Bool -> AxisFn Double -- i.e., [Double] -> AxisData Double
myAxisFn labelvs nMinorSegments toStringFn drawGrid = \_ -> makeAxis (map toStringFn) (labelvs, tickvs, labelvs')
    where tickvs = [min' + fromIntegral i * (max' - min') / fromIntegral nMinorSegments | i <- [0..nMinorSegments]]
          min' = minimum labelvs
          max' = maximum labelvs
          labelvs' = if drawGrid then labelvs
                                 else [last labelvs]

myAxisLineStyle = LineStyle { _line_width = thinLineWidth
    , _line_color = opaque jet
    , _line_dashes = []
    , _line_cap = LineCapButt
    , _line_join = LineJoinMiter }

myAxisGridStyle = line_color .~ opaque gris
                $ line_dashes .~ [smallMarkerSize, bigMarkerSize]
                $ myAxisLineStyle

myAxisStyle = AxisStyle { _axis_line_style = myAxisLineStyle
    , _axis_label_style = tickLabelFontStyle
    , _axis_grid_style = myAxisGridStyle
    , _axis_label_gap = 7 }

data Cases = Case1 -- False
           | Case2 -- True
           | Case3 -- False2

defLayout xlabelvs nXMinorSegments ylabelvs nYMinorSegments (shouldRoundX, shouldRoundY) nColumns drawGrid = Layout
    { _layout_background      = solidFillStyle $ opaque white
    , _layout_plot_background = Nothing

    , _layout_title           = ""
    , _layout_title_style     = titleFontStyle

    , _layout_x_axis          = LayoutAxis { _laxis_title_style = axisTitleFontStyle
                                           , _laxis_title       = ""
                                           , _laxis_style       = myAxisStyle
                                           , _laxis_generate    = myAxisFn xlabelvs nXMinorSegments (show' shouldRoundX) drawGrid
                                           , _laxis_override    = id
                                           , _laxis_reverse     = False}
    , _layout_top_axis_visibility    = AxisVisibility { _axis_show_line   = False
                                                      , _axis_show_ticks  = False
                                                      , _axis_show_labels = False }
    , _layout_bottom_axis_visibility = AxisVisibility { _axis_show_line   = True
                                                      , _axis_show_ticks  = True
                                                      , _axis_show_labels = True}

    , _layout_y_axis                 = LayoutAxis { _laxis_title_style = axisTitleFontStyle
                                           , _laxis_title       = ""
                                           , _laxis_style       = myAxisStyle
                                           , _laxis_generate    = myAxisFn ylabelvs nYMinorSegments (show' shouldRoundY) drawGrid
                                           , _laxis_override    = id
                                           , _laxis_reverse     = False}

    , _layout_left_axis_visibility   = AxisVisibility { _axis_show_line   = True
                                                      , _axis_show_ticks  = True
                                                      , _axis_show_labels = True}

    , _layout_right_axis_visibility  = AxisVisibility { _axis_show_line   = False
                                                      , _axis_show_ticks  = False
                                                      , _axis_show_labels = False }


    , _layout_margin          = fontSize / goldenRatio
    , _layout_plots           = []
    , _layout_legend          = Just (legend_label_style .~ titleFontStyle
    {--Just LegendStyle { _legend_label_style = titleFontStyle
                                                 , _legend_margin = 14
                                                 , _legend_plot_size = 20
                                                 , _legend_orientation = LORows nColumns
                                                 } --}
                              $ legend_margin .~ 13
                              $ legend_plot_size .~ 17
                              $ legend_orientation .~ LORows nColumns
                              $ def)
    , _layout_grid_last       = False
    }
        where show' Case1 = printf "%.1f"
              show' Case2 = (show . round)
              show' Case3 = printf "%.2f"

myText :: FontStyle -> Point -> String -> Renderable ()
myText fs (Point x y) s = Renderable { minsize = mf, render = rf }
    where mf = withFontStyle fs $ do
              ts <- textSize s
              return (textSizeWidth ts, textSizeHeight ts)
          rf (_, _) = withFontStyle fs $ do
              drawText (Point x y) s
              return (\_-> Nothing)

myText' :: FontStyle -> Point -> String -> Grid (Renderable ()) -> Grid (Renderable ())
myText' fs p s = overlay (tval $ myText fs p s)

vecLineStyle col = LineStyle { _line_width = normalLineWidth
    , _line_color = col
    , _line_dashes = []
    , _line_cap = LineCapButt
    , _line_join = LineJoinMiter }

vecHeadStyle col = PointStyle { _point_color = col
    , _point_border_color = col
    , _point_border_width = 0
    , _point_radius = smallMarkerSize
    , _point_shape = PointShapePolygon 3 False }

defVecStyle col = VectorStyle { _vector_line_style = vecLineStyle col
                              , _vector_head_style = vecHeadStyle col}

labelPosition = Point (0.3 * fontSize) (plasticNumber * fontSize)

annBackgroundRect = def {-rect_minsize .~ (fontSize / 2, fontSize / 2)
                  $ rect_fillStyle .~ (Just $ FillStyleSolid $ opaque chromeYellow)
                  $ rect_lineStyle .~ (Just $ line_color .~ (opaque minium) $ myAxisLineStyle)
                  -- $ rect_fillStyle .~ Nothing
                  -- $ rect_lineStyle .~ Nothing
                  -- $ rect_cornerStyle .~ RCornerRounded smallMarkerSize
                  $ rect_cornerStyle .~ RCornerBevel smallMarkerSize
                  $ def-}

defPlotAnnotation = plot_annotation_vanchor .~ VTA_BaseLine
                  $ plot_annotation_hanchor .~ HTA_Left
                  $ plot_annotation_style .~ axisTitleFontStyle
                  $ plot_annotation_background .~ annBackgroundRect
                  $ def

-- | Draws the 'PlotAnnotation' over the given grid
annotationOverlay :: PlotAnnotation Double Double -> Grid (Renderable ()) -> Grid (Renderable ())
annotationOverlay p = overlay (tval $ plotToRenderable $ toPlot p)
    where plotToRenderable plot = Renderable{ minsize = mf, render = rf }
              where mf = return (0, 0)
                    rf _ = do (_plot_render plot) idPointMapFn 
                              return nullPickFn
                    idPointMapFn (LValue x, LValue y) = Point x y

myText'' :: String -> Grid (Renderable ()) -> Grid (Renderable ())
myText'' s = annotationOverlay p
    where p = plot_annotation_style .~ titleFontStyle
            $ plot_annotation_values .~ [(x, y, s)]
            $ defPlotAnnotation
          Point x y = labelPosition
          --}
