{-# LANGUAGE OverloadedStrings, RecordWildCards, DisambiguateRecordFields #-}

-- Copyright (C) 2014  Sami Liedes <sami.liedes@iki.fi>
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301, USA.

module NonoSVG (Puzzle(..),
                puzzleToSvg,
                mkPuzzle,
                showPuzzle,
                defaultConfig,
                loadConfig) where

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)
import Control.Monad (forM_, when, join, liftM)
import Data.String (fromString)
import Data.List (transpose)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT

-- configuration, read from nonosvg.conf
data Config = Config { guides :: (Int, Int)
                     , strokeWidth :: (Double, Double)
                     , scaleFactor :: Double
                     , guideColor :: (String, String)
                     , normalColor :: (String, String)
                     , horizCluesMargin :: Double
                     , vertCluesMargin :: Double
                     , horizCluesSep :: Double
                     , vertCluesSep :: Double
                     , defaultTextSize :: Double
                     , defaultCellSize :: Double
                     , fontFamily :: String
                     , textPosX :: Double
                     , textPosY :: Double
                     , svgMargin :: Double
                     , textAnchor :: String
                     }

loadHorizVert :: CT.Configured a => CT.Config -> CT.Name -> IO (a, a)
loadHorizVert c name = do
  hv <- C.lookup c name
  case hv of
    Just (h, v) -> return (h, v)
    Nothing     -> liftM (join (,)) $ C.require c name

-- FIXME currently loads from working directory
loadConfig :: IO Config
loadConfig = do
  c <- C.load [ C.Required "nonosvg.conf" ]
  guides           <- loadHorizVert c "guides"
  strokeWidth      <- loadHorizVert c "stroke_width"
  scaleFactor      <- C.require c "scale_factor"
  guideColor       <- loadHorizVert c "strong_color"
  normalColor      <- loadHorizVert c "weak_color"
  horizCluesMargin <- C.require c "horiz_clues_margin"
  vertCluesMargin  <- C.require c "vert_clues_margin"
  horizCluesSep    <- C.require c "horiz_clues_sep"
  vertCluesSep     <- C.require c "vert_clues_sep"
  defaultTextSize  <- C.require c "text_size"
  defaultCellSize  <- C.require c "cell_size"
  fontFamily       <- C.require c "font_family"
  textPosX         <- C.require c "text_pos_x"
  textPosY         <- C.require c "text_pos_y"
  svgMargin        <- C.require c "svg_margin"
  textAnchor       <- C.require c "clue_text_anchor"
  return Config{..}

defaultConfig :: Config
defaultConfig = Config{..}
  where
    -- stronger lines every n cells
    guides = join (,) 5
    strokeWidth = join (,) 0.7
    scaleFactor = 0.35
    guideColor = join (,) "black"
    normalColor = join (,) "lightgray"
    horizCluesMargin = 0.4
    vertCluesMargin = 0.4
    horizCluesSep = 1.3
    vertCluesSep = 1.2
    -- portion of grid
    defaultTextSize = 0.6
    defaultCellSize = 12.0
    fontFamily = "Droid Sans, FreeSans, sans"
    textPosX = 0.5
    textPosY = 0.7
    svgMargin = 3.0
    textAnchor = "middle"

val :: Show a => a -> S.AttributeValue
val = fromString . show

data GridParams = GridParams { columns :: Int
                             , rows :: Int
                             , cellWidth :: Double
                             , cellHeight :: Double
                             , textSize :: Double
                             }
                deriving (Eq, Show)

mkGridParams :: Config -> Int -> Int -> GridParams
mkGridParams Config{..} r c =
  GridParams { columns = c
             , rows = r
             , cellWidth = defaultCellSize
             , cellHeight = defaultCellSize
             , textSize = defaultTextSize * defaultCellSize
             }

modCellWidth :: (Double -> Double) -> GridParams -> GridParams
modCellWidth f gparams = gparams { cellWidth = f $ cellWidth gparams }

modCellHeight :: (Double -> Double) -> GridParams -> GridParams
modCellHeight f gparams = gparams { cellHeight = f $ cellHeight gparams }

data CellData = CellData { _cdataX, _cdataY :: Int
                         , _cdataPosX, _cdataPosY :: Double
                         }
              deriving (Eq, Show)

cellDatas :: GridParams -> [[CellData]]
cellDatas GridParams{..} =
  let cw x = cellWidth * fromIntegral x
      ch y = cellHeight * fromIntegral y
      row ry = map (\x -> CellData x ry (cw x) (ch ry)) [0..columns-1]
  in map row [0..rows-1]

data HorizVert = Horiz | Vert
               deriving (Eq, Show)

chooseHV :: HorizVert -> (a, a) -> a
chooseHV Horiz = fst
chooseHV Vert  = snd

drawGrid :: Config -> GridParams -> S.Svg
drawGrid Config{..} GridParams{..} =
  let gridWidth = cellWidth * fromIntegral columns
      gridHeight = cellHeight * fromIntegral rows
      cx x = fromIntegral x * cellWidth
      cy y = fromIntegral y * cellHeight
      strongLine hv = strokeLine (chooseHV hv guideColor) (chooseHV hv strokeWidth)
      weakLine hv = strokeLine (chooseHV hv normalColor) (chooseHV hv strokeWidth)
      noLine _ _ _ _ _ = return ()
      strokeLine color width x1 y1 x2 y2 =
        S.line ! A.x1 (val x1) ! A.y1 (val y1) ! A.x2 (val x2) ! A.y2 (val y2) !
        A.stroke (fromString color) ! A.strokeWidth (val width)
      -- draw either strong or weak lines, depending on param
      drawEither strong =
        let strong_ = if strong     then strongLine else noLine
            weak_   = if not strong then weakLine   else noLine
        in do
          forM_ [0..rows] $ \y ->
            (if y `mod` chooseHV Horiz guides == 0 ||
                y == rows then strong_ else weak_)
            Horiz 0 (cy y) gridWidth (cy y)
          forM_ [0..columns] $ \x ->
            (if x `mod` chooseHV Vert guides == 0 ||
                x == columns then strong_ else weak_)
            Vert (cx x) 0 (cx x) gridHeight
  in do
    drawEither False
    drawEither True

-- right-justify
rJust :: a -> Int -> [a] -> [a]
rJust filler n xs =
  let len = length xs
  in replicate (n-len) filler ++ xs

gridText :: Config -> GridParams -> [[String]] -> S.Svg
gridText Config{..} gparams@GridParams{..} texts =
  let cd = cellDatas gparams
      cdAndTexts = concat $ zipWith zip cd texts
  in forM_ cdAndTexts $ \(CellData _ _ posx posy, text) ->
    when (text /= "") $
    S.text_ !
      A.x (val $ posx + cellWidth * textPosX) !
      A.y (val $ posy + cellHeight * textPosY) !
      A.fontSize (val textSize) !
      A.fontFamily (fromString fontFamily) !
      A.textAnchor (fromString textAnchor) $
      fromString text

data Puzzle = Puzzle { puzzleWidth, puzzleHeight :: Int
                     , puzzleSquares :: [[Bool]]
                     , hClues, vClues :: [[Int]]
                     , maxHClues, maxVClues :: Int
                     }
            deriving (Eq, Show)

mkPuzzle :: [String] -> Puzzle
mkPuzzle [] = error "mkPuzzle: Empty puzzle"
mkPuzzle s =
  let puzzleHeight = length s
      ncol0 = length $ head s
      puzzleWidth = if all (\row -> length row == ncol0) s
                     then ncol0
                     else error "mkPuzzle: Not all rows have same length."
      charToBool '#' = True
      charToBool '.' = False
      charToBool c = error $ "Invalid char in puzzle (use # and .): " ++
                     show c
      puzzleSquares = map (map charToBool) s
      hClues = _hClues puzzleSquares
      vClues = _vClues puzzleSquares
      maxHClues :: Int
      maxHClues = maximum $ map length hClues
      maxVClues = maximum $ map length vClues
  in Puzzle{..}

_hClues :: [[Bool]] -> [[Int]]
_hClues = map rowClues

_vClues :: [[Bool]] -> [[Int]]
_vClues = map rowClues . transpose

showPuzzle :: Puzzle -> String
showPuzzle Puzzle{..} =
  unlines $ map (map $ \x -> if x then '#' else '.') puzzleSquares

rowClues :: [Bool] -> [Int]
rowClues [] = []
rowClues r@(True:_) = let (run, rest) = span id r in
  length run : rowClues rest
rowClues (False:r) = rowClues r

-- returns svg, width
horizCluesGrid :: Config -> Puzzle -> (S.Svg, Double)
horizCluesGrid conf@Config{..} Puzzle{..} =
  let w = maxHClues
      h = puzzleHeight
      clues = map (rJust "" w . map show) hClues
      gparams = modCellWidth (* horizCluesSep) $ mkGridParams conf h w
      svg = gridText conf gparams clues
      margin = horizCluesMargin * defaultCellSize
  in (svg, cellWidth gparams * fromIntegral w + margin)

-- returns svg, height
vertCluesGrid :: Config -> Puzzle -> (S.Svg, Double)
vertCluesGrid conf@Config{..} Puzzle{..} =
  let w = puzzleWidth
      h = maxVClues
      clues = transpose $ map (rJust "" h . map show) vClues
      gparams = modCellHeight (* vertCluesSep) $ mkGridParams conf h w
      svg = gridText conf gparams clues
      margin = vertCluesMargin * defaultCellSize
  in (svg, cellHeight gparams * fromIntegral h + margin)

-- returns svg, width
drawHorizClues :: Config -> Puzzle -> (S.Svg, Double)
drawHorizClues = horizCluesGrid

-- returns svg, (width, height)
renderPuzzleGrid :: Config -> Puzzle -> (S.Svg, (Double, Double))
renderPuzzleGrid conf@Config{..} Puzzle{..} =
  let svg = drawGrid conf $ mkGridParams conf puzzleHeight puzzleWidth
  in (svg, (defaultCellSize * fromIntegral puzzleWidth,
            defaultCellSize * fromIntegral puzzleHeight))

-- returns svg, (width, height)
renderPuzzle :: Config -> Puzzle -> (S.Svg, (Double, Double))
renderPuzzle conf p =
  let (hclues, hcluesw) = drawHorizClues conf p
      (vclues, vcluesh) = vertCluesGrid conf p
      px = hcluesw
      py = vcluesh
      (puzgrid, (pw, ph)) = renderPuzzleGrid conf p
      svg = do
        S.g ! A.transform (S.translate px 0) $ vclues
        S.g ! A.transform (S.translate 0 py) $ hclues
        S.g ! A.transform (S.translate px py) $ puzgrid
  in (svg, (px + pw, py + ph))

svgDoc :: Config -> Puzzle -> S.Svg
svgDoc conf@Config{..} puz =
  let (puzzleSvg, (w, h)) = renderPuzzle conf puz
      vboxw = w + svgMargin*2
      vboxh = h + svgMargin*2
      viewBoxStr = "0 0 " ++ show vboxw ++ " " ++ show vboxh
      realWidth = show (vboxw * scaleFactor) ++ "mm"
      realHeight = show (vboxh * scaleFactor) ++ "mm"
  in S.docTypeSvg ! A.version "1.1" !
     A.width (fromString realWidth) !
     A.height (fromString realHeight) !
     A.viewbox (fromString viewBoxStr) $
       S.g ! A.transform (S.translate svgMargin svgMargin) $ puzzleSvg

puzzleToSvg :: Config -> Puzzle -> String
puzzleToSvg conf = renderSvg . svgDoc conf
