{-# LANGUAGE OverloadedStrings #-}

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

module Main (main) where

import LoadImage
import qualified NonoSVG as S
import qualified Nonogram as N
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import qualified Data.Configurator as C

nPuzzle :: S.Puzzle -> N.Puzzle
nPuzzle s = N.puzzle (S.hClues s) (S.vClues s)

mkNono :: [[Double]] -> S.Puzzle
mkNono = S.mkPuzzle . map (map $ \x -> if x < 0.0 then '#' else '.')

loadThreshold :: IO Double
loadThreshold = do
  cfg <- C.load [ C.Required "nonosvg.conf" ]
  C.require cfg "threshold"

threshold :: Double -> [[Double]] -> [[Double]]
threshold th = map $ map $ subtract th

equalize :: [[Double]] -> [[Double]]
equalize ds =
  let min_ = minimum $ concat ds
      max_ = maximum $ concat ds
      remap x = (x-min_) / max_
  in map (map remap) ds

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    hPutStrLn stderr "Usage: pngToNono filename.png"
    exitWith $ ExitFailure 1
  thresh <- loadThreshold
  svgConfig <- S.loadConfig
  let fname = head args
  im <- equalize <$> loadToDoubles fname
  let s = mkNono $ threshold thresh im
  hPutStrLn stderr $ S.showPuzzle s
  -- A puzzle is simple if it can be solved by propagation alone
  hPutStrLn stderr $ "puzzle is simple: " ++ show (N.isSimple $ nPuzzle s)
  putStr $ S.puzzleToSvg svgConfig s
