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

module LoadImage (loadToDoubles,
                  loadResized) where

import Graphics.GD
import Control.Monad (forM)
import Control.Applicative ((<$>))

-- FIXME load only png (for now)
loadImage :: FilePath -> IO Image
loadImage = loadPngFile

intColorToDouble :: (Int, Int, Int, Int) -> (Double, Double, Double, Double)
intColorToDouble (r, g, b, a) = (c r, c g, c b, c a)
  where c val = fromIntegral val / 255.0

colorToDouble :: Color -> Double
colorToDouble c =
  let (r, g, b, _) = intColorToDouble $ toRGBA c
  in (r+g+b)/3.0

imagePixels :: Image -> IO [[Double]]
imagePixels im = do
  (width, height) <- imageSize im
  forM [0..height-1] $ \y ->
    forM [0..width-1] $ \x -> colorToDouble <$> getPixel (x, y) im

resizeToDoubles :: Int -> Int -> Image -> IO [[Double]]
resizeToDoubles w h im = resizeImage w h im >>= imagePixels

loadResized :: Int -> Int -> FilePath -> IO [[Double]]
loadResized w h fname = do
  im <- loadImage fname
  resizeToDoubles w h im

-- load without resizing
loadToDoubles :: FilePath -> IO [[Double]]
loadToDoubles fname = loadImage fname >>= imagePixels
