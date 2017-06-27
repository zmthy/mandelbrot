------------------------------------------------------------------------------
module Main where

import           Control.Monad (Monad ((>>=)), join, mzero)
import           Data.Complex  (Complex ((:+)), magnitude)


------------------------------------------------------------------------------
main :: IO ()
main = print (makeDataBw 240 135)


------------------------------------------------------------------------------
makeData :: ((Complex Double, Int) -> a) -> Double -> Int -> Int -> Int -> [a]
makeData f limit iter w h =
    map (f . mandelbrot limit iter . toComplexCoord w h) [0 .. w * h - 1]

makeDataBw :: Int -> Int -> [Bool]
makeDataBw = makeData ((> limit) . magnitude . fst) limit 1024
  where limit = 2


------------------------------------------------------------------------------
toColor:: Int -> (Complex Double, Int) -> (Int, Int, Int)
toColor limit (z, i)
    | magnitude z > 2 =
      (truncate $ v * 255, truncate $ 125 * v + 125, truncate $ 75 + v * 75)
    | otherwise = (255, 255, 255)
  where
    zn = magnitude z
    nu = logBase 2 (logBase 2 zn)
    iter = fromIntegral i + 1 - nu
    v = threshDiv (fromIntegral limit) iter

threshDiv :: Double -> Double -> Double
threshDiv b a
    | a > b = 255
    | otherwise = a / b * 128


------------------------------------------------------------------------------
toComplexCoord:: Int -> Int -> Int -> Complex Double
toComplexCoord w h i = scaleX xf :+ scaleY yf
  where
    x = fromIntegral $ i `rem` w
    y = fromIntegral $ i `quot` w
    xf = x / fromIntegral (w - 1)
    yf = y / fromIntegral (h - 1)
    scaleY = scale 1 1
    scaleX = scale 2 1
    scale mn mx v = v * (mx + mn) - mn


------------------------------------------------------------------------------
mandelbrot :: Double -> Int -> Complex Double -> (Complex Double, Int)
mandelbrot limit iter v = foldl (mandelbrot' limit) (v, 0) $ replicate iter v
  where
    mandelbrot' limit l@(z, i) c | magnitude z < limit = (z * z + c, i + 1)
                                 | otherwise = l
