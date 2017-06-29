------------------------------------------------------------------------------
module Main where

------------------------------------------------------------------------------
import           Control.Monad               (Monad ((>>=)), join, mzero)
import           Control.Parallel.Strategies (NFData, parBuffer)
import qualified Control.Parallel.Strategies as P
import           Data.Complex                (Complex ((:+)), magnitude)
import           Data.List                   (find, isPrefixOf)
import           Data.Monoid                 ((<>))
import           Options.Applicative         hiding (helper)

------------------------------------------------------------------------------
import           Pnm


------------------------------------------------------------------------------
main :: IO ()
main = join (execParser opts)
  where
    helper = abortOption ShowHelpText
        ( long "help"
       <> help "Show this help text" )
    opts = info (helper <*> setup)
        ( fullDesc
       <> progDesc "Render a Mandelbrot fractal" )


------------------------------------------------------------------------------
setup :: Parser (IO ())
setup = makePNM
    <$> option (str >>= readType)
        ( long "mode"
       <> metavar "b/w|grey|colour"
       <> value BW
       <> help "Rendering mode" )
    <*> option auto
        ( long "width"
       <> short 'w'
       <> metavar "WIDTH"
       <> value 1920
       <> help "Image width" )
    <*> option auto
        ( long "height"
       <> short 'h'
       <> metavar "HEIGHT"
       <> value 1080
       <> help "Image height" )
  where
    readType c = maybe mzero (return . snd) $
        find (isPrefixOf c . fst) [("b/w",    BW),
                                   ("grey",   Grey),
                                   ("colour", Colour)]


------------------------------------------------------------------------------
data Mode = Colour | BW | Grey
    deriving (Show, Eq)


------------------------------------------------------------------------------
makePNM :: Mode -> Int -> Int -> IO ()
makePNM BW     w h = writePnm (makeDataBw w h)    255 (w, h)
makePNM Grey   w h = writePnm (makeDataGray w h)  255 (w, h)
makePNM Colour w h = writePnm (makeDataColor w h) 255 (w, h)


------------------------------------------------------------------------------
makeDataBw :: Int -> Int -> [Bool]
makeDataBw w h = parMap
    ((> limit) . magnitude . fst . mandelbrot limit iter . toComplexCoord w h)
    [0 .. w * h - 1]
  where
    limit = 2
    iter  = 1024


------------------------------------------------------------------------------
makeDataGray :: Int -> Int -> [Gray]
makeDataGray w h = parMap
    (truncate . threshDiv limit . magnitude . fst .
        mandelbrot limit iter . toComplexCoord w h)
    [0 .. w * h - 1]
  where
    limit = 2
    iter  = 1024


------------------------------------------------------------------------------
makeDataColor :: Int -> Int -> [Pixel]
makeDataColor w h = parMap
    (toColor iter . mandelbrot limit iter . toComplexCoord w h)
    [0 .. w * h - 1]
  where
    limit = 2
    iter = 1024


------------------------------------------------------------------------------
toColor:: Int -> (Complex Double, Int) -> Pixel
toColor limit (z, i)
    | magnitude z > 2 = Pixel
                        (truncate $ v * 255)
                        (truncate $ 125 * v + 125)
                        (truncate $ 75 + v * 75)
    | otherwise = Pixel 255 255 255
  where
    zn = magnitude z
    nu = logBase 2 (logBase 2 zn)
    iter = fromIntegral i + 1 - nu
    v = threshDiv (fromIntegral limit) iter


------------------------------------------------------------------------------
threshDiv :: Double -> Double -> Double
threshDiv b a
    | a > b     = 255
    | otherwise = a / b * 128


------------------------------------------------------------------------------
parMap :: NFData b => (a -> b) -> [a] -> [b]
parMap f = P.withStrategy (parBuffer 100 P.rdeepseq) . map f


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
mandelbrot limit iter v = mandelbrot' 0 v
  where
    mandelbrot' i z@(x :+ y)
        | i == iter || x * x + y * y >= limit * limit = (z, i)
        | otherwise = mandelbrot' (succ i) (z * z + v)
