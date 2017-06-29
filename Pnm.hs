{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
module Pnm
    ( Gray
    , Pixel (Pixel)

    , writePnm
    ) where

------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import           Control.Monad   (when)
import           Data.List       (intersperse)
import           Data.Monoid     ((<>))
import           Data.Text       (Text, pack)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Data.Word       (Word8)
import           GHC.Generics    (Generic)
import           System.IO       (hFlush, stdout)


------------------------------------------------------------------------------
type Dimension = (Int, Int)


------------------------------------------------------------------------------
type Threshold = Int


------------------------------------------------------------------------------
data PnmKind = BW | Grey | RGB
    deriving (Eq)

type Gray = Word8

data Pixel = Pixel Word8 Word8 Word8
  deriving (Generic)

instance NFData Pixel


------------------------------------------------------------------------------
class PnmRender a where
    pnmRender :: a -> Text
    pnmKind :: [a] -> PnmKind

instance PnmRender Bool where
    pnmRender True  = "1"
    pnmRender False = "0"
    pnmKind _ = BW

instance PnmRender Word8 where
    pnmRender = render
    pnmKind _ = Grey

instance PnmRender Pixel where
    pnmRender (Pixel r g b) =
        pnmRender r <> " " <> pnmRender g <> " " <> pnmRender b
    pnmKind _ = RGB


------------------------------------------------------------------------------
writePnm :: PnmRender a
         => [a] -> Threshold -> Dimension -> IO ()
writePnm pixels depth (w, h) = do
    let kind = pnmKind pixels
    T.putStrLn (toHeader kind)
    T.putStrLn dim
    when (kind /= BW) $
        T.putStrLn (render depth)
    mapM_ T.putStr $ intersperse " " (map pnmRender pixels)
    T.putStr "\n"
    hFlush stdout
  where
    toHeader BW   = "P1"
    toHeader Grey = "P2"
    toHeader RGB  = "P3"
    dim = render w <> " " <> render h


------------------------------------------------------------------------------
render :: Show a => a -> Text
render = pack . show
