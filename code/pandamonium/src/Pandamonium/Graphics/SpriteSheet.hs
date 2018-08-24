module Pandamonium.Graphics.SpriteSheet (loadSpriteSheet) where

import Graphics.Gloss
import Codec.BMP
import qualified Data.ByteString as B
import Data.Word
import Debug.Trace

loadBMP' :: String -> IO BMP
loadBMP' filename = do picture <- readBMP filename
                       case picture of
                         (Right sprite) -> return sprite
                         (Left e)       -> error $ show e

splitSprites :: Int -> Int -> BMP -> [B.ByteString]
splitSprites w h pic = let (x, y) = bmpDimensions pic
                           cols = x `quot` w
                           rows = y `quot` h
                        in fst $ sprites (w * 4) cols h rows (unpackBMPToRGBA32 pic)

-- takes single pixel rows from the spritesheet
segments :: Int -> Int -> B.ByteString -> ([B.ByteString], B.ByteString)
segments w 0    bytes     = ([], bytes)
segments w cols bytes = let (slice, rest) = B.splitAt w bytes
                            (slices, leftovers) = segments w (cols - 1) rest
                         in (slice : slices, leftovers)

-- takes layers of the spritesheet, and composes them into a row of sprites
layers :: Int -> Int -> Int -> B.ByteString -> ([B.ByteString], B.ByteString)
layers w cols 0 bytes = (replicate cols B.empty, bytes)
layers w cols h bytes = let (slices, rest) = segments w cols bytes
                            (layer, leftovers) = layers w cols (h - 1) rest
                         in (zipWith B.append slices layer, leftovers)

-- reads rows of sprites from the spritesheet, returning them all as a list
sprites :: Int -> Int -> Int -> Int -> B.ByteString -> ([B.ByteString], B.ByteString)
sprites w cols h 0    bytes = ([], bytes)
sprites w cols h rows bytes = let (row, rest) = layers w cols h bytes
                                  (rows', leftovers) = sprites w cols h (rows - 1) rest
                               in (row ++ rows', leftovers)

setTransparent :: B.ByteString -> B.ByteString
setTransparent bytes = B.pack $ setTransparent' $ B.unpack bytes where
  setTransparent' :: [ Word8 ] -> [ Word8 ]
  setTransparent' [] = []
  setTransparent' bytes = let (pixel, rest) = splitAt 4 bytes
                              newPixel = makeTransparent pixel
                           in newPixel ++ setTransparent' rest where
    makeTransparent :: [ Word8 ] -> [ Word8 ]
    makeTransparent [0, 0, 0, _] = [0, 0, 0, 0]
    makeTransparent x = x




loadSpriteSheet :: Int -> Int -> String -> IO [Picture]
loadSpriteSheet w h file = do sprite <- loadBMP' file
                              let bytes = splitSprites w h sprite
                              let transparency = setTransparent <$> bytes
                              let sprites = packRGBA32ToBMP w h <$> transparency
                              return $ bitmapOfBMP <$> sprites
