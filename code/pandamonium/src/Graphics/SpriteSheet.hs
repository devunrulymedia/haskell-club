module Graphics.SpriteSheet where

import Graphics.Gloss
import Codec.BMP
import qualified Data.ByteString as B

loadBMP' :: String -> IO BMP
loadBMP' filename = do picture <- readBMP filename
                       case picture of
                         (Right sprite) -> return sprite
                         (Left e)       -> error $ show e

loadSprite :: String -> IO Picture
loadSprite filename = bitmapOfBMP <$> loadBMP' filename

splitSprites :: Int -> Int -> BMP -> [Picture]
splitSprites w h pic = case bmpDimensions pic of
  (x, y) -> let cols = x `quot` w
            -- rows = y `quot` h -- assume one row for now
                bmps = layers (w * 4) cols y (unpackBMPToRGBA32 pic)
             in bitmapOfBMP <$> packRGBA32ToBMP w h <$> bmps

segments :: Int -> Int -> B.ByteString -> ([B.ByteString], B.ByteString)
segments chunk 0 bytes     = ([], bytes)
segments chunk count bytes = let (slice, rest) = B.splitAt chunk bytes
                                 (slices, leftovers) = segments chunk (count - 1) rest
                              in (slice : slices, leftovers)

layers :: Int -> Int -> Int -> B.ByteString -> [B.ByteString]
layers chunk count 0    bytes = replicate count B.empty
layers chunk count rows bytes = let (slices, rest) = segments chunk count bytes
                                    layer = layers chunk count (rows - 1) rest
                                 in zipWith B.append slices layer

loadSpriteSheet :: Int -> Int -> String -> IO [Picture]
loadSpriteSheet w h file = splitSprites w h <$> loadBMP' file
