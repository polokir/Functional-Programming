{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture
import Codec.Picture.Gif
import Data.Bits
import qualified Data.ByteString as BS

-- Функція для приховування інформації у GIF-зображенні
hideMessageInGIF :: FilePath -> FilePath -> String -> IO ()
hideMessageInGIF inputPath outputPath message = do
    gifImage <- readGif inputPath

    let transformedGif = fmap (transformFrame message) gifImage

    case transformedGif of
        Left err -> putStrLn $ "Error: " ++ err
        Right newGif -> writeGif outputPath newGif

transformFrame :: String -> GifFrame -> GifFrame
transformFrame message (GifFrame delays disposal method palette imgData) =
    GifFrame delays disposal method palette (BS.pack modifiedPixels)
  where
    pixels = BS.unpack imgData

    messageBytes = BS.unpack $ BS.pack $ map fromEnum message

    modifiedPixels = zipWith (\pixel byte -> (pixel .&. 0xFE) .|. (byte .&. 0x01)) pixels (cycle messageBytes)

main :: IO ()
main = do
    hideMessageInGIF "input.gif" "output.gif" "Hello, Haskell!"
