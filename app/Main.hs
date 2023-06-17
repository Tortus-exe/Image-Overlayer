{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Environment
import System.FilePath
import qualified Data.Vector.Storable as V

import Codec.Picture

data Config = Config {
        baseImage :: String,
        outputImageDirectory :: String,
        newImagePosition :: (Int, Int)
                 }
        deriving (Eq, Show, Read)

readConfig :: FilePath -> IO Config
readConfig loc = do
    configString <- readFile loc
    return $ read configString

loadImage :: FilePath -> IO (Image PixelRGB8)
loadImage fp = do
    img <- either error return =<< readImage fp
    return $ convertRGB8 img

applyMask :: V.Storable a => [Int] -> V.Vector a -> V.Vector a -> V.Vector a
applyMask mask base overlay = V.fromList [if x < 0 then base V.! ((-x)-1) else overlay V.! x | x<-mask]

genMask :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [Int]
genMask (xp,y) (wp,h) (wlp,hl) = zipWith (\x y -> if y < 0 then (-x) else y) [1..] $ concat $ [[(-1)|_<-[1..wl]]|_<-[1..y]] ++ [[(-1)|_<-[1..x]] ++ [w*r..w*(r+1)-1] ++ [(-1)|_<-[w+x..wl-1]] | r <- [0..h-1]] ++ [[(-1)|_<-[1..wl]]|_<-[1..hl-y-h]]
    where
        x = xp*3
        w = wp*3
        wl = wlp*3

overlayImage :: Config -> Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
overlayImage conf base over = 
    Image {
        imageWidth = imageWidth base,
        imageHeight = imageHeight base,
        imageData = applyMask (genMask (x,y) (overWidth, overHeight) (imageWidth base,imageHeight base)) baseData overData
    }
    where baseData = imageData base
          overData = imageData over
          overWidth = imageWidth over
          overHeight = imageHeight over
          x = fst $ newImagePosition conf
          y = snd $ newImagePosition conf

outputImage :: Config -> Image PixelRGB8 -> Event -> IO ()
outputImage config base (Modified path time isDir) = 
    if (isDir == IsFile) && (takeExtension path == ".png") then do
        print $ "MODIFIED:  " <> path
        overlay <- loadImage path
        print $ [imageWidth base, imageHeight base]
        print $ length $ genMask (0,0) (0,0) (imageWidth base, imageHeight base)
        print $ V.length $ imageData base
        -- print $ length $ genMask (newImagePosition config) (0,0) (imageWidth base, imageHeight base)
        let overlaidImage = overlayImage config base overlay
        -- writePng (outputImageDirectory config ++ (takeFileName path)) overlaidImage
        saveJpgImage 8 (outputImageDirectory config ++ (takeFileName path)) (ImageRGB8 overlaidImage)
        print $ V.length $ imageData overlaidImage
    else return ()
outputImage _ _ _ = return ()

main :: IO ()
main = withManager $ \mgr -> do
    location <- getArgs
    config <- readConfig $ head location
    image <- loadImage $ baseImage config
    print $ imageWidth image
    watchTree mgr "." (const True) (outputImage config image)
    forever $ threadDelay 1000000
