{- |
Module      :   Main
Description :   Web-application which allows users to input handwritten digits to a neural network.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc
               
                Date: 9.8.2018
                This module handles everything related to initializing and running the web application, including the processing of user inputted digits.

                Scotty has been chosen to be used as the web framework for this project. As you can see from the code, serving 
                GET- and POST-requests is very easy with scotty.
                
                For image processing, I am using the JuicyPixels library.
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Codec.Picture
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSP (putStrLn)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy as TX
import qualified Data.Vector.Storable as VS
import Data.Word
import Web.Scotty

import qualified Teaching as T
import qualified NeuralNetwork as NN

htmlPath = "web\\index.html"
picPath = "data\\digit.png"

defaultPath = "Data\\defaultNet.txt"

-- Main function, which initializes the web application to localhost:3000/
main = scotty 3000 $
        digitRec

-- a scotty monad, which houses the application.
digitRec :: ScottyM ()
digitRec = do
        get "/" showPage
        post "/" guess

-- Presents the page for the user to see.
showPage :: ActionM ()
showPage = do
        setHeader "Content-Type" "text/html"
        file htmlPath

-- Processes a HTTP POST request, containing the user inputted digit.
guess :: ActionM ()
guess = do
        liftIO $ print "I'm about to serve a request!"
        b <- body
        net <-  liftIO $  NN.readFromFile defaultPath -- Loads the default network from file.
        let drpd = BSL.drop 22 b -- Drop needless prefix created by "dataToUrl". (html)
        let bs = B64.decodeLenient(drpd) -- Decodes Base64 encoding.
        let strict = BSL.toStrict bs          
        img <- liftIO $ getImage strict
        --liftIO $ saveImageAsPng strict
        let asInts = map fromIntegral img
        let inverted = invertValues asInts
        let input = makeGreyScale inverted
        let normalizedInput = map (/255.0) input -- Normalizes the input values to range from 0 to 1.
        let result = T.classify net normalizedInput T.reLuAcF
        liftIO $ print ("User inputted digit classified as a " ++ (show result))
        text $ TX.pack (show result)

-- Function for turning a ByteString of a png-file to a list of RGB pixel values in Word8.
getImage :: BS.ByteString -> IO [Word8]
getImage bs = do
        let img = case decodePng bs of
                    Right pic -> pic 
                    otherwise -> error "Picture decoding failed."
        let rgb = convertRGB8 img
        let vec = imageData rgb
        return $ VS.toList vec

-- Function for saving an image to your disc. Default path data/digit.png.
-- Takes a bytestring encoding of a .png file.
saveImageAsPng :: BS.ByteString -> IO ()
saveImageAsPng bs = do
                    case (decodePng bs) of
                            Left err    -> error err
                            Right pic   -> savePngImage picPath pic

-- Turns a list of RGB pixel values to single pixel luminance values.
makeGreyScale :: [Int] -> [Double]
makeGreyScale lst = helper lst []
        where   helper (r:g:b:xs) ans   = helper xs (fromIntegral (bw r g b) : ans)
                helper [] ans           = reverse ans 
                bw red green blue       = round ((0.2126 * (fromIntegral red)) + (0.7152 * (fromIntegral green)) + 
                                            (0.0722 * (fromIntegral blue))) -- Transforms RGB-pixel values to single pixel luminance.

-- Inverts a pictures pixel values, so that: 255 = black and 0 = White.
invertValues :: [Int] -> [Int]
invertValues lst = helper lst []
        where   helper (r:g:b:xs) ans   = helper xs (255 - r: 255 - g: 255 - b:ans)
                helper [] ans           = reverse ans