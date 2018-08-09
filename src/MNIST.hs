{- |
Module      :   MNIST
Description :   Functions related to decoding MNIST-data from IDX-files.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc              

                Date: 9.8.2018
                This module contains a functions for decoding MNIST data from IDX-files.
                
                There are distinct functions for training data and testing data.
                (Functions for training return labels as expected output vectors,
                 Functions for testing return labels as integers, describing the digit it is labeling.)
-}

module MNIST where

import qualified Data.ByteString.Lazy as BS

-- Helper function, which collects the normalized pixel data of multiple digits.
-- Takes the ByteString data consisting of all the digits and the number of digits to be collected, and returns data for (max + 1) digits.
getDigits :: BS.ByteString -> Int -> [[Double]] 
getDigits imgs maxi = helper maxi []
        where helper n xy = case (n >= 0) of
                                True -> helper (n - 1) (getDigit imgs n : xy)
                                False -> xy
        
-- Turns the the binary pixel data of a digit in to a list of Double-values, normalized to range from 0 to 1.
-- The value describes the darkness of each pixel: 0 == White & 1 == Black
-- Takes the ByteString data of all digits, and the index of the digit to be collected. Returns the normalized pixel data.
getDigit :: BS.ByteString -> Int -> [Double]
getDigit s n = map ( / 255.0) (fromIntegral . BS.index s . ((fromIntegral n*28^2 + 16) +) <$> [0..28^2 - 1])
        
-- Takes the ByteString data of all the labels and the number of labels to be collected.
-- Returns data for (max + 1) labels. 
getLabels :: BS.ByteString -> Int -> [[Double]]
getLabels labs maxi = helper maxi []
        where helper n xy = case (n >= 0) of
                                True    -> helper (n-1) (getLabel labs n : xy)
                                False   -> xy
        
-- Returns the label of a digit as the expected output vector.
-- Example: Label = 7 -> Returns [0,0,0,0,0,0,0,1,0,0]
-- So, when the label is n, the expected output vector has 1 on the nth index. (Indexing starts from 0.)
getLabel :: BS.ByteString -> Int -> [Double]
getLabel s n = asExpOut (fromIntegral $ BS.index s (fromIntegral(n + 8)))
            where   asExpOut i   = replaceAtIndex i 1.0 initList
                    initList    = replicate 10 0.0
        
-- Helper function, which replaces the element in index i with value x.
replaceAtIndex :: Int -> a -> [a] -> [a] 
replaceAtIndex i x xs = ys ++ x:zs 
            where  (ys, _:zs) = splitAt i xs

-- getLabels-function for the purposes of testing a network.
-- Returns the labels as a list of integers, each number describing the digit it's labeling.
getLabelsTesting :: BS.ByteString -> Int -> [Int]
getLabelsTesting labs maxi = helper maxi []
        where helper n xy = case (n >= 0) of
                                True    -> helper (n-1) (getLabelTesting labs n : xy)
                                False   -> xy

-- Returns the label from the index as an integer.
getLabelTesting :: BS.ByteString -> Int -> Int
getLabelTesting s n = fromIntegral $ BS.index s (fromIntegral(n + 8))   