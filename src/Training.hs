{- |
Module      :   Training
Description :   Training algorithm for the neural network. Uses automatic differentation.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   non-portable

                This module handles everything related to training a neural network using automatic differentation..
                Training an network means determining new weights for the network, so that it provides a wanted output.

                Inspiration and guidance for the implementation of this module has been drawn from the article "Neural Networks and Automated Differentiation" (Dominic Steinitz, 31.5.2013).
                Link to the article: https://idontgetoutmuch.wordpress.com/2013/05/31/neural-networks-and-automated-differentiation-3/ (Accessed: 11.6.2018)
-}

module Training where

import Numeric.AD

import NeuralNetwork
import qualified Data.Vector as V
import Data.List

delta :: Double 
delta = 0.01

-- A cost functions for the neural network. Designed for cases when gradient descent is performed
-- over the whole training set.
networkCost :: Int -> V.Vector Int -> V.Vector [Double] -> NeuralNet -> Double 
networkCost n expected inputs net = cost 
    where   cost = (x + delta * y) / l 
            l = fromIntegral $ V.length expected 
            x = V.sum $ V.zipWith (\exp input -> costFN n exp input net) expected inputs
            y = (/(2 * m)) $ sum $ map (^2) ws 
            m = fromIntegral $ length ws 
            ws = concat $ concat $ map stripBias $ extractWeights net 
            stripBias xs = map (drop 1) xs

costFN :: Int -> Int -> [Double] -> NeuralNet -> Double 
costFN n expected input net = 0.5 * sum (map (^2) diff)
    where   diff = zipWith (-) ((targets n)!!expected) predicted
            predicted = runNNet net input
        
targets :: Int -> [[Double]]
targets n = map row [0 .. n - 1]
    where row m = concat [x, 1.0 : y]
            where (x,y) = splitAt m (take (n - 1) $ repeat 0.0)

-- By using the gradientDescent function (from the automatic differentation library),
-- this generates neural networks which fit the data.
--estimate :: V.Vector Int -> V.Vector [Double] -> NeuralNet -> [NeuralNet]
--estimate labels inputs = gradientDescent $ 
--                            \theta -> networkCost 10 labels (V.map (map auto) inputs) theta