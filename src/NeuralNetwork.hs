{- |
Module      :   NeuralNetwork
Description :   A simple feed-forward neural network.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc

                This module handles everything related to assembling a neural network and propagating through it.

                Guidance and inspiration for the implementation of this module has been drawn from the tutorial "Get a brain".
                Link to the tutorial: https://crypto.stanford.edu/~blynn/haskell/brain.html (Accessed: 4.8.2018)
-}

module NeuralNetwork where

import Data.Functor
import Data.List
import System.Random
import Control.Monad
        
        
type Bias = [Double]
type Weights = [[Double]]
        
-- Type, which houses our neural network. We see our neural network as a group of layers.
-- A layer consists of a tuple, which holds the bias and the weight values of the layer.
type NeuralNet = [(Bias, Weights)]
        
-- Data type, which houses the activation function used in this impelementation.
data ActivationFunction = ActivationFunction {
                                acFunc  :: Double -> Double,
                                acFunc' :: Double -> Double, -- Derivative of the activation function.
                                description :: String -- Name of the function
}
        
-- Initializes a new neural network, wit the given dimensions.
-- Format: newNNet [2, 4, 1] => Network with 2 input neurons, 4 hidden neurons and 1 output neuron.
-- Initializes all the biases with the value 1, and weights from a normal distribution,
-- with mean 0 and standard deviation of 0.01
newNNet :: [Int] -> IO NeuralNet
newNNet dims@(_:xs) = initLrs
        where   initLrs = zip initBias <$> initWeights
                initBias = (flip replicate 1.0 <$> xs)
                initWeights = zipWithM (\x y -> replicateM y $ replicateM x $ bmt 0.01) dims xs
        
-- Computes the weighted inputs of a layer of the neural network, which is fed to the activation function.
-- Takes the input and the bias and weight values of the layer => returns the weighted inputs.
-- Follows the following formula: Wi * Xi + b 
-- Where    Wi  = i:th weight
--          Xi  = i:th input
--          b   = bias of the layer.
computeLayer :: [Double] -> (Bias, Weights) -> [Double]
computeLayer input (bs, ws) = zipWith (+) bs $ sum . zipWith(*) input <$> ws
        
-- Functions which runs the input through the neural net, calculating the output.
-- Takes the input, a neural network and the activation function => returns the calculated output of the network.
runNNet :: [Double] -> NeuralNet -> ActivationFunction -> [Double]
runNNet input net ac = foldl' (((af <$>) . ) . computeLayer) input net
        where   af = acFunc ac
        
-- A simple impelementation of the Box-Muller transformation.
-- Used to provide a sampling from a normal distribution.
bmt :: Double -> IO Double 
bmt dev = do
        x <- randomIO
        y <- randomIO 
        return $ dev * sqrt (-2 * log x) * cos (2 * pi * y)       