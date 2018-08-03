{- |
Module      :   NeuralNetwork
Description :   A simple feed-forward neural network.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   non-portable

                This module handles everything related to assembling a neural network and propagating through it.

                Guidance for the implementation of this module has been drawn from the tutorial "A Functional Approach to Neural Networks" (The Monad.Reader, Issue 21, 29.3.2013).
                Link to the issue: https://themonadreader.files.wordpress.com/2013/03/issue214.pdf (Accessed: 5.6.2018)
-}

{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module NeuralNetwork where

import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

-- Specifies an activation function. Storing the specification this way makes it easy to define
-- multiple different activation functions and attempt to find the most suitable function for the problem.
newtype ActivationFunc = ActivationFunc {
                acFunc :: forall a. Floating a => a -> a
}

-- A layer in the network consists of a matrix of weights and the activation function.
data Layer a = Layer {
                layerWeights :: [[a]],
                layerAcFunc :: ActivationFunc
} deriving (Functor, Foldable, Traversable)

data NeuralNet a = NeuralNet {
                    layers :: [Layer a],
                    learningRate :: Double -- Parameter which controls the rate at which the network learns new patterns.                   
} deriving (Functor, Foldable, Traversable)

-- When assembling a network, the outputs of a layer must match the inputs of the following layer.
checkDimensions :: [[a]] -> [[a]] -> [[a]]
checkDimensions w1 w2 = case ((1 + length w1) == (length (head w2))) of
                            True -> w2
                            _    -> error "Unmatching dimensions in weight matrix."

-- Function which assembles a neural net.
-- The function makes sure the dimensions between layers match.
assembleNNet :: Double -> [[[a]]] -> ActivationFunc -> NeuralNet a
assembleNNet lrnRate weights aFunc = NeuralNet {layers = ls, learningRate = lrnRate}
    where   checkedWeights = scanl1 checkDimensions weights
            ls = map assembleLayer checkedWeights
            assembleLayer ws = Layer {layerWeights = ws, layerAcFunc = aFunc}

-- There are two types of propagated layers.
-- For the first layer of the network no processing is performed, and the input is distributed as output without any changes.
-- For the other layers the propagation process includes processing and data for every layer is stored.
-- Derivative is not needed when using automated differentation, so it's commented out.
data PropagatedLayer a   =  PropagatedSensorLayer {
                                pOut :: [a] -- Output from this layer.
}
                        |   PropagatedLayer {
                                pIn :: [a], -- Input to this layer.
                                pOut :: [a], -- Output from this layer.
                                -- pFunc'Val :: [Double], -- Value of the first derivative of the activation function for this layer.
                                pWeights :: [[a]], 
                                pAcFunc :: ActivationFunc
} deriving (Functor, Foldable, Traversable)
            
-- Functions which propagates through a single layer.
-- We add 1 to the inputs of each layer to give bias.
propagate :: (Floating a, Show a) => PropagatedLayer a -> Layer a -> PropagatedLayer a
propagate prevLayer nextLayer = PropagatedLayer {
                            pIn = input,
                            pOut = output,
                            pWeights = ws,
                            pAcFunc = layerAcFunc nextLayer
                        }
    where   input = pOut prevLayer
            ws = layerWeights nextLayer
            a = ws <**> (1:input)
            acFun = acFunc (layerAcFunc nextLayer)
            output = map acFun a 


-- This function (forward) propagates through the whole network.
-- Takes the input ("sensor layer", and the rest of the network and return the propagated layers
-- for the whole network.
propagateNet :: (Floating a, Ord a, Show a) => [a] -> NeuralNet a -> [PropagatedLayer a]
propagateNet input network = tail calculations
        where   calculations = scanl propagate sensorL (layers network)
                sensorL = PropagatedSensorLayer {pOut = validated}
                validated = validateInput network input 

-- We want to make sure the the input is in a valid form.
-- We make sure of two things:
--      1.      The input has is of the correct length.
--      2.      The elements are withing the range of [0,1]
--              (MNIST-data is black and white, so we will be normalizing it to:
--                0 == White, 1 == Black.)
validateInput :: (Floating a, Ord a, Show a) => NeuralNet a -> [a] -> [a]
validateInput network = validateInputValues . validateInputDimensions 
        where   validateInputValues input = case ((minimum input >= 0) && (maximum input <= 1)) of
                                                        True    -> input 
                                                        _       -> error "Input values outside of range [0,1]"
                validateInputDimensions input = case (got == expected) of
                                                        True    -> input
                                                        _       -> error "The input isn't the correct length"
                        where   got = length input
                                expected = (+(negate 1)) $ length $ head $ layerWeights $ head (layers network)

-- Handy operator for the matrix multiplication function.
(<**>) :: Num a => [[a]] -> [a] -> [a]
x <**> y = matMultip x y

-- Performs matrix multiplication. An inefficient function, but works for this purpose.
matMultip :: Num a => [[a]] -> [a] -> [a]
matMultip ws ins = case (all (== len) lrs) of
                        True    -> map (\r -> sum $ zipWith (*) r ins) ws
                        _       -> error ("Unmatching dimensions when multiplaying matrices. lrs = " ++ (show (length (last ws))) ++ " and len = " ++ (show len))
        where   lrs = map length ws 
                len = length ins

-- Extracts the weights from a net and returns them as a list.
extractWeights :: NeuralNet a -> [[[a]]]
extractWeights net = map layerWeights $ layers net 

-- Function which propagates through the neural net, providing the output
-- created from the input.
runNNet :: (Floating a, Ord a, Show a) => NeuralNet a -> [a] -> [a]
runNNet net input = pOut $ last calculations 
        where   calculations = propagateNet input net