{- |
Module      :   NeuralNetwork
Description :   A simple feed-forward neural network.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc

                This module handles everything related to assembling a neural network and propagating through it.

                Guidance for the implementation of this module has been drawn from the tutorial "A Functional Approach to Neural Networks" (The Monad.Reader, Issue 21, 29.3.2013).
                Link to the issue: https://themonadreader.files.wordpress.com/2013/03/issue214.pdf (Accessed: 5.6.2018)
-}

module NeuralNetwork where

-- Specifies an activation function. Storing the specification this way makes it easy to define
-- multiple different activation functions and attempt to find the most suitable function for the problem.
-- The first derivative of the activation function is stored for convenience, as it is needed in backpropagation.
data ActivationSpec = ActivationSpec {
                        acFunc :: Double -> Double, -- The activation function.
                        acFunc' :: Double -> Double, -- First derivative of the activation function.
                        description :: String -- What kind of activation function is being used.
}

-- A layer in the network consists of a matrix of weights and the activation specification.
data Layer = Layer {
                layerWeights :: [[Double]],
                layerAcSpec :: ActivationSpec
}

data BackpropNet = BackpropNet {
                    layers :: [Layer],
                    learningRate :: Double -- Parameter which controls the rate at which the network learns new patterns.                   
}

-- When assembling a network, the outputs of a layer must match the inputs of the following layer.
checkDimensions :: [[Double]] -> [[Double]] -> [[Double]]
checkDimensions w1 w2 = case ((1 + length w1) == (length (head w2))) of
                            True -> w2
                            _    -> error "Unmatching dimensions in weight matrix."

-- Function which assembles a neural net for suitable for backpropagation.
-- The function makes sure the dimensions between layers match.
assembleBackpropNet :: Double -> [[[Double]]] -> ActivationSpec -> BackpropNet
assembleBackpropNet lrnRate weights aSpec = BackpropNet {layers = ls, learningRate = lrnRate}
    where   checkedWeights = scanl1 checkDimensions weights
            ls = map assembleLayer checkedWeights
            assembleLayer ws = Layer {layerWeights = ws, layerAcSpec = aSpec}

-- There are two types of propagated layers.
-- For the first layer of the network no processing is performed, and the input is distributed as output without any changes.
-- For the other layers the propagation process includes processing and data for every layer is stored.
data PropagatedLayer    =  PropagatedSensorLayer {
                                pOut :: [Double] -- Output from this layer.
}
                        |   PropagatedLayer {
                                pIn :: [Double], -- Input to this layer.
                                pOut :: [Double], -- Output from this layer.
                                pAcFunc' :: [Double], -- Value of the first derivative of the activation function for this layer.
                                pWeights :: [[Double]], 
                                pAcSpec :: ActivationSpec
}
            
-- Functions which propagates through a single layer.
propagate :: PropagatedLayer -> Layer -> PropagatedLayer
propagate prevLayer nextLayer = PropagatedLayer {
                            pIn = input,
                            pOut = output,
                            pAcFunc' = derivOut, 
                            pWeights = ws,
                            pAcSpec = layerAcSpec nextLayer
                        }
    where   input = pOut prevLayer
            ws = layerWeights nextLayer
            a = matMultip ws input 
            acFun = acFunc (layerAcSpec nextLayer)
            output = map acFun a 
            acFun' = acFunc' (layerAcSpec nextLayer)
            derivOut = map acFun' a

-- This function (forward) propagates through the whole network.
-- Takes the input ("sensor layer", and the rest of the network and return the propagated layers
-- for the whole network.
propagateNet :: [Double] -> BackpropNet -> [PropagatedLayer]
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
validateInput :: BackpropNet -> [Double] -> [Double]
validateInput network = validateInputValues . validateInputDimensions 
        where   validateInputValues input = case ((minimum input >= 0) && (maximum input <= 1)) of
                                                        True    -> input 
                                                        _       -> error "Input values outside of range [0,1]"
                validateInputDimensions input = case (got == expected) of
                                                        True    -> input
                                                        _       -> error "The input isn't the correct length"
                        where   got = length input
                                expected = length $ head $ layerWeights $ head (layers network)

-- Performs matrix multiplication. An inefficient function, but works for this purpose.
matMultip :: [[Double]] -> [Double] -> [Double]
matMultip ws ins = case (all (== len) lrs) of
                        True    -> map (\r -> sum $ zipWith (*) r ins) ws
                        _       -> error "Unmatching dimensions when multiplaying matrices."
        where   lrs = map length ws 
                len = length ins

