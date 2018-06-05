{- |
Module      :   NeuralNetwork
Description :   A simple feed-forward neural network.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc

                This module handles everything related to assembling a neural network on propagating through it.

                Guidance for the implementation of this module has been drawn from the tutorial "A Functional Approach to Neural Networks" (The Monad.Reader, Issue 21, 29.3.2013).
                Link to the issue: https://themonadreader.files.wordpress.com/2013/03/issue214.pdf (Accessed: 5.6.2018)
-}

module NeuralNetwork where

import Numeric.LinearAlgebra as Hmatrix

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
                layerWeights :: Matrix Double,
                layerAcSpec :: ActivationSpec
}

data BackpropNet = BackpropNet {
                    layers :: [Layer],
                    learningRate :: Double -- Parameter which controls the rate at which the network learns new patterns.                   
}

-- When assembling a network, the outputs of a layer must match the inputs of the following layer.
checkDimensions :: Matrix Double -> Matrix Double -> Matrix Double
checkDimensions w1 w2 = case (rows w1 == cols w2) of
                            True -> w2
                            otherwise -> error "Unmatching dimensions in weight matrix."

-- Function which assembles a neural net for suitable for backpropagation.
-- The function makes sure the dimensions between layers match.
assembleBackpropNet :: Double -> [Matrix Double] -> ActivatinSpec -> BackpropNet
assembleBackpropNet lrnRate weights aSpec = BackpropNet {layers = ls, learningRate = lrnRate}
    where   checkedWeights = scanl1 checkDimensions weights
            ls = map assembleLayer checkedWeights
            assembleLayer ws = Layer {layerWeights = ws, layerAcSpec = aSpec}

-- There are two types of propagated layers.
-- For the first layer of the network no processing is performed, and the input is distributed as output without any changes.
-- For the other layers the propagation process includes processing and data for every layer is stored.
data PropagatedLayer    =  PropagatedSensorLayer {
                                pOut :: Matrix Double -- Output from this layer.
}
                        |   PropagatedLayer {
                                pIn :: Matrix Double, -- Input to this layer.
                                pOut :: Matrix Double, -- Output from this layer.
                                pAcFunc' :: Matrix Double, -- Value of the first derivative of the activation function for this layer.
                                pWeights :: Matrix Double, 
                                pAcSpec :: ActivationSpec
}
            
-- Functions which propagates through a single layer.
propagate :: PropagatedLayer -> Layer -> PropagatedLayer
propagate prevLayer nextLayer = PropagatedLayer {
                            pIn = input,
                            pOut = output,
                            pAcFunc' = derivOut, 
                            pWeifhts = ws,
                            pAcSpec = layerAcSpec nextLayer
                        }
    where   input = pOut prevLayer
            ws = layerWeights nextLayer
            a = ws <> x -- <> performs matrix multiplication, as defined in the hmatrix package.
            acFun = acFunc (layerAcSpec nextLayer)
            output = mapMatrix acFun input 
            acFun' = acFunc' (layerAcSpec nextLayer)
            derivOut = mapMatrix acFun' input

-- Helpful function which applies a function to each element of a matrix.
mapMatrix :: (Field a) => (a -> a) -> Matrix a -> Matrix a
mapMatrix f m = (rs Hmatrix.>< cs) y 
    where   x' = toList (flatten m)
            y = map f x' 
            rs = rows m 
            cs = cols m