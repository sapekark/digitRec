{- |
Module      :   Backpropagation
Description :   Simple backpropagation algorithm for a feed-forward network..
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc

                This module handles everything related to backpropagating a neural network.
                Backpropagating is a method of determining new weights for the network, i.e training it.

                Guidance for the implementation of this module has been drawn from the tutorial "A Functional Approach to Neural Networks" (The Monad.Reader, Issue 21, 29.3.2013).
                Link to the issue: https://themonadreader.files.wordpress.com/2013/03/issue214.pdf (Accessed: 5.6.2018)
-}

module Backpropagation where

import NeuralNetwork
import Data.List (transpose)

-- This structure, just like the one for propagated layers, helps us keep track of the calculations
-- needed for backpropagation.
data BackpropagatedLayer = BackpropagatedLayer {
                            bpDazzle :: [Double], -- ∇-sub-z-sub-l of E (Error gradient for the last layer.)
                            bpErrGrad :: [Double], -- Error due to this layer.
                            bpFunc'Val :: [Double], -- The value of the first derivative of the activation function for this layer.
                            bpInput :: [Double], 
                            bpOutput :: [Double],
                            bpWeights :: [[Double]],
                            bpAcSpec :: ActivationSpec
}

-- Function which backpropagates through a single layer.
-- Note, that the backpropagation process moves backwards in the layer.
-- This means that "prevLayer" is actually a layer closer to the output (i.e end) of the network,
-- and "nextLayer" is the next layer to be backpropagated, so a layer earlier in the network.
backpropagate :: PropagatedLayer -> BackpropagatedLayer -> BackpropagatedLayer 
backpropagate nextLayer prevLayer = BackpropagatedLayer {
                                        bpDazzle = dazzle,
                                        bpErrGrad = errorGrad dazzle func'VNext (pIn nextLayer),
                                        bpFunc'Val = pFunc'Val nextLayer,
                                        bpInput = pIn nextLayer,
                                        bpOutput = pOut nextLayer, 
                                        bpWeights = pWeights nextLayer,
                                        bpAcSpec = pAcSpec nextLayer
                                    }
    where   dazzle = prevWsTrans <**> (zipWith (*) dazzlePrev func'VPrev)
            prevWsTrans = transpose (bpWeights prevLayer) -- Transposes the rows and columns of the weight matrix.
            dazzlePrev = bpDazzle prevLayer
            func'VPrev = bpFunc'Val prevLayer
            func'VNext = pFunc'Val nextLayer

-- Backpropagation for the last layer. (i.e the first layer in the network, the sensor/input layer.)
-- Takes the propagation information of the layer nad the target output.
backpropagateLastLayer :: PropagatedLayer -> [Double] -> BackpropagatedLayer
backpropagateLastLayer layer target = BackpropagatedLayer {
                                        bpDazzle = dazzle, 
                                        bpErrGrad = errorGrad dazzle f'VNext (pIn layer),
                                        bpFunc'Val = pFunc'Val layer, 
                                        bpInput = pIn layer, 
                                        bpOutput = pOut layer, 
                                        bpWeights = pWeights layer, 
                                        bpAcSpec = pAcSpec layer
                                        }
    where   dazzle = zipWith (-) (pOut layer) target 
            f'VNext = pFunc'Val layer

-- Calculates the error gradient for the layer.
errorGrad :: [Double] -> [Double] -> [Double] -> [Double]
errorGrad dzl f' input = zipWith (*) (zipWith (*) dzl f') input

-- Function that backpropagates through the whole net.
-- Takes the target result and a list of propagated layers.
backpropagateNet :: [Double] -> [PropagatedLayer] -> [BackpropagatedLayer]
backpropagateNet target layers = scanr backpropagate lastLayer hiddenLayers
        where   hiddenLayers = init layers
                lastLayer = backpropagateLastLayer (last layers) target

-- After the backpropagation, the weights of the layer are updated.
update :: Double -> BackpropagatedLayer -> Layer 
update rate layer = Layer {
                        layerWeights = newWeights,
                        layerAcSpec = bpAcSpec layer
                        }
        where   oldWeights = bpWeights layer
                delW = map (\r -> r * rate) (bpErrGrad layer)
                newWeights = map (\r -> zipWith (-) r delW) oldWeights 
