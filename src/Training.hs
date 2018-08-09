{- |
Module      :   Training
Description :   A backpropagation algoritm for a simple feed-forward neural network.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc

                Date: 9.8.2018
                This module handles everything related to training the network through backpropagation.

                Guidance and inspiration for the implementation of this module has been drawn from the tutorial "Get a brain".
                Link to the tutorial: https://crypto.stanford.edu/~blynn/haskell/brain.html (Accessed: 4.8.2018)
-}

module Training where

import NeuralNetwork
    
import Data.Functor
import Data.List
    
learningRate = 0.002
    
-- Function, which handles teaching the network through backpropagation.
-- Takes the input of the network, the expected output, the network and the activation function.
-- Returns the network, with its layer values modified by the training process.
learn :: [Double] -> [Double] -> NeuralNet -> ActivationFunction -> NeuralNet
learn input expected net af = updatedLayers
        where   updatedLayers   = zip newBias newWeights
                newBias         = zipWith descend (fst <$> net) dlts
                newWeights      = zipWith3 (\wvs a d1 -> zipWith (\wv d2 -> descend wv ((d2*) <$> a)) wvs d1) (snd <$> net) acs dlts
                (acs, dlts)     = deltas input expected net af
                descend ac del  = zipWith (-) ac ((learningRate *) <$> del)
    
-- Function, which calculates the output error for the network.
-- Takes the input, expected output, the network and the activation function.
-- Returns a list of activations and deltas for each layer.
deltas :: [Double] -> [Double] -> NeuralNet -> ActivationFunction -> ([[Double]], [[Double]])
deltas input expected net acFun = (activations, deltas)
        where   activations             = reverse acs
                deltas                  = func (transpose . snd <$> reverse net) wis [dlt]
                (acs@(ac:_), wi:wis)    = revAcsWis input acFun net
                dlt                     = zipWith (*) (zipWith cost' ac expected) (af' <$> wi)
                af'                     = acFunc' acFun   
                func _ [] dlts          = dlts
                func (wm:wms) (wi2:wis2) dlts@(del:_) = func wms wis2 $ (:dlts) $ zipWith (*) [sum $ zipWith (*) row del | row <- wm] (af' <$> wi2)
    
-- Helper function, which calculates the weighted inputs and activations for every neuron.
-- (The values before and after activation, when propagating through the net.)
-- The values are needed for backpropagation. 
-- Takes the input, the activation function and the network.
-- Returns the (activationds, weighted inputs) for each layer.
-- As backpropagation goes through the network backwards, the values are returned in reverse order.
-- (As in, from the last layer to the first.)
revAcsWis :: [Double] -> ActivationFunction -> NeuralNet -> ([[Double]], [[Double]])
revAcsWis input ac net  = foldl' helper ([input], []) net 
        where   helper  = (\(ins@(i:_), wis) (bs, ws) -> let 
                            wi = computeLayer i (bs, ws) in (((af <$> wi):ins), (wi:wis)))
                af      = acFunc ac
     
-- Function, which calculates derivate of the cost, calculated using the de.
cost' :: Double -> Double -> Double 
cost' x y 
    | y == 1 && x >= y  = 0
    | otherwise         = x - y    