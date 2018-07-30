{- |
Module      :   Testing
Description :   Tests for the neural net.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc              

                This module handles everything related to testing the system, attempting to classify MNIST-data correctly.

-}

module Testing where

import NeuralNetwork
import Training 
import qualified Data.Vector as V
import System.Random
import Data.IDX
import Data.List.Split

import Control.Monad
import Control.Monad.State

import Data.Random ()
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Uniform
import Data.RVar

imgPath = "Data\\train-images.idx3-ubyte"
labPath = "Data\\train-labels.idx1-ubyte"
lRate = 0.007

logit :: Double -> Double 
logit x = 1 / (1 + exp (negate x))

logitAS :: ActivationSpec
logitAS = ActivationSpec {acFunc = logit, description = "The logit function, inverse of sigmoidal logistic function."}

randomWeightMatrix :: Int -> Int -> [[Double]]
randomWeightMatrix numInputs numOutputs = y
   where
        y = chunksOf numInputs weights
        weights = map (/ 100.0) $ uniforms (numOutputs * numInputs)

initNet = assembleNNet lRate [w1,w2] logitAS
    where   w1 = randomWeightMatrix (28*28 + 1) 20 
            w2 = randomWeightMatrix 20 10

func :: IO ()
func = do 
    let net = initNet
    Just imag <- decodeIDXFile imgPath
    Just lab <- decodeIDXLabelsFile labPath
    let Just lst = labeledDoubleData lab imag
    let (lab0, digits) = unzip lst 
    let lab1 = V.fromList lab0
    -- let solutions = drop 100 $ estimates lab1 digits net
    putStrLn (show ((length lab0)))

    

uniforms :: Int -> [Double]
uniforms n = fst $ runState (replicateM n (sampleRVar stdUniform)) (mkStdGen seed)
        where   seed = 0

