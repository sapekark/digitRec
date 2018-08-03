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
import Data.List.Split
import qualified Data.ByteString.Lazy as BS

import Control.Monad
import Control.Monad.State

import Data.Random ()
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Uniform
import Data.RVar

imgPath = "Data\\train-images.idx3-ubyte"
labPath = "Data\\train-labels.idx1-ubyte"
testPath = "Data\\t10k-images.idx3-ubyte"
testLabPath = "Data\\t10k-labels.idx1-ubyte"
lRate = 0.007
digitsToBeTrained = 100


logit :: Floating a => a -> a 
logit x = 1 / (1 + exp (negate x))

logitAS :: ActivationFunc
logitAS = ActivationFunc {acFunc = logit}

randomWeightMatrix :: Int -> Int -> [[Double]]
randomWeightMatrix numInputs numOutputs = y
   where
        y = chunksOf numInputs weights
        weights = map (/ 100.0) $ uniforms (numOutputs * numInputs)

initNet = assembleNNet lRate [w1,w2] logitAS
    where   w1 = randomWeightMatrix (28*28 + 1) 30 
            w2 = randomWeightMatrix 31 10

func :: IO ()
func = do 
    let net = initNet
    imag0 <- BS.readFile imgPath
    lab0 <- BS.readFile labPath
    let imag1 = getImages imag0 digitsToBeTrained 
    let lab1 = getLabels lab0 digitsToBeTrained  
    let labVec = V.fromList lab1
    let imgVec = V.fromList imag1
    let solutions = drop 100 $ estimate labVec imgVec net
    test0 <- BS.readFile testPath
    let test1 = getImage test0 0
    let testNet = head solutions
    let ans = runNNet testNet test1
    putStrLn (show ans)

    

uniforms :: Int -> [Double]
uniforms n = fst $ runState (replicateM n (sampleRVar stdUniform)) (mkStdGen seed)
        where   seed = 0

getImages :: BS.ByteString -> Int -> [[Double]] 
getImages imgs max = helper max []
        where helper n xy = case (n >= 0) of
                                True -> helper (n - 1) (getImage imgs n : xy)
                                False -> xy

getImage :: BS.ByteString -> Int -> [Double]
getImage s n = map ( / 255.0) (fromIntegral . BS.index s . ((fromIntegral n*28^2 + 16) +) <$> [0..28^2 - 1])

getLabels :: BS.ByteString -> Int -> [Int]
getLabels labs max = helper max []
        where helper n xy = case (n >= 0) of
                                True    -> helper (n-1) (getLabel labs n : xy)
                                False   -> xy
getLabel :: BS.ByteString -> Int -> Int
getLabel s n = fromIntegral $ BS.index s (fromIntegral(n + 8))


--try = do 
--        fr <- BS.readFile testLabPath
--        let imgs = getLabel fr 0
--        putStrLn (show imgs)