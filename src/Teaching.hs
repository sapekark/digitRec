{- |
Module      :   Teaching
Description :   The neural network is taught to classify MNIST-data.
Copyright   :   (c) Saku Kärkkäinen
License     :   MIT

Maintainer  :   sapekark@student.jyu.fi
Stability   :   experimental
Portability :   ghc              

                This module handles everything related to teaching the system to classify MNIST-data correctly.
                
                To teach a new network simply type "teach" into ghci. A network will be taught with the full dataset
                and it will be saved in the data folder, if you wish to use it again.
                Trained models usually reach around 90% accuracy, networks with more hidden nodes performing better.
-}

module Teaching where

import NeuralNetwork
import Training 
import MNIST

        
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Ord
import System.Random
import System.Random.Shuffle
        
        
digPath = "Data\\train-images.idx3-ubyte"
labPath = "Data\\train-labels.idx1-ubyte"
testPath = "Data\\t10k-images.idx3-ubyte"
testLabPath = "Data\\t10k-labels.idx1-ubyte"
        
saveNetPath = "Data\\solutionNet.txt"
        
digitsToBeTrained = 60000

digitsToBeTested = 10000
        
-- The rectifier function, which we will be using as the activation function in this implementation.
reLu :: Double -> Double
reLu = max 0
        
-- Derivative of the rectifier function.
reLu' :: Double -> Double 
reLu' x
    | x < 0.0   = 0
    | otherwise = 1
        
-- Initializes the rectifier function to be an activation function is this implementation.
reLuAcF :: ActivationFunction
reLuAcF = ActivationFunction {acFunc = reLu, acFunc' = reLu', description = "Rectifier function"}
 
-- Function which teaches a network to classify MNIST data from scratch.
-- NOTE: This function is VERY resource heavy.
-- To decreace runtime and resource needs, decrese the amount of "digitsToBeTrained" or reduce the number of hidden layers.
teach :: IO ()
teach = do
        let n = digitsToBeTrained - 1 -- Because indexing starts from zero, we have to take the - 1.
        net <- newNNet [(28*28),100,10]
        let af = reLuAcF
        dig0 <- BS.readFile digPath
        lab0 <- BS.readFile labPath       
        let digits0 = getDigits dig0 n 
        let labels0 = getLabels lab0 n
        -- The Following code shuffles the training set, shuffled data sets generally produce better results.
        let toShfl = zip digits0 labels0
        let len = length toShfl
        gen <- getStdGen
        let shuffled = shuffle' toShfl len gen
        let (digits, labels) = unzip shuffled
        --
        let solution = teachNet net digits0 labels0 af
        writeFile saveNetPath (show solution)
        putStrLn "Network has been taught. Solution saved to file."
        testAccuracy solution


-- Function for testing an already taught network.
testAccuracy :: NeuralNet -> IO ()
testAccuracy net  = do
        let n = digitsToBeTested - 1 -- Again, because of indexing, we have to take the -1. 
        let af = reLuAcF
        test0 <- BS.readFile testPath
        testLab0 <- BS.readFile testLabPath
        let inputs = getDigits test0 n
        let labels = getLabelsTesting testLab0 n
        let correct = testNet net inputs labels af
        let ans = 100 *(fromIntegral (correct) / fromIntegral (digitsToBeTested)) 
        putStrLn ("The number of correct digits: " ++ (show correct) ++ " Out of " ++ (show digitsToBeTested) ++
                 ". Accuracy: " ++ (show ans) ++ "%")

-- Function, which teaches the neural net with the whole dataset.
-- Takes the network, data of the digits, and the labels for each digit.
-- Returns the neural net with its inner values (biases and weights) changed by the training.
teachNet :: NeuralNet -> [[Double]] -> [[Double]] -> ActivationFunction -> NeuralNet
teachNet net inputs@(x:xs) answers@(y:ys) ac = teachNet (learn x y net ac) xs ys ac
teachNet net _ [] _ = net
teachNet net [] _ _ = net 

-- Tests, how many cases the neural network classifies correctly.
-- Takes the net, list of inputs, expected answers and the activation function.
-- Returns the number digits which were correctly classified.
testNet :: NeuralNet -> [[Double]] -> [Int] -> ActivationFunction -> Int
testNet net inputs answers af = helper net inputs answers 0
        where   helper net (x:xs) (y:ys) n = case (run == y) of
                                                True            -> helper net xs ys (n+1)
                                                otherwise       -> helper net xs ys n
                        where   run     = classify net x af
                helper net [] _ n = n 
                helper net _ [] n = n

-- Function, which classifies which digit the input represents.
-- Takes the neural network, the input, the activation function.
-- Returns the digit, which the input represents.
classify :: NeuralNet -> [Double] -> ActivationFunction -> Int
classify net input af = maxIndex (runNNet input net af)
        where   maxIndex= fst . maximumBy (comparing snd) . zip [0..]