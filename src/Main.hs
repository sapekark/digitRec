{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BS
import Web.Scotty


htmlPath = "web\\index.html"

main = scotty 3000 $
    digitRec

digitRec :: ScottyM ()
digitRec = do
    get "/" showPage
    post "/" guess

showPage :: ActionM ()
showPage = do
    setHeader "Content-Type" "text/html"
    file htmlPath

guess :: ActionM ()
guess = do
    b <- body
    liftIO $ print "I'm about to serve a request!"
    liftIO $ print $ BS.length b
    liftIO $ BS.putStrLn b
    text $ "1"