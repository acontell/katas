{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text hiding (map)
import Data.Functor
import Control.Concurrent (threadDelay)

import qualified Network.WebSockets as WS

import Conway

-- Conway's universe size. Bonus: could be customized by the client sending the size in headers or request params.
size :: Int
size = 50

seconds :: Int -> Int
seconds = (* 1000000)

fps :: Int -> Int
fps = seconds . quot 1

updatesPerSecond :: Int
updatesPerSecond = fps 1

main :: IO ()
main = do
    WS.runServer "127.0.0.1" 9160 $ application

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    universe <- conway size
    send conn universe

send :: WS.Connection -> [Universe] -> IO ()
send conn (state:states) = do
    WS.sendTextData conn (toText state)
    threadDelay updatesPerSecond
    send conn states

toText :: Universe -> Text
toText = pack . show . (map (map show))