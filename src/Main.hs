module Main where

import System.IO
import Control.Exception

getPassword :: IO String
getPassword = getPassword' >>= passThroughAction (putStrLn "")
  where
    getPassword' =     putStr "Seed: "
                   >>  hFlush stdout
                   >>  hSetEcho stdin False
                   >>  getLine
                   >>= return

    passThroughAction act x = act >> return x

main :: IO ()
main = do
    pass <- getPassword
    putStrLn pass
