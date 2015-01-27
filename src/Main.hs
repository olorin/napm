module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Text           (Text)
import qualified Data.Text           as T
import           System.Directory
import           System.IO

getPassword :: IO Text
getPassword = getPassword' >>= passThroughAction (putStrLn "")
  where
    getPassword' = liftM T.pack $ putStr "Seed: "
                   >>  hFlush stdout
                   >>  hSetEcho stdin False
                   >>  getLine

    passThroughAction act x = act >> return x

napmDataDir :: IO (Either SomeException FilePath)
napmDataDir = try $ getAppUserDataDirectory "napm"

main :: IO ()
main = error "NYI"
