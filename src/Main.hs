module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.SHA512  as SHA
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding
import           System.Directory
import           System.IO

readPassphrase :: IO Text
readPassphrase = readPassphrase' >>= passThroughAction (putStrLn "")
  where
    readPassphrase' = liftM T.pack $ putStr "Seed: "
                   >>  hFlush stdout
                   >>  hSetEcho stdin False
                   >>  getLine

    passThroughAction act x = act >> return x

napmDataDir :: IO (Either SomeException FilePath)
napmDataDir = try $ getAppUserDataDirectory "napm"

computePassword :: Int -> Text -> Text -> Text
computePassword len passphrase context =
    T.take len . decodeUtf8 $ SHA.hash salted
  where
    salted = encodeUtf8 passphrase <> encodeUtf8 context

main :: IO ()
main = error "NYI"
