{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Monoid
import           Data.Text     (Text)
import qualified Data.Text     as T
import           System.Random
import           Test.Hspec

import           Napm.Password

main :: IO ()
main = hspec suite

suite :: Spec
suite = describe "computePassword" $ do
    it "hashes an alphanumeric passphrase and context" $
        computePassword 12 "s33d" "c0nt3xt" `shouldBe` "xwR3ziEmkGc7"

    it "generates a password of the requested (short) length" $ do
        l <- getRandomLength
        s <- getRandomText 12
        c <- getRandomText 12
        T.length (computePassword l s c) `shouldBe` l

getRandomLength :: IO Int
getRandomLength = getStdRandom (randomR (1,50))

getRandomText :: Int -> IO Text
getRandomText n = liftM (T.pack . take n . randoms) getStdGen
