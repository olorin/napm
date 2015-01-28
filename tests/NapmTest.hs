{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import           Data.Text     (Text)
import           Test.Hspec

import           Napm.Password

main :: IO ()
main = hspec suite

suite :: Spec
suite = describe "computePassword" $
    it "hashes an alphanumeric passphrase and context" $
        computePassword 12 "s33d" "c0nt3xt" `shouldBe` "xwR3ziEmkGc7"
