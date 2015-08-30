{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Monad

import           Data.Monoid
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Napm.Password
import           Napm.Types

import           Test.Hspec
import           Test.QuickCheck

tuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
tuple = (,) <$> arbitrary <*> arbitrary

triple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
triple = (,,) <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = hspec suite

suite :: Spec
suite = describe "computePassword" $ do
    it "hashes a constant context to a consistent value" $
        computePassword (PasswordLength 12) (Domain "c0nt3xt") (Passphrase "s33d") `shouldBe` "xwR3ziEmkGc7"

    it "generates a password of the requested length" $
        forAll triple $ \(l@(PasswordLength m),p,d) ->
          T.length (computePassword l p d) === m
