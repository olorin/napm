{-# LANGUAGE OverloadedStrings #-}

module Napm.ContextFile () where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Trifecta

type Context = (Text, Int)

comment :: Parser ()
comment =  string "--" >> manyTill (try newLine) >> return ()
