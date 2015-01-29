{-# LANGUAGE OverloadedStrings #-}

module Napm.ContextFile (
    PasswordContext
) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Trifecta

-- |The context in which a password is generated. Consists of a textual
--  context that's not sensitive and easy for the user to remember
--  (hostname, URL, whatever), plus the length of the generated
--  password. Contexts are not considered sensitive, although they
--  contain information which may aid an attacker and so should not be
--  accessible by anyone other than the user.
type PasswordContext = (Text, Integer)

comment :: Parser ()
comment =  string "--" >> manyTill anyChar (try newline) >> return ()

context :: Parser String
context =  many (noneOf ":")

passwordLength :: Parser Integer
passwordLength =  decimal <* spaces

passwordContext :: Parser PasswordContext
passwordContext =  (,) <$>
                   (T.pack <$> context) <*>
                   (char ':' *> passwordLength)
