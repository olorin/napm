{-# LANGUAGE OverloadedStrings #-}

module Napm.ContextFile (
    PasswordContext,
    parseContextFile
) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta

-- |The context in which a password is generated. Consists of a textual
--  context that's not sensitive and easy for the user to remember
--  (hostname, URL, whatever), plus the length of the generated
--  password. Contexts are not considered sensitive, although they
--  contain information which may aid an attacker and so should not be
--  accessible by anyone other than the user.
type PasswordContext = (Text, Integer)

eol :: Parser ()
eol =  void $ oneOf "\n\r"

comment :: Parser ()
comment =  char '#' >> skipMany (noneOf "\r\n")

context :: Parser String
context =  many (noneOf ":")

passwordLength :: Parser Integer
passwordLength =  decimal <* (try eol <|> try comment <|> eof)

passwordContext :: Parser PasswordContext
passwordContext =  (,) <$>
                   (T.pack <$> context) <*>
                   (char ':' *> passwordLength)

contextLine :: Parser (Maybe PasswordContext)
contextLine =  whiteSpace >> ((comment >> return Nothing) <|> liftM Just passwordContext) <* whiteSpace

contexts :: Parser [PasswordContext]
contexts =  catMaybes <$> manyTill contextLine eof

parseContextFile :: FilePath -> IO (Either Doc [PasswordContext])
parseContextFile fn = liftM finalize $ parseFromFileEx contexts fn
  where
    finalize (Success r) = Right r
    finalize (Failure err) = Left err
