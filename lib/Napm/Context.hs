{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Napm.Context(
    ContextMap,
    updateContextMap,
    parseContextFile,
    writeContextMap
) where

import           Control.Applicative
import           Control.Exception            (SomeException)
import qualified Control.Exception            as E
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           System.Posix.Files
import           Text.Trifecta

-- |The context in which a password is generated. Consists of a textual
--  context that's not sensitive and easy for the user to remember
--  (hostname, URL, whatever), plus the length of the generated
--  password. Contexts are not considered sensitive, although they
--  contain information which may aid an attacker and so should not be
--  accessible by anyone other than the user.
type ContextMap = Map Text Int

type PasswordContext = (Text, Int)

eol :: Parser ()
eol =  void $ oneOf "\n\r"

comment :: Parser ()
comment =  char '#' >> skipMany (noneOf "\r\n")

context :: Parser String
context =  many (noneOf ":")

passwordLength :: Parser Int
passwordLength =  fromIntegral <$> decimal <* (try eol <|> try comment <|> eof)

passwordContext :: Parser PasswordContext
passwordContext =  (,) <$>
                   (T.pack <$> context) <*>
                   (char ':' *> passwordLength)

contextLine :: Parser (Maybe PasswordContext)
contextLine =  whiteSpace >> ((comment >> return Nothing) <|> liftM Just passwordContext) <* whiteSpace

contexts :: Parser [PasswordContext]
contexts =  catMaybes <$> manyTill contextLine eof

-- | Catch fire if the context file isn't chmod 600.
bailOnInsecureContext :: (MonadError String m, MonadIO m)
                     => FilePath
                     -> m ()
bailOnInsecureContext fn = do
    st <- liftIO $ getFileStatus fn
    case intersectFileModes badModes (fileMode st) of
        0 -> return () -- must be go-rwx
        _ -> throwError $ "Context file " <> fn <> " must be accessible only by its owner (chmod 600)."
  where
    badModes = unionFileModes groupModes otherModes

parseContextFile :: (MonadError String m, MonadIO m)
                 => FilePath
                 -> m ContextMap
parseContextFile fn = do
    bailOnInsecureContext fn
    parsed <- liftIO $ parseFromFileEx contexts fn
    case parsed of
        Success r -> return $ M.fromList r
        Failure e -> throwError $ show e

updateContextMap :: ContextMap -> Int -> Text -> (ContextMap, Int)
updateContextMap m len ctx = case M.lookup ctx m of
    Nothing -> (M.insert ctx len m, len)
    Just x -> (m, x)

fmtContextMap :: ContextMap -> Text
fmtContextMap =  T.intercalate "\n" . map fmtItem . M.toList
  where
    fmtItem (ctx, len) = ctx <> ":" <> T.pack (show len)

writeContextMap :: (MonadError String m, MonadIO m)
                => ContextMap
                -> FilePath
                -> m ()
writeContextMap m fp = do
    res <- liftIO $ E.try (T.writeFile fp (fmtContextMap m))
    case res of
        Left e -> throwError $ show (e :: SomeException)
        Right _ -> return ()
