{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Napm.Context where

import           Control.Applicative
import           Control.Exception         (SomeException)
import qualified Control.Exception         as E
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           System.Directory
import           System.IO
import           System.Posix.Files
import           Text.Trifecta

import           Napm.Types

eol :: Parser ()
eol =  void $ oneOf "\n\r"

comment :: Parser ()
comment =  char '#' >> skipMany (noneOf "\r\n")

context :: Parser String
context =  many (noneOf ":")

passwordLength :: Parser Int
passwordLength =  fromIntegral <$> decimal <* (try eol <|> try comment <|> eof)

passwordContext :: Parser Context
passwordContext =  (,) <$>
                   (T.pack <$> context) <*>
                   (char ':' *> passwordLength)

contextLine :: Parser (Maybe Context)
contextLine =  whiteSpace >> ((comment >> return Nothing) <|> liftM Just passwordContext) <* whiteSpace

contexts :: Parser [Context]
contexts =  catMaybes <$> manyTill contextLine eof

{-
Catch fire if the context file isn't chmod 600.
-}
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

{-
Write context map out to file, ensuring it is accessible only by its
owner afterwards.
-}
writeContextMap :: (MonadError String m, MonadIO m)
                => ContextMap
                -> FilePath
                -> m ()
writeContextMap m fp = do
    write_res <- liftIO $ E.try (T.writeFile fp (fmtContextMap m))
    case write_res of
        Left e -> throwError $ show (e :: SomeException)
        Right _ -> ensurePrivateMode fp

ensurePrivateMode :: (MonadError String m, MonadIO m)
                  => FilePath -> m ()
ensurePrivateMode fp = do
    res <- liftIO $ E.try (setFileMode fp rwOwner)
    case res of
        Left e -> throwError $ show (e :: SomeException)
        Right _ -> return ()
  where
    rwOwner = unionFileModes ownerWriteMode ownerReadMode

{-
Return the path to our context file, given the path to our data
directory.
-}
napmContextFile :: FilePath
                -> FilePath
napmContextFile dataDir = dataDir <> "/contexts"

{-
Read existing contexts from the context file, if it exists.
-}
getContexts :: (MonadError String m, MonadIO m)
            => FilePath
            -> m ContextMap
getContexts dataDir = do
    existsp <- liftIO $ doesFileExist (napmContextFile dataDir)
    if existsp then parseContextFile (napmContextFile dataDir) else do
        liftIO $ hPutStrLn stderr "Can't find a context file. Proceeding without one."
        return M.empty

