{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Napm.Util where

import           Control.Applicative

import           Control.Exception
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class

import qualified Data.Text.IO              as TIO

import           Napm.Types

import           System.Directory
import           System.IO

{-
Issue a prompt and read a passphrase (without echo).
-}
readPassphrase :: IO Passphrase
readPassphrase = do
    pp <- readPassphrase'
    hPutStrLn stderr ""
    hSetEcho stdin True
    pure pp
  where
    readPassphrase' = Passphrase <$> do
        hPutStr stderr "Seed: "
        hFlush stderr
        hSetEcho stdin False
        TIO.hGetLine stdin

{-
Find the directory we should store our data files in, or error if it
can't be found.
-}
getDataDir :: (MonadError String m, MonadIO m)
           => m FilePath
getDataDir = do
    dir <- liftIO $ try $ getAppUserDataDirectory "napm"
    case dir of
        Left e -> throwError $ show (e :: SomeException)
        Right dir' -> return dir'
