{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Napm.Util where

import           Control.Exception
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Data.Map                  as M
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.IO              as TIO
import           System.Directory
import           System.IO

import           Napm.Context
import           Napm.Password
import           Napm.Types

{-
Issue a prompt and read a passphrase (without echo).
-}
readPassphrase :: IO Text
readPassphrase = do
    pp <- readPassphrase'
    hPutStrLn stderr ""
    hSetEcho stdin True
    return pp
  where
    readPassphrase' = hPutStr stderr "Seed: "
                   >> hFlush stderr
                   >> hSetEcho stdin False
                   >> TIO.hGetLine stdin

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

