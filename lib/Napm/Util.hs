{-# LANGUAGE FlexibleContexts #-}

module Napm.Util where

import Control.Exception
import Control.Monad.Except
import Data.Monoid
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import System.Directory
import System.IO

import Napm.Types
import Napm.Password
import Napm.Context

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
