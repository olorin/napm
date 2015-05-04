{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Map             as M
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Options.Applicative
import           System.Directory
import           System.IO

import           Napm.Context
import           Napm.Password
import           Napm.Types
import           Napm.Util

main :: IO ()
main = do
    NapmOpts{..} <- execParser napmOptParser
    res <- runExceptT $ do
        dataDir <- getDataDir
        contexts <- getContexts dataDir
        pp <- liftIO readPassphrase
        ctx <- liftIO $ getOrReadContext napmContext
        let (newMap, len) = updateContextMap contexts (pwlen napmPasswordLen) ctx
        liftIO $ TIO.hPutStr stdout $ computePassword len pp ctx
        writeContextMap newMap $ napmContextFile dataDir
    case res of
        Left e -> hPutStrLn stderr e
        Right _ -> return ()
  where
    pwlen (-1) = 12
    pwlen l    = l

    getOrReadContext "" = hPutStr stderr "Context: " >> TIO.hGetLine stdin
    getOrReadContext c  = return $ T.pack c
