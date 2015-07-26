{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.Text.IO               as TIO
import           Options.Applicative
import           System.IO

import           Napm.Context
import           Napm.Options
import           Napm.Password
import           Napm.Types
import           Napm.Util

main :: IO ()
main = do
    NapmOpts{..} <- execParser napmOptParser
    res <- runExceptT $ do
        dataDir <- getDataDir
        ctxs <- getContexts dataDir
        pp <- liftIO readPassphrase
        ctx <- liftIO $ getOrReadDomain napmDomain
        let (newMap, len) = updateContextMap ctxs (pwlen napmPasswordLen) ctx
        liftIO $ TIO.hPutStr stdout $ computePassword len ctx pp
        writeContextMap newMap $ napmContextFile dataDir
    case res of
        Left e -> hPutStrLn stderr e
        Right _ -> return ()
  where
    pwlen Nothing  = 16
    pwlen (Just l) = l

    getOrReadDomain Nothing = do
      hPutStr stderr "Context: "
      Domain <$> (TIO.hGetLine stdin)
    getOrReadDomain (Just c) = pure c
