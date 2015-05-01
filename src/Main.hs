{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Map                     as M
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import           Options.Applicative
import           System.Directory
import           System.IO

import           Napm.Context
import           Napm.Password

data NapmOpts = NapmOpts
    { napmPasswordLen :: Int
    -- ^ Override default (or configured) length of generated password.
    , napmContext     :: String
    -- ^ Supply context via the CLI rather than being prompted for it.
    } deriving Show

napmOptParser :: ParserInfo NapmOpts
napmOptParser =  info (helper <*> opts)
             (   fullDesc
              <> progDesc "Deterministic hashing password generator."
              <> header   "napm - deterministic hashing password generator"
             )
  where
    opts = NapmOpts
           <$> option auto (   long "password-length"
                            <> short 'l'
                            <> metavar "LENGTH"
                            <> value (-1)
                            <> help ("Override default or configured password " <>
                                     "length. The default of -1 will cause the " <>
                                     "default or configured value to be used.")
                           )
           <*> strOption   (   long "context"
                            <> short 'c'
                            <> metavar "CONTEXT"
                            <> value ""
                            <> help ("Supply context via CLI rather than " <>
                                     "being prompted for it.")
                           )

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

getDataDir :: (MonadError String m, MonadIO m) => m FilePath
getDataDir = do
    dir <- liftIO $ try $ getAppUserDataDirectory "napm"
    case dir of
        Left e -> throwError $ show (e :: SomeException)
        Right dir' -> return dir'

napmContextFile :: FilePath -> FilePath
napmContextFile dataDir = dataDir <> "/contexts"

getContexts :: (MonadError String m, MonadIO m)
            => FilePath
            -> m ContextMap
getContexts dataDir = do
    existsp <- liftIO $ doesFileExist (napmContextFile dataDir)
    if existsp then parseContextFile (napmContextFile dataDir) else do
        liftIO $ hPutStrLn stderr "Can't find a context file. Proceeding without one."
        return M.empty

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
