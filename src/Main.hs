{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Data.Bifunctor
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.IO                 as TIO
import           Options.Applicative
import           System.Directory
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Napm.ContextFile
import           Napm.Password

data NapmOpts = NapmOpts
    { _passwordLen :: Int -- ^ Override default (or configured) length of
                          --   generated password.
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

readPassphrase :: IO Text
readPassphrase = readPassphrase' >>= passThroughAction (hPutStrLn stderr "" >> hSetEcho stdin True)
  where
    readPassphrase' = hPutStr stderr "Seed: "
                   >> hFlush stderr
                   >> hSetEcho stdin False
                   >> TIO.hGetLine stdin

    passThroughAction act x = act >> return x

napmDataDir :: IO (Either String FilePath)
napmDataDir = first show `fmap` (try $ getAppUserDataDirectory "napm" :: IO (Either SomeException FilePath))

getContexts :: FilePath -> IO (Either String [PasswordContext])
getContexts dataDir = doesFileExist napmContextFile >>= maybeParseContexts
  where
    napmContextFile = dataDir <> "/contexts"

    maybeParseContexts False = do
        hPutStrLn stderr "Can't find a context file. Proceeding without one."
        return $ Right []
    maybeParseContexts True = first show `fmap` parseContextFile napmContextFile

main :: IO ()
main = do
    NapmOpts{..} <- execParser napmOptParser
    dataDir <- napmDataDir
    contexts <- case dataDir of
        Left e -> return $ Left e
        Right d -> getContexts d
    case contexts of
        Left e -> hPutStrLn stderr e
        Right _ -> do
            pp <- readPassphrase
            hPutStr stderr "Context: "
            ctx <- TIO.hGetLine stdin
            TIO.hPutStr stdout $ computePassword (pwlen _passwordLen) pp ctx

  where
    pwlen (-1) = 12
    pwlen l    = l
