{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Data.Bifunctor
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.IO                 as TIO
import           Options.Applicative
import           System.Directory
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Napm.Context
import           Napm.Password

data NapmOpts = NapmOpts
    { _passwordLen :: Int
    -- ^ Override default (or configured) length of generated password.
    , _context     :: String
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
readPassphrase = readPassphrase' >>= passThroughAction (hPutStrLn stderr "" >> hSetEcho stdin True)
  where
    readPassphrase' = hPutStr stderr "Seed: "
                   >> hFlush stderr
                   >> hSetEcho stdin False
                   >> TIO.hGetLine stdin

    passThroughAction act x = act >> return x

napmDataDir :: IO (Either String FilePath)
napmDataDir = first show `fmap` (try $ getAppUserDataDirectory "napm" :: IO (Either SomeException FilePath))

napmContextFile :: FilePath -> FilePath
napmContextFile dataDir = dataDir <> "/contexts"

getContexts :: FilePath -> IO (Either String ContextMap)
getContexts dataDir = doesFileExist (napmContextFile dataDir) >>= maybeParseContexts
  where
    maybeParseContexts False = do
        hPutStrLn stderr "Can't find a context file. Proceeding without one."
        return $ Right M.empty
    maybeParseContexts True = first show `fmap` parseContextFile (napmContextFile dataDir)

main :: IO ()
main = do
    NapmOpts{..} <- execParser napmOptParser
    dataDir <- napmDataDir
    case dataDir of
        Left e -> hPrint stderr e
        Right dataDir' -> do
            contexts <- getContexts dataDir'
            case contexts of
                Left e -> hPutStrLn stderr e
                Right ctxs -> do
                    pp <- readPassphrase
                    ctx <- getOrReadContext _context
                    let (newMap, len) = updateContextMap ctxs (pwlen _passwordLen) ctx
                    TIO.hPutStr stdout $ computePassword len pp ctx
                    r <- writeContextMap newMap $ napmContextFile dataDir'
                    case r of
                        Left e -> hPrint stderr e
                        Right _ -> return ()
  where
    pwlen (-1) = 12
    pwlen l    = l

    getOrReadContext "" = hPutStr stderr "Context: " >> TIO.hGetLine stdin
    getOrReadContext c  = return $ T.pack c
