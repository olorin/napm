module Napm.Options(
    NapmOpts(..),
    napmOptParser
) where

import           Options.Applicative

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
