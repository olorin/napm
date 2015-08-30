module Napm.Options(
    NapmOpts(..),
    napmOptParser
) where

import           Control.Applicative

import qualified Data.Text           as T
import           Napm.Types
import           Options.Applicative

data NapmOpts = NapmOpts
    { napmPasswordLen :: Maybe PasswordLength
    -- ^ Override default (or configured) length of generated password.
    , napmDomain      :: Maybe Domain
    -- ^ Supply context via the CLI rather than being prompted for it.
    } deriving Show

-- FIXME: parsers
napmOptParser :: ParserInfo NapmOpts
napmOptParser =  info (helper <*> opts)
             (   fullDesc
              <> progDesc "Deterministic hashing password generator."
              <> header   "napm - deterministic hashing password generator"
             )
  where
    opts = NapmOpts
           <$> ((maybe Nothing (Just . PasswordLength)) <$> ( option auto (   long "password-length"
                            <> short 'l'
                            <> metavar "LENGTH"
                            <> value Nothing
                            <> help ("Override default or configured password " <>
                                     "length. The default of -1 will cause the " <>
                                     "default or configured value to be used.")
                           )))
           <*> ((maybe Nothing (Just . Domain . T.pack)) <$> option auto  (   long "context"
                            <> short 'c'
                            <> metavar "CONTEXT"
                            <> value Nothing
                            <> help ("Supply context via CLI rather than " <>
                                     "being prompted for it.")
                           ))


