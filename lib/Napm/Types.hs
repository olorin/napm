module Napm.Types where

import           Data.Map            (Map)
import           Data.Text           (Text)
import           Options.Applicative

{-
The context in which passwords are generated. Consists of a password
domain/textual context that's not sensitive and easy for the user to
remember (hostname, URL, whatever), plus the length of the generated
password. Contexts are not considered sensitive, although they contain
information which may aid an attacker and so should not be accessible
by anyone other than the user.
-}
type ContextMap = Map Text Int

{-
The context in which a single password is generated, consisting of a
password domain and a password length.
-}
type PasswordContext = (Text, Int)

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


