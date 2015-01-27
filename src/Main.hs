module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding
import qualified Data.Text.IO        as TIO
import           System.Directory
import           System.IO

import           Napm.Password

readPassphrase :: IO Text
readPassphrase = readPassphrase' >>= passThroughAction (hPutStrLn stderr "" >> hSetEcho stdin True)
  where
    readPassphrase' = hPutStr stderr "Seed: "
                   >> hFlush stderr
                   >> hSetEcho stdin False
                   >> TIO.hGetLine stdin

    passThroughAction act x = act >> return x

napmDataDir :: IO (Either SomeException FilePath)
napmDataDir = try $ getAppUserDataDirectory "napm"

main :: IO ()
main = do
    pp <- readPassphrase
    hPutStr stderr "Context: "
    ctx <- TIO.hGetLine stdin
    TIO.hPutStr stdout $ computePassword 12 pp ctx
