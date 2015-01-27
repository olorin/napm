module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.SHA512     as SHA
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding
import qualified Data.Text.IO           as TIO
import           System.Directory
import           System.IO

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

computePassword :: Int -> Text -> Text -> Text
computePassword len passphrase context =
    T.take len . decodeUtf8 . B64.encode $ SHA.hash salted
  where
    salted = encodeUtf8 passphrase <> encodeUtf8 context

main :: IO ()
main = do
    pp <- readPassphrase
    hPutStr stderr "Context: "
    ctx <- TIO.hGetLine stdin
    TIO.hPutStr stdout $ computePassword 12 pp ctx
