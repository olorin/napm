module Napm.Password(
    computePassword
) where

import           Control.Monad
import qualified Crypto.Hash.SHA512     as SHA
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding

computePassword :: Int -> Text -> Text -> Text
computePassword len passphrase context =
    T.take len . decodeUtf8 . B64.encode $ SHA.hash salted
  where
    salted = encodeUtf8 passphrase <> encodeUtf8 context
