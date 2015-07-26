module Napm.Password(
    computePassword
) where

import qualified Crypto.Hash.SHA512     as SHA
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Napm.Types

computePassword :: PasswordLength -> Domain -> Passphrase -> Text
computePassword (PasswordLength l) (Domain d) (Passphrase p) =
    T.take l . decodeUtf8 . B64.encode $ SHA.hash salted
  where
    salted = encodeUtf8 p <> encodeUtf8 d
