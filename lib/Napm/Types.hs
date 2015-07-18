{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Napm.Types(
    Context,
    ContextMap
) where

import           Control.Lens        hiding (Context)
import           Control.Monad
import           Data.Map            (Map)
import           Data.Text           (Text)
import qualified Data.Text           as T

{-
Password domain/textual context, e.g.,
"bob@example.com". Mostly-freeform, but can't contain whitespace or
colons (':').
-}
newtype Domain = Domain { unDomain :: Text }
    deriving (Eq, Show, Ord)

domain :: Prism' Text Domain
domain = prism' unDomain $ \d -> do
    guard . not $ T.null d
    guard $ T.all validDomainChar d
    return (Domain d)
  where
    validDomainChar c = not $ elem c ":\n\r\t "

{-
Length of a generated password; must be a positive integer.
-}
newtype PasswordLength = PasswordLength { unPasswordLength :: Integer }
    deriving (Eq, Show, Ord, Num)

passwordLength :: Prism' Integer PasswordLength
passwordLength = prism' unPasswordLength $ \n ->
    if n <= 0 then Nothing
    else Just (PasswordLength n)

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
type Context = (Text, Int)
