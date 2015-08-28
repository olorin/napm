{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Napm.Types(
    Domain(..),
    PasswordLength(..),
    Context,
    ContextMap
) where

import           Data.Map            (Map)
import           Data.Text           (Text)

{-
Password domain/textual context, e.g.,
"bob@example.com". Mostly-freeform, but can't contain whitespace or
colons (':').
-}
newtype Domain = Domain { unDomain :: Text }
    deriving (Eq, Show, Ord)

{-
Length of a generated password; must be a positive integer.
-}
newtype PasswordLength = PasswordLength { unPasswordLength :: Integer }
    deriving (Eq, Show, Ord, Num)

{-
The context in which passwords are generated. Consists of a password
domain/textual context that's not secret and easy for the user to
remember (hostname, URL, whatever), plus the length of the generated
password. Revealing a context should not compromise the password, but
they're considered non-public.
-}
type ContextMap = Map Text Int

{-
The context in which a single password is generated, consisting of a
password domain and a password length.
-}
type Context = (Text, Int)
