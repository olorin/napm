{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Napm.Types(
    Domain(..)
  , PasswordLength(..)
  , Passphrase(..)
  , ContextMap
) where

import           Control.Applicative

import           Data.Map  (Map)
import           Data.Text (Text)
import qualified Data.Text as T

import           Test.QuickCheck

-- FIXME: unicode
text :: Gen Text
text = fmap T.pack $ listOf1 textChar
  where
    textChar = elements . concat $ [ ['a'..'z']
                                   , ['A'..'Z']
                                   , ['0'..'9']
                                   , [' '..'/']
                                   ]

{-
Password domain/textual context, e.g.,
"bob@example.com". Mostly-freeform, but can't contain whitespace or
colons (':').
-}
newtype Domain = Domain 
    { unDomain :: Text }
    deriving (Eq, Show, Ord)

-- FIXME: unicode
instance Arbitrary Domain where
    arbitrary = Domain <$> text
    
{-
Length of a generated password.
-}
newtype PasswordLength = PasswordLength 
    { unPasswordLength :: Int }
    deriving (Eq, Show, Ord, Num)

instance Arbitrary PasswordLength where
    arbitrary = PasswordLength <$> positive
      where
        positive = arbitrary `suchThat` (\l -> l > 0 && l <= 64) 

newtype Passphrase = Passphrase
    { unPassphrase :: Text }
    deriving (Eq, Show, Ord)

instance Arbitrary Passphrase where
    arbitrary = Passphrase <$> text

{-
The context in which passwords are generated. Consists of a password
domain/textual context that's not secret and easy for the user to
remember (hostname, URL, whatever), plus the length of the generated
password. Revealing a context should not compromise the password, but
they're considered non-public.
-}
type ContextMap = Map Domain PasswordLength
