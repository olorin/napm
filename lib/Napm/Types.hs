module Napm.Types(
    Context,
    ContextMap
) where

import           Data.Map            (Map)
import           Data.Text           (Text)

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
