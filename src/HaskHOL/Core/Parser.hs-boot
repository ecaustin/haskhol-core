module HaskHOL.Core.Parser where

import HaskHOL.Core.Lib
import HaskHOL.Core.Parser.Prims
import HaskHOL.Core.State.Monad

holTermParser :: Text -> HOL cls thry PreTerm

holTypeParser :: Text -> HOL cls thry PreType