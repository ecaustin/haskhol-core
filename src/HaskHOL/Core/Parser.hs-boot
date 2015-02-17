module HaskHOL.Core.Parser where

import HaskHOL.Core.Lib
import HaskHOL.Core.Parser.Prims

holTermParser :: Text -> HOLContext thry -> Either ParseError PreTerm

holTypeParser :: Text -> HOLContext thry -> Either ParseError PreType