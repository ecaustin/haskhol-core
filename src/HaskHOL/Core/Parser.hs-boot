module HaskHOL.Core.Parser where

import HaskHOL.Core.State

import HaskHOL.Core.Parser.Lib

holTermParser :: String -> HOLContext thry -> Either ParseError PreTerm

holTypeParser :: String -> HOLContext thry -> Either ParseError PreType