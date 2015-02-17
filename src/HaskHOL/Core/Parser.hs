{-|
  Module:    HaskHOL.Core.Parser
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines the parsers for 'HOLType's and 'HOLTerm's.

  It also re-exports the related benign flags, theory extension mechanisms, 
  and type/term elaborators.

  For examples of the parsers and elaborators in use see the 
  "HaskHOL.Core.TermRep" module.
-}
module HaskHOL.Core.Parser
    ( -- * Elaboration Functions
      tyElab
    , elab
      -- * Parsing Functions
    , runHOLParser
    , ptype
    , holTypeParser
    , pterm
    , holTermParser
      -- * Type/Term Representation Conversions
    , HOLTypeRep(..)
    , HOLTermRep(..)
    , HOLThmRep(..)
      -- * Primitive Parser types, utility functions, and extensions.
    , module HaskHOL.Core.Parser.Lib
    ) where

import HaskHOL.Core.Lib

import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.Parser.TermParser
import HaskHOL.Core.Parser.Elab
import HaskHOL.Core.Parser.Rep

{-| Runs a custom parser when provided with an input 'String' and a 
    'HOLContext'.
-}
runHOLParser :: MyParser thry a -> Text -> HOLContext thry -> 
                Either ParseError a
runHOLParser parser input ctxt =
    runParser parser (ctxt, [], 0) "" input

-- | Parser for 'HOLTerm's.
holTermParser :: Text -> HOLContext thry -> Either ParseError PreTerm
holTermParser = runHOLParser pterm

-- | Parser for 'HOLType's.
holTypeParser :: Text -> HOLContext thry -> Either ParseError PreType
holTypeParser = runHOLParser ptype
