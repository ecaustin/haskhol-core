{-# LANGUAGE FlexibleContexts #-}
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
    , ptype
    , holTypeParser
    , pterm
    , holTermParser
      -- * Type/Term Representation Conversions
    , HOLTypeRep(..)
    , HOLTermRep(..)
    , HOLThmRep(..)
      -- * Type Abbreviations
    , newTypeAbbrev
    , removeTypeAbbrev
    , typeAbbrevs
      -- * Primitive Parser types, utility functions, and extensions.
    , module HaskHOL.Core.Parser.Lib
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

import HaskHOL.Core.Parser.Lib
import qualified HaskHOL.Core.Parser.Prims as Prims
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.Parser.TermParser
import HaskHOL.Core.Parser.Elab
import HaskHOL.Core.Parser.Rep

-- Type Abbreviations
{-| 
  Specifies a 'Text' to act as an abbreviation for a given type in the parser.
  Upon recognizing the abbreviation the parser will replace it with the 
  'PreType' value for it's associated 'HOLType' such that the elaborator can
  infer the correct type for polymorphic abbreviations.
-}
newTypeAbbrev :: HOLTypeRep ty Theory thry => Text -> ty -> HOL Theory thry ()
newTypeAbbrev s pty =
    do ty <- toHTy pty
       Prims.overParseContext Prims.typeAbbrevs (mapInsert s ty)

{-| 
  Specifies a 'Text' for the parser to stop recognizing as a type 
  abbreviation.
-}
removeTypeAbbrev :: Text -> HOL Theory thry ()
removeTypeAbbrev s = Prims.overParseContext Prims.typeAbbrevs (mapDelete s)

{-| 
  Returns all 'Text's currently acting as type abbreviations in the parser
  paired with their associated types.
-}
typeAbbrevs :: HOL cls thry (Map Text HOLType)
typeAbbrevs = Prims.viewParseContext Prims.typeAbbrevs
