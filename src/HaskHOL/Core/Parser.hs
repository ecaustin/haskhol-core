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
    ( -- * Parser Data Types
      PreTerm
    , PreType
      -- * Type Elaboration Flags
    , FlagIgnoreConstVarstruct(..)
    , FlagTyInvWarning(..)
    , FlagTyOpInvWarning(..)
    , FlagAddTyAppsAuto(..)
       -- * Extensible Parser Operators
    , parseAsBinder           -- :: String -> HOL Theory thry ()
    , parseAsTyBinder         -- :: String -> HOL Theory thry ()
    , parseAsPrefix           -- :: String -> HOL Theory thry ()
    , parseAsInfix            -- :: (String, (Int, Assoc)) -> HOL Theory thry ()
    , unparseAsBinder         -- :: String -> HOL Theory thry ()
    , unparseAsTyBinder       -- :: String -> HOL Theory thry ()
    , unparseAsPrefix         -- :: String -> HOL Theory thry ()
    , unparseAsInfix          -- :: String -> HOL Theory thry ()
    , binders                 -- :: HOLContext thry -> [String]
    , tyBinders               -- :: HOLContext thry -> [String]
    , prefixes                -- :: HOLContext thry -> [String]
    , infixes                 -- :: HOLContext thry -> [(String, (Int, Assoc))]
    , parsesAsBinder          -- :: String -> HOLContext thry -> Bool
    , parsesAsTyBinder        -- :: String -> HOLContext thry -> Bool
    , isPrefix                -- :: String -> HOLContext thry -> Bool
    , getInfixStatus          -- :: String -> HOLContext thry -> 
                              --    Maybe (Int, Assoc)
      -- * Overloading and Interface Mapping
    , makeOverloadable   -- :: String -> HOLType -> HOL Theory thry ()
    , removeInterface    -- :: String -> HOL Theory thry ()
    , reduceInterface    -- :: String -> HOLTerm -> HOL Theory thry ()
    , overrideInterface  -- :: String -> HOLTerm -> HOL Theory thry ()
    , overloadInterface  -- :: String -> HOLTerm -> HOL Theory thry ()
    , prioritizeOverload -- :: HOLType -> HOL Theory thry ()
    , getInterface       -- :: HOLContext thry -> [(String, (String, HOLType))]
    , getOverloads       -- :: HOLContext thry -> [(String, HOLType)]
      -- * Type Abbreviations
    , newTypeAbbrev    -- :: String -> HOLType -> HOL Theory thry ()
    , removeTypeAbbrev -- :: String -> HOL Theory thry ()
    , typeAbbrevs      -- :: HOLContext thry -> [(String, HOLType)]
      -- * Hidden Constant Mapping 
    , hideConstant   -- :: String -> HOL Theory thry ()
    , unhideConstant -- :: String -> HOL Theory thry ()
    , getHidden      -- :: HOLContext thry -> [String]
      -- * Elaboration Functions
    , tyElab -- :: PreType -> HOL cls thry HOLTerm
    , elab   -- :: PreTerm -> HOL cls thry HOLTerm
      -- * Parsing Functions
    , holTypeParser -- :: String -> HOLContext thry -> Either ParseError PreType
    , holTermParser -- :: String -> HOLContext thry -> Either ParseError PreTerm
      -- * Type/Term Representation Conversions
    , HOLTypeRep(..)
    , HOLTermRep(..)
    ) where

import HaskHOL.Core.State

import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.Parser.TermParser
import HaskHOL.Core.Parser.Elab
import HaskHOL.Core.Parser.Rep

runHOLParser :: MyParser thry a -> String -> HOLContext thry -> 
                Either ParseError a
runHOLParser parser input ctxt =
    runParser parser (ctxt, []) "" input

-- | Parser for 'HOLTerm's.
holTermParser :: String -> HOLContext thry -> Either ParseError PreTerm
holTermParser = runHOLParser pterm

-- | Parser for 'HOLType's.
holTypeParser :: String -> HOLContext thry -> Either ParseError PreType
holTypeParser = runHOLParser ptype
