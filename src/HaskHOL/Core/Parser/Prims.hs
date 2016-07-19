{-# LANGUAGE DeriveLift, TypeFamilies #-}
{-|
  Module:    HaskHOL.Core.Parser.Prims
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module implements the primitive data types and methods for HaskHOL's
  parser.
-}
module HaskHOL.Core.Parser.Prims where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

import Control.Lens

import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import qualified Text.Parsec as P

-- | Parsed, but pre-elaborated HOL types.
data PreType
    = PTyCon !Text
    | UTyVar !Bool !Text !Int
    | STyVar !Integer
    | PTyComb !PreType ![PreType]
    | PUTy !PreType !PreType
    deriving (Eq, Show)

-- | Parsed, but pre-elaborated HOL terms.
data PreTerm
    = PVar !Text !PreType
    | PConst !Text !PreType
    | PComb !PreTerm !PreTerm
    | PAbs !PreTerm !PreTerm
    | PAs !PreTerm !PreType
    | PInst ![(PreType, Text)] !PreTerm
    | PApp !PreType
    | TyPAbs !PreType !PreTerm
    | TyPComb !PreTerm !PreType !PreType
    deriving (Eq, Show)

-- | A re-export of 'P.ParseError'.
type ParseError = P.ParseError

-- | A context for pure parsing.
data ParseContext = ParseContext
    { _typeConstants :: !(Map Text TypeOp)
    , _termConstants :: !(Map Text HOLTerm)
    , _typeAbbrevs :: !(Map Text HOLType)
    , _prefixes :: ![Text] 
    , _binders :: ![Text]
    , _tyBinders :: ![Text]
    , _infixes :: ![(Text, (Int, Text))]
    , _interface :: ![(Text, (Text, HOLType))]
    , _overloads :: !(Map Text HOLType)
    , _hidden :: ![Text]
    } deriving Lift
makeLenses ''ParseContext

deriveSafeCopy 0 'base ''ParseContext

putParseContext :: ParseContext -> Update ParseContext ()
putParseContext = put

getParseContext :: Query ParseContext ParseContext
getParseContext = ask

makeAcidic ''ParseContext ['putParseContext, 'getParseContext]

-- | The initial parser context.
initParseContext :: ParseContext
initParseContext = ParseContext 
    initTypeConstants initTermConstants mapEmpty [] 
    initBinderOps initTyBinderOps initInfixOps [] mapEmpty []

initBinderOps :: [Text]
initBinderOps = ["\\"]

initTyBinderOps :: [Text]
initTyBinderOps = ["\\\\"]

initInfixOps :: [(Text, (Int, Text))]
initInfixOps = [("=", (12, "right"))]

-- | Retrieves the current parser context.
parseContext :: HOL cls thry ParseContext
parseContext =
    do acid <- openLocalStateHOL initParseContext
       ctxt <- queryHOL acid GetParseContext
       closeAcidStateHOL acid
       return ctxt

viewParseContext :: Getting a ParseContext a -> HOL cls thry a
viewParseContext f = view f `fmap` parseContext

overParseContext :: Setting (->) ParseContext ParseContext a a -> (a -> a) 
                 -> HOL Theory thry ()
overParseContext f p =
    do acid <- openLocalStateHOL initParseContext
       ctxt <- queryHOL acid GetParseContext
       updateHOL acid (PutParseContext $ over f p ctxt)
       closeAcidStateHOL acid

testParseContext :: Optical (->) (->) (Const Bool) ParseContext ParseContext a a
                 -> (a -> Bool) -> HOL cls thry Bool
testParseContext f p = views f p `fmap` parseContext
