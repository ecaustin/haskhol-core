module HaskHOL.Core.Parser.Prims
    ( PreType(..)
    , PreTerm(..)
    , ParseError
    ) where

import HaskHOL.Core.Lib

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
