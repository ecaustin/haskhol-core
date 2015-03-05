module HaskHOL.Core.Parser.Prims
    ( PreType(..)
    , PreTerm(..)
    , ParseError
    , HOLContext(..)
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel

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

data HOLContext thry = HOLContext 
    { infixes :: ![(Text, (Int, Text))]
    , prefixes :: ![Text]
    , binders :: ![Text]
    , tyBinders :: ![Text]
    , typesCtxt :: !(Map Text TypeOp)
    , constsCtxt :: !(Map Text HOLTerm)
    , typeAbbrevsCtxt :: !(Map Text HOLType)
    , getInterfaceCtxt :: ![(Text, (Text, HOLType))]
    , flagPrintAllThm :: !Bool
    , flagRevInterface :: !Bool
    , unspacedBinops :: ![Text]
    , prebrokenBinops :: ![Text]
    }
