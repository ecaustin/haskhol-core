{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, ViewPatterns #-}

{-|
  Module:    HaskHOL.Core.Parser.Lib
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines or re-exports common utility functions, type classes, 
  and auxilliary data types used in HaskHOL's parsers.  These primarily fall
  three classes of objects:

  * Types and functions used by the parsers.

  * Benign flag and state extensions used by the parsers.

  * Predicates and modifiers for state extensions used by the parsers.

  Note that, because these state extensions were designed to be used with the
  parser, the accessor and predicate functions are written to use 'getExtCtxt' 
  rather than 'getExt' for convenience.

  To see what is actually exported to the user, see the module 
  "HaskHOL.Core.Parser".
-}

module HaskHOL.Core.Parser.Lib
    ( -- * Parser Utilities and Types
      PreType(..)
    , PreTerm(..)
    , dpty          -- :: PreType
    , pretypeOfType -- :: HOLType -> PreType
    , MyParser
    , myparens
    , mybraces
    , mybrackets
    , mycommaSep1
    , mysemiSep
    , myreserved
    , myidentifier
    , myinteger
    , myoperator
    , myreservedOp
    , choiceOp
    , mywhiteSpace
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
      -- * Re-export Parsec for convenience reasons
    , module Text.ParserCombinators.Parsec
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)

-- new flags and extensions
{-| 
  Flag to say whether to treat a constant varstruct, i.e.  @\\ const . bod@, as
  variable.
-}
newFlag "FlagIgnoreConstVarstruct" True

{-|
  Flag indicating that the user should be warned if a type variable was invented
  during parsing.
-}
newFlag "FlagTyInvWarning" True

{-|
  Flag indicating that the user should be warned if a type operator variable was
  invented during parsing.
-}
newFlag "FlagTyOpInvWarning" True

{-|
  Flag to say whether implicit type applications are to be added during parsing.
-}
newFlag "FlagAddTyAppsAuto" True

newExtension "BinderOps" [| ["\\"] :: [String] |]

newExtension "TyBinderOps" [| ["\\\\"] :: [String] |]

newExtension "PrefixOps" [| [] :: [String] |]

newExtension "InfixOps" 
  [| [("=", (12, AssocRight))] :: [(String, (Int, Assoc))] |]

newExtension "Interface" [| [] :: [(String, (String, HOLType))] |]

newExtension "Overload" [| [] :: [(String, HOLType)] |]

newExtension "TypeAbbreviations" [| [] :: [(String, HOLType)] |]

newExtension "Hidden" [| [] :: [String] |]

-- | Parsed, but pre-elaborated HOL types.
data PreType
    = PTyCon String
    | UTyVar Bool String Int
    | STyVar Int
    | PTyComb PreType [PreType]
    | PUTy PreType PreType
    deriving (Eq, Show)

-- | Parsed, but pre-elaborated HOL terms.
data PreTerm
    = PVar String PreType
    | PConst String PreType
    | PComb PreTerm PreTerm
    | PAbs PreTerm PreTerm
    | PAs PreTerm PreType
    | PInst [(PreType, String)] PreTerm
    | PApp PreType
    | TyPAbs PreType PreTerm
    | TyPComb PreTerm PreType PreType
    deriving Show

-- | The default 'PreType' to be used as a blank for the type inference engine.
dpty :: PreType
dpty = PTyComb (PTyCon "") []

-- | Converts a 'HOLType' to 'PreType'
pretypeOfType :: HOLType -> PreType
pretypeOfType (view -> TyVar f v) = UTyVar f v 0
pretypeOfType (view -> TyApp tyop args) =
    let (s, n) = destTypeOp tyop
        tyop' = if n == -1 then UTyVar False s (length args) else PTyCon s in
      PTyComb tyop' $ map pretypeOfType args
pretypeOfType (view -> UType tv tb) = 
    PUTy (pretypeOfType tv) $ pretypeOfType tb
pretypeOfType _ = error "pretypeOfType: incomplete view pattern"

{-| 
  An alias to a stateful 'CharParser' that carries a 'HOLContext' and list of
  known type operator variables with their arity.  This second list is used
  to guarantee that all instances of a type operator variable in a term have
  the same arity.
-}
type MyParser thry a = CharParser (HOLContext thry, [(String, Int)]) a

-- used internally by the numerous parser combinators seen below.
lexer :: TokenParser (HOLContext thry, [(String, Int)])
lexer = makeTokenParser
        (emptyDef
        { reservedNames = ["TYINST", "let", "and", "in", "if", "then", "else"]
        , reservedOpNames = ["%", "_", "'", "->", "+", "#", "^", ":", ";"]
        })

-- | A version of 'parens' for our language.
myparens :: MyParser thry a -> MyParser thry a
myparens = parens lexer

-- | A version of 'braces' for our language.
mybraces :: MyParser thry a -> MyParser thry a
mybraces = braces lexer

-- | A version of 'brackets' for our language.
mybrackets :: MyParser thry a -> MyParser thry a
mybrackets = brackets lexer

-- | A version of 'commaSep1' for our language.
mycommaSep1 :: MyParser thry a -> MyParser thry [a]
mycommaSep1 = commaSep1 lexer

-- | A version of 'semiSep' for our language.
mysemiSep :: MyParser thry a -> MyParser thry [a]
mysemiSep = semiSep lexer

-- | A version of 'reserved' for our language.
myreserved :: String -> MyParser thry ()
myreserved = reserved lexer

-- | A version of 'identifier' for our language.
myidentifier :: MyParser thry String
myidentifier = identifier lexer

-- | A version of 'integer' for our language.
myinteger :: MyParser thry Integer
myinteger = integer lexer

-- | A version of 'operator' for our language.
myoperator :: MyParser thry String
myoperator = operator lexer

-- | A version of 'reservedOp' for our language.
myreservedOp :: String -> MyParser thry ()
myreservedOp = reservedOp lexer

-- | Selects the first matching reserved operator.
choiceOp :: [String] -> MyParser thry String
choiceOp ops = 
    choice $ map (\ name -> do myreservedOp name
                               return name) ops

-- | A version of 'whiteSpace' for our language.
mywhiteSpace :: MyParser thry ()
mywhiteSpace = whiteSpace lexer

-- State Extensions
-- Operators
-- | Specifies a 'String' to be recognized as a term binder by the parser.
parseAsBinder :: String -> HOL Theory thry ()
parseAsBinder op =
    modifyExt (\ (BinderOps ops) -> BinderOps $ op `insert` ops)

-- | Specifies a 'String' to be recognized as a type binder by the parser.
parseAsTyBinder :: String -> HOL Theory thry ()
parseAsTyBinder op =
    modifyExt (\ (TyBinderOps ops) -> TyBinderOps $ op `insert` ops)

-- | Specifies a 'String' to be recognized as a prefix operator by the parser.
parseAsPrefix :: String -> HOL Theory thry ()
parseAsPrefix op =
    modifyExt (\ (PrefixOps ops) -> PrefixOps $ op `insert` ops)

{-| 
  Specifies a 'String' to be recognized as an infix operator by the parser with
  a given precedence level and associativity.
-}
parseAsInfix :: (String, (Int, Assoc)) -> HOL Theory thry ()
parseAsInfix op =
    modifyExt (\ (InfixOps ops) -> InfixOps $ op `insertInfix` ops)
  where insertInfix :: (String, (Int, Assoc)) -> [(String, (Int, Assoc))] ->
                       [(String, (Int, Assoc))]
        insertInfix i@(n, _) is =
            case find (\ (n', _) -> n == n') is of
              Just _ -> is
              _ -> sortBy (\ (_, (x, _)) (_, (y, _)) -> y `compare` x) $ i:is

-- | Specifies a 'String' for the parser to stop recognizing as a term binder.
unparseAsBinder :: String -> HOL Theory thry ()
unparseAsBinder op =
    modifyExt (\ (BinderOps ops) -> BinderOps $ op `delete` ops)

-- | Specifies a 'String' for the parser to stop recognizing as a type binder.
unparseAsTyBinder :: String -> HOL Theory thry ()
unparseAsTyBinder op =
    modifyExt (\ (TyBinderOps ops) -> TyBinderOps $ op `delete` ops)

{-| 
  Specifies a 'String' for the parser to stop recognizing as a prefix operator.
-}
unparseAsPrefix :: String -> HOL Theory thry ()
unparseAsPrefix op =
    modifyExt (\ (PrefixOps ops) -> PrefixOps $ op `delete` ops)

{-| 
  Specifies a 'String' for the parser to stop recognizing as an infix operator.
-}
unparseAsInfix :: String -> HOL Theory thry ()
unparseAsInfix op =
    modifyExt (\ (InfixOps ops) -> 
                  InfixOps $ filter (\ (x, _) -> x == op) ops)

-- | Returns all 'String's recognized as term binders by the parser.
binders :: HOLContext thry -> [String]
binders ctxt =
    let (BinderOps ops) = getExtCtxt ctxt in
      ops

-- | Returns all 'String's recognized as type binders by the parser.
tyBinders :: HOLContext thry -> [String]
tyBinders ctxt =
    let (TyBinderOps ops) = getExtCtxt ctxt in
      ops

-- | Returns all 'String's recognized as prefix operators by the parser.
prefixes :: HOLContext thry -> [String]
prefixes ctxt =
    let (PrefixOps ops) = getExtCtxt ctxt in
      ops

{-| 
  Returns all 'String's recognized as infix operators by the parser along with
  their precedence and associativity pairs.
-} 
infixes :: HOLContext thry -> [(String, (Int, Assoc))]
infixes ctxt =
    let (InfixOps ops) = getExtCtxt ctxt in
      ops

-- | Predicate for 'String's recognized as term binders by the parser.
parsesAsBinder :: String -> HOLContext thry -> Bool
parsesAsBinder op = elem op . binders

-- | Predicate for 'String's recognized as term binders by the parser.
parsesAsTyBinder :: String -> HOLContext thry -> Bool
parsesAsTyBinder op = elem op . tyBinders

-- | Predicate for 'String's recognized as prefix operators by the parser.
isPrefix :: String -> HOLContext thry -> Bool
isPrefix op = elem op . prefixes

{-| 
  Predicate for 'String's recognized as infix operators by the parser.  Returns
  a precidence and associativity pair guarded by 'Maybe'.
-}
getInfixStatus :: String -> HOLContext thry -> Maybe (Int, Assoc)
getInfixStatus op = lookup op . infixes

-- Interface
{-|
  Specifies a 'String' that can act as an overloadable identifier within the
  parser.  The provided type is the most general type that instances of this
  symbol may have.  Throws a 'HOLException' if the given symbol has already been
  declared as overloadable with a different type.

  Note that defining a symbol as overloadable will erase any interface overloads
  that were previously introduced via 'overrideInterface' in order to guarantee
  that all overloads are matchable with their most general type.
-}
makeOverloadable :: String -> HOLType -> HOL Theory thry ()
makeOverloadable s gty =
    do (Overload overs) <- getExt
       case lookup s overs of
         Just ty
             | gty == ty -> return ()
             | otherwise -> 
                 fail "makeOverloadable: differs from existing skeleton"
         _ -> do putExt $ Overload ((s, gty):overs)
                 modifyExt (\ (Interface iface) -> 
                               Interface $ filter (\ (x, _) -> x /= s) iface)

-- | Removes all instances of an overloaded symbol from the interface.
removeInterface :: String -> HOL Theory thry ()
removeInterface sym =
    modifyExt (\ (Interface iface) -> Interface $ 
                                        filter (\ (x, _) -> x /= sym) iface)

{-| 
  Removes a specific instance of an overloaded symbol from the interface.  
  Throws a 'HOLException' if the provided term is not a constant or varible term
  representing an instance of the overloaded symbol.
-}
reduceInterface :: String -> HOLTerm -> HOL Theory thry ()
reduceInterface sym tm =
    do namty <- liftMaybe "reduceInterface: term not a constant or variable" $ 
                  destConst tm <|> destVar tm
       modifyExt (\ (Interface iface) -> Interface $ 
                                           (sym, namty) `delete` iface)

{-|
  Removes all existing overloads for a given symbol and replaces them with a
  single, specific instance.  Throws a 'HOLException' if the provided term is
  not a constant or variable term representing an instance of the overloaded
  symbol.

  Note that because 'overrideInterface' can introduce at most one overload for
  a symbol it does not have to be previously defined as overloadable via 
  'makeOverloadable'.  However, if the symbol is defined as overloadable then 
  the provided term must have a type that is matchable with the symbol's most
  general type.
-}
overrideInterface :: String -> HOLTerm -> HOL Theory thry ()
overrideInterface sym tm =
    do namty <- liftMaybe "overrideInterface: term not a constant or variable" $
                  destConst tm <|> destVar tm
       let m = modifyExt (\ (Interface iface) -> 
                             let iface' = filter (\ (x, _) -> x /= sym) iface in
                               Interface $ (sym, namty) : iface')
       (Overload overs) <- getExt
       case sym `lookup` overs of
         Just gty -> if isNothing $ typeMatch gty (snd namty) ([], [], [])
                     then fail $ "overrideInterface: " ++
                                 "not an instance of type skeleton"
                     else m
         _ -> m

{-|
  Introduces a new overload for a given symbol.  Throws a 'HOLException' in the
  following cases:

  * The symbol has not previously been defined as overloadable via 
    'makeOverloadable'.
  
  * The provided term is not a constant or variable term representing a 
    specific instance of the overloaded symbol.

  * The provided term does not have a type that is matchable with the
    overloadable symbol's specified most general type.

  Note that specifying an overload that already exists will move it to the front
  of the interface list, effectively prioritizing it.  This behavior is utilized
  by 'prioritizeOverload'.
-}
overloadInterface :: String -> HOLTerm -> HOL Theory thry ()
overloadInterface sym tm =
    do (Overload overs) <- getExt
       gty <- liftMaybe ("overloadInstace: symbol " ++ sym ++ 
                         " is not overloadable.") $ lookup sym overs
       namty <- liftMaybe "overloadInstance: term not a constant or variable" $ 
                  destConst tm <|> destVar tm
       if isNothing $ typeMatch gty (snd namty) ([], [], [])
          then fail "overloadInstance: not an instance of type skeleton"
          else modifyExt (\ (Interface iface) -> 
                             let iface' = (sym, namty) `delete` iface in
                               Interface $ (sym, namty) : iface')

{-|
  Specifies a type to prioritize when the interface is used to overload a 
  symbol.  Note that this applies to all overloads in the system whose match
  with the specified most general type involves the provided type.  
  Prioritization is done by redefining overloads via 'overloadInterface'.
-}
prioritizeOverload :: HOLType -> HOL Theory thry ()
prioritizeOverload ty =
    do (Overload overs) <- getExt
       mapM_ (\ (s, gty) -> 
              (do (Interface iface) <- getExt
                  let (n, t') = fromJust $ 
                                tryFind (\ (s', x@(_, t)) ->
                                         if s' /= s then Nothing
                                         else do (tys, _, _) <- typeMatch gty t
                                                                  ([], [], [])
                                                 _ <- ty `revLookup` tys
                                                 return x) iface
                  overloadInterface s $ mkVar n t')
              <|> return ()) overs

-- | Returns the list of all currently defined interface overloads.
getInterface :: HOLContext thry -> [(String, (String, HOLType))]
getInterface ctxt =
    let (Interface iface) = getExtCtxt ctxt in iface

{-| 
  Returns the list of all overloadable symbols paired with their most generic 
  types.
-}
getOverloads :: HOLContext thry -> [(String, HOLType)]
getOverloads ctxt =
    let (Overload overs) = getExtCtxt ctxt in overs

-- Type Abbreviations
{-| 
  Specifies a 'String' to act as an abbreviation for a given type in the parser.
  Upon recognizing the abbreviation the parser will replace it with the 
  'PreType' value for it's associated 'HOLType' such that the elaborator can
  infer the correct type for polymorphic abbreviations.
-}
newTypeAbbrev :: String -> HOLType -> HOL Theory thry ()
newTypeAbbrev s ty =
    modifyExt (\ (TypeAbbreviations abvs) -> 
                  TypeAbbreviations $ insertMap s ty abvs)

{-| 
  Specifies a 'String' for the parser to stop recognizing as a type 
  abbreviation.
-}
removeTypeAbbrev :: String -> HOL Theory thry ()
removeTypeAbbrev s =
    modifyExt (\ (TypeAbbreviations abvs) ->
                  TypeAbbreviations $ filter (\ (s', _) -> s' /= s) abvs)

{-| 
  Returns all 'String's currently acting as type abbreviations in the parser
  paired with their associated types.
-}
typeAbbrevs :: HOLContext thry -> [(String, HOLType)]
typeAbbrevs ctxt =
    let (TypeAbbreviations abvs) = getExtCtxt ctxt in abvs

-- Hidden Constant Mapping
-- | Specifies a 'String' for the parser to stop recognizing as a constant.
hideConstant :: String -> HOL Theory thry ()
hideConstant c = modifyExt (\ (Hidden hcs) -> Hidden $ c `insert` hcs)

-- | Specifies a 'String' for the parser to resume recognizing as a constant.
unhideConstant :: String -> HOL Theory thry ()
unhideConstant c = modifyExt (\ (Hidden hcs) -> Hidden $ c `delete` hcs)

-- | Returns all 'String's currently acting as constants hidden from the parser.
getHidden :: HOLContext thry -> [String]
getHidden ctxt = 
    let (Hidden hidden) = getExtCtxt ctxt in hidden

deriveLiftMany [ ''BinderOps, ''TyBinderOps 
               , ''PrefixOps, ''InfixOps
               , ''Interface, ''Overload 
               , ''TypeAbbreviations, ''Hidden ]
