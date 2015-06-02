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
      -- * Extensible Parser Operators
    , parseAsBinder
    , parseAsTyBinder
    , parseAsPrefix
    , parseAsInfix
    , unparseAsBinder
    , unparseAsTyBinder
    , unparseAsPrefix
    , unparseAsInfix
    , parsesAsBinder
    , parsesAsTyBinder
    , parsesAsPrefix
    , parsesAsInfix
    , binders
    , tyBinders
    , prefixes
    , infixes
      -- * Overloading and Interface Mapping
    , getInterface
    , getOverloads
    , removeInterface
    , reduceInterface
    , overrideInterface
    , makeOverloadable
    , overloadInterface
    , prioritizeOverload
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
import HaskHOL.Core.Basics

import HaskHOL.Core.Parser.Lib
import qualified HaskHOL.Core.Parser.Prims as Prims
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.Parser.TermParser
import HaskHOL.Core.Parser.Elab
import HaskHOL.Core.Parser.Rep

-- Parser Methods
-- | Specifies a 'Text' to be recognized as a term binder by the parser.
parseAsBinder :: Text -> HOL Theory thry ()
parseAsBinder op = 
    Prims.overParseContext Prims.binders (\ ops -> nub (op : ops))

-- | Specifies a 'Text' to be recognized as a type binder by the parser.
parseAsTyBinder :: Text -> HOL Theory thry ()
parseAsTyBinder op = 
    Prims.overParseContext Prims.tyBinders (\ ops -> nub (op : ops))

-- | Specifies a 'Text' to be recognized as a prefix operator by the parser.
parseAsPrefix :: Text -> HOL Theory thry ()
parseAsPrefix op = 
    Prims.overParseContext Prims.prefixes (\ ops -> nub (op : ops))

{-| 
  Specifies a 'Text' to be recognized as an infix operator by the parser with
  a given precedence level and associativity.
-}
parseAsInfix :: (Text, (Int, Text)) -> HOL Theory thry ()
parseAsInfix i@(n, _) = Prims.overParseContext Prims.infixes insertFun
  where insertFun :: [(Text, (Int, Text))] -> [(Text, (Int, Text))]
        insertFun is
            | isJust (find (\ (n', _) -> n == n') is) = is
            | otherwise = sort (\ (s, (x, a)) (t, (y, b)) ->
                  x < y || x == y && a > b || x == y && a == b && s < t) (i:is) 

-- | Specifies a 'Text' for the parser to stop recognizing as a term binder.
unparseAsBinder :: Text -> HOL Theory thry ()
unparseAsBinder op = Prims.overParseContext Prims.binders (delete op)

-- | Specifies a 'Text' for the parser to stop recognizing as a type binder.
unparseAsTyBinder :: Text -> HOL Theory thry ()
unparseAsTyBinder op = Prims.overParseContext Prims.tyBinders (delete op)

{-| 
  Specifies a 'Text' for the parser to stop recognizing as a prefix operator.
-}
unparseAsPrefix :: Text -> HOL Theory thry ()
unparseAsPrefix op = Prims.overParseContext Prims.prefixes (delete op)

{-| 
  Specifies a 'Text' for the parser to stop recognizing as an infix operator.
-}
unparseAsInfix :: Text -> HOL Theory thry ()
unparseAsInfix op = 
    Prims.overParseContext Prims.infixes (filter (\ (x, _) -> x /= op))

-- | Predicate for 'Text's recognized as term binders by the parser.
parsesAsBinder :: Text -> HOL cls thry Bool
parsesAsBinder op = Prims.testParseContext Prims.binders (elem op)

-- | Predicate for 'Text's recognized as type binders by the parser.
parsesAsTyBinder :: Text -> HOL cls thry Bool
parsesAsTyBinder op = Prims.testParseContext Prims.tyBinders (elem op)

-- | Predicate for 'Text's recognized as prefix operators by the parser.
parsesAsPrefix :: Text -> HOL cls thry Bool
parsesAsPrefix op = Prims.testParseContext Prims.prefixes (elem op)

-- | Predicate for 'Text's recognized as infix operators by the parser.
parsesAsInfix :: Text -> HOL cls thry Bool
parsesAsInfix op = Prims.testParseContext Prims.infixes (isJust . lookup op)

-- | Returns all binders defined in the context.
binders :: HOL cls thry [Text]
binders = Prims.viewParseContext Prims.binders

-- | Returns all type binders defined in the context.
tyBinders :: HOL cls thry [Text]
tyBinders = Prims.viewParseContext Prims.tyBinders

-- | Returns all prefix operators defined in the context.
prefixes :: HOL cls thry [Text]
prefixes = Prims.viewParseContext Prims.prefixes

-- | Returns all infix operators defined in the context.
infixes :: HOL cls thry [(Text, (Int, Text))]
infixes = Prims.viewParseContext Prims.infixes

-- Interface
-- | Removes all instances of an overloaded symbol from the interface.
removeInterface :: Text -> HOL Theory thry ()
removeInterface sym = 
    Prims.overParseContext Prims.interface (filter (\ (x, _) -> x /= sym))

{-| 
  Removes a specific instance of an overloaded symbol from the interface.  
  Throws a 'HOLException' if the provided term is not a constant or varible term
  representing an instance of the overloaded symbol.
-}
reduceInterface :: Text -> HOLTerm -> HOL Theory thry ()
reduceInterface sym tm =
    do namty <- liftMaybe "reduceInterface: term not a constant or variable" $ 
                  destConst tm <|> destVar tm
       Prims.overParseContext Prims.interface (delete (sym, namty))

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
overrideInterface :: Text -> HOLTerm -> HOL Theory thry ()
overrideInterface sym tm =
    do namty <- liftMaybe "overrideInterface: term not a constant or variable" $
                  destConst tm <|> destVar tm
       let m = Prims.overParseContext Prims.interface 
                 (\ ifc -> (sym, namty) : filter (\ (x, _) -> x /= sym) ifc)
       overs <- getOverloads
       case sym `mapLookup` overs of
         Just gty -> if isNothing $ typeMatch gty (snd namty) ([], [], [])
                     then fail $ "overrideInterface: " ++
                                 "not an instance of type skeleton"
                     else m
         _ -> m

{-|
  Specifies a 'Text' that can act as an overloadable identifier within the
  parser.  The provided type is the most general type that instances of this
  symbol may have.  Throws a 'HOLException' if the given symbol has already been
  declared as overloadable with a different type.

  Note that defining a symbol as overloadable will erase any interface overloads
  that were previously introduced via 'overrideInterface' in order to guarantee
  that all overloads are matchable with their most general type.
-}
makeOverloadable :: Text -> HOLType -> HOL Theory thry ()
makeOverloadable s gty =
    do overs <- getOverloads
       case mapLookup s overs of
         Just ty
             | gty == ty -> return ()
             | otherwise -> 
                 fail "makeOverloadable: differs from existing skeleton"
         _ -> do Prims.overParseContext Prims.overloads (mapInsert s gty)
                 removeInterface s

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
overloadInterface :: Text -> HOLTerm -> HOL Theory thry ()
overloadInterface sym tm =
    do overs <- getOverloads
       gty <- liftMaybe ("overloadInstace: symbol " ++ show sym ++ 
                         " is not overloadable.") $ mapLookup sym overs
       namty <- liftMaybe "overloadInstance: term not a constant or variable" $ 
                  destConst tm <|> destVar tm
       if isNothing $ typeMatch gty (snd namty) ([], [], [])
          then fail "overloadInstance: not an instance of type skeleton"
          else let i = (sym, namty) in
                 Prims.overParseContext Prims.interface 
                   (\ ifc -> i : delete i ifc)

{-|
  Specifies a type to prioritize when the interface is used to overload a 
  symbol.  Note that this applies to all overloads in the system whose match
  with the specified most general type involves the provided type.  
  Prioritization is done by redefining overloads via 'overloadInterface'.
-}
prioritizeOverload :: HOLType -> HOL Theory thry ()
prioritizeOverload ty =
    do overs <- getOverloads
       mapM_ (\ (s, gty) -> 
              (do iface <- getInterface
                  let (n, t') = fromJust $ 
                                tryFind (\ (s', x@(_, t)) ->
                                         if s' /= s then Nothing
                                         else do (ts, _, _) <- typeMatch gty t
                                                                 ([], [], [])
                                                 _ <- ty `revLookup` ts
                                                 return x) iface
                  overloadInterface s $ mkVar n t')
              <|> return ()) $ mapToList overs

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
