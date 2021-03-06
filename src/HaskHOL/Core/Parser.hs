{-# LANGUAGE FlexibleContexts #-}
{-|
  Module:    HaskHOL.Core.Parser
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
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
      -- * Hidden Constant Mappings
    , getHidden
    , hideConstant
    , unhideConstant
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
  (HOL, Theory, overParseContext, viewParseContext, testParseContext)
import qualified HaskHOL.Core.State.Monad as State

import HaskHOL.Core.Parser.Lib hiding ((<|>))
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.Parser.TermParser
import HaskHOL.Core.Parser.Elab
import HaskHOL.Core.Parser.Rep

-- Parser Methods
-- | Specifies a 'Text' to be recognized as a term binder by the parser.
parseAsBinder :: Text -> HOL Theory thry ()
parseAsBinder op = let fun ops = nub (op : ops) in
    overParseContext State.binders fun

-- | Specifies a 'Text' to be recognized as a type binder by the parser.
parseAsTyBinder :: Text -> HOL Theory thry ()
parseAsTyBinder op = let fun ops = nub (op : ops) in
    overParseContext State.tyBinders fun

-- | Specifies a 'Text' to be recognized as a prefix operator by the parser.
parseAsPrefix :: Text -> HOL Theory thry ()
parseAsPrefix op = let fun ops = nub (op : ops) in
    overParseContext State.prefixes fun

{-| 
  Specifies a 'Text' to be recognized as an infix operator by the parser with
  a given precedence level and associativity.
-}
parseAsInfix :: (Text, (Int, Text)) -> HOL Theory thry ()
parseAsInfix i@(n, (p, as)) = 
    do overParseContext State.infixes insertFun
       let f = if as == "right" then State.rights else State.lefts
       overParseContext f (\ ops -> nub ((n, p) : ops))
  where insertFun :: [(Text, (Int, Text))] -> [(Text, (Int, Text))]
        insertFun is
            | test' (find (\ (n', _) -> n == n') is) = is
            | otherwise = sort (\ (s, (x, a)) (t, (y, b)) ->
                  x < y || x == y && a > b || x == y && a == b && s < t) (i:is) 

-- | Specifies a 'Text' for the parser to stop recognizing as a term binder.
unparseAsBinder :: Text -> HOL Theory thry ()
unparseAsBinder op = let fun = delete op in
    overParseContext State.binders fun

-- | Specifies a 'Text' for the parser to stop recognizing as a type binder.
unparseAsTyBinder :: Text -> HOL Theory thry ()
unparseAsTyBinder op = let fun = delete op in
    overParseContext State.tyBinders fun
{-| 
  Specifies a 'Text' for the parser to stop recognizing as a prefix operator.
-}
unparseAsPrefix :: Text -> HOL Theory thry ()
unparseAsPrefix op = let fun = delete op in
    overParseContext State.prefixes fun

{-| 
  Specifies a 'Text' for the parser to stop recognizing as an infix operator.
-}
unparseAsInfix :: Text -> HOL Theory thry ()
unparseAsInfix op = let fun = filter (\ (x, _) -> x /= op) in
    do overParseContext State.infixes fun
       overParseContext State.rights fun
       overParseContext State.lefts fun

-- | Predicate for 'Text's recognized as term binders by the parser.
parsesAsBinder :: Text -> HOL cls thry Bool
parsesAsBinder op = testParseContext State.binders (elem op)

-- | Predicate for 'Text's recognized as type binders by the parser.
parsesAsTyBinder :: Text -> HOL cls thry Bool
parsesAsTyBinder op = testParseContext State.tyBinders (elem op)

-- | Predicate for 'Text's recognized as prefix operators by the parser.
parsesAsPrefix :: Text -> HOL cls thry Bool
parsesAsPrefix op = testParseContext State.prefixes (elem op)

-- | Predicate for 'Text's recognized as infix operators by the parser.
parsesAsInfix :: Text -> HOL cls thry Bool
parsesAsInfix op = testParseContext State.infixes (test' . assoc op)

-- | Returns all binders defined in the context.
binders :: HOL cls thry [Text]
binders = viewParseContext State.binders

-- | Returns all type binders defined in the context.
tyBinders :: HOL cls thry [Text]
tyBinders = viewParseContext State.tyBinders

-- | Returns all prefix operators defined in the context.
prefixes :: HOL cls thry [Text]
prefixes = viewParseContext State.prefixes

-- | Returns all infix operators defined in the context.
infixes :: HOL cls thry [(Text, (Int, Text))]
infixes = viewParseContext State.infixes

-- Interface
-- | Returns the current parser interface.
getInterface :: HOL cls thry [(Text, (Text, HOLType))]
getInterface = viewParseContext State.interface

-- | Returns the current mapping of overloads for the parser.
getOverloads :: HOL cls thry (Map Text HOLType)
getOverloads = viewParseContext State.overloads

-- | Removes all instances of an overloaded symbol from the interface.
removeInterface :: Text -> HOL Theory thry ()
removeInterface sym = let fun = filter (\ (x, _) -> x /= sym) in
    overParseContext State.interface fun
{-| 
  Removes a specific instance of an overloaded symbol from the interface.  
  Throws a 'HOLException' if the provided term is not a constant or varible term
  representing an instance of the overloaded symbol.
-}
reduceInterface :: Text -> HOLTerm -> HOL Theory thry ()
reduceInterface sym tm =
    do namty <- destConst tm <|> destVar tm <?> 
                  "reduceInterface: term not a constant or variable"
       let fun = delete (sym, namty)
       overParseContext State.interface fun
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
    do namty <- destConst tm <|> destVar tm <?> 
                  "overrideInterface: term not a constant or variable"
       let fun ifc = (sym, namty) : filter (\ (x, _) -> x /= sym) ifc
           m = overParseContext State.interface fun
       overs <- getOverloads
       case runCatch $ sym `mapAssoc` overs of
         Right gty -> if not . test' $ typeMatch gty (snd namty) ([], [], [])
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
       case runCatch $ mapAssoc s overs of
         Right ty
             | gty == ty -> return ()
             | otherwise -> 
                 fail "makeOverloadable: differs from existing skeleton"
         _ -> do overParseContext State.overloads (mapInsert s gty)
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
       gty <- mapAssoc sym overs <?> ("overloadInstace: symbol " ++ show sym ++ 
                                      " is not overloadable.")
       namty <- destConst tm <|> destVar tm <?>
                  "overloadInstance: term not a constant or variable"
       if not . test' $ typeMatch gty (snd namty) ([], [], [])
          then fail "overloadInstance: not an instance of type skeleton"
          else let i = (sym, namty) 
                   fun ifc = i : delete i ifc in
                 overParseContext State.interface fun

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
                  (n, t') <- tryFind (\ (s', x@(_, t)) ->
                                         if s' /= s then fail' "tryFind"
                                         else do (ts, _, _) <- typeMatch gty t
                                                                 ([], [], [])
                                                 _ <- ty `revAssoc` ts
                                                 return x) iface
                  overloadInterface s $ mkVar n t')
              <|> return ()) $ mapToList overs

-- Hidden Constants
getHidden :: HOL cls thry [Text]
getHidden = viewParseContext State.hidden

-- | Specifies a 'Text' for the parser to stop recognizing as a constant.
hideConstant :: Text -> HOL Theory thry ()
hideConstant sym =
    overParseContext State.hidden (\ syms -> sym : syms)

-- | Specifies a 'Text' for the parser to resume recognizing as a constant.
unhideConstant :: Text -> HOL Theory thry ()
unhideConstant sym = 
    overParseContext State.hidden (\ syms -> syms \\ [sym])

-- Type Abbreviations
{-| 
  Specifies a 'Text' to act as an abbreviation for a given type in the parser.
  Upon recognizing the abbreviation the parser will replace it with the 
  'PreType' value for it's associated 'HOLType' such that the elaborator can
  infer the correct type for polymorphic abbreviations.
-}
newTypeAbbrev :: Text -> HOLType -> HOL Theory thry ()
newTypeAbbrev s ty = 
    overParseContext State.typeAbbrevs (mapInsert s ty)

{-| 
  Specifies a 'Text' for the parser to stop recognizing as a type 
  abbreviation.
-}
removeTypeAbbrev :: Text -> HOL Theory thry ()
removeTypeAbbrev s = overParseContext State.typeAbbrevs (mapDelete s)

{-| 
  Returns all 'Text's currently acting as type abbreviations in the parser
  paired with their associated types.
-}
typeAbbrevs :: HOL cls thry (Map Text HOLType)
typeAbbrevs = viewParseContext State.typeAbbrevs
