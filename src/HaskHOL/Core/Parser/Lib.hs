{-# LANGUAGE FlexibleContexts, PatternSynonyms, TypeFamilies #-}
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

  To see what is actually exported to the user, see the module 
  "HaskHOL.Core.Parser".
-}

module HaskHOL.Core.Parser.Lib
    ( -- * Parser Utilities and Types
      PreType(..)
    , PreTerm(..)
    , dpty
    , pretypeOfType
    , MyParser
    , ParseError
    , runParserT
    , langDef
    , lexer
    , mysymbol
    , myparens
    , mybraces
    , mybrackets
    , mycommaSep1
    , mysemiSep
    , mysemiSep1
    , myreserved
    , myidentifier
    , myinteger
    , myoperator
    , myreservedOp
    , choiceSym
    , choiceId
    , mywhiteSpace
    , mymany
    , mymany1
    , mysepBy1
    , mytry
      -- * Type Elaboration Flags
    , FlagIgnoreConstVarstruct(..)
    , FlagTyInvWarning(..)
    , FlagTyOpInvWarning(..)
    , FlagAddTyAppsAuto(..)
      -- * Pretty Printer Flags
    , FlagRevInterface(..)
    , FlagPrintAllThm(..)
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
    , makeOverloadable
    , removeInterface
    , reduceInterface
    , overrideInterface
    , overloadInterface
    , prioritizeOverload
    , getInterface
    , getOverloads
      -- * Type Abbreviations
    , newTypeAbbrev
    , removeTypeAbbrev
    , typeAbbrevs
      -- * Hidden Constant Mapping 
    , hideConstant
    , unhideConstant
    , getHidden
      -- * Extensible Printer Operators
    , addUnspacedBinop 
    , addPrebrokenBinop
    , removeUnspacedBinop
    , removePrebrokenBinop
    , getUnspacedBinops
    , getPrebrokenBinops
      -- * Parser state manipulations
    , getState
    , setState
    , updateState
    ) where

import HaskHOL.Core.Lib hiding ((<?>))
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics
import HaskHOL.Core.Parser.Prims

import Text.Parsec hiding (runParserT, setState, getState, updateState
                          ,ParseError, (<|>))
import qualified Text.Parsec as P
import Text.Parsec.Language
import Text.Parsec.Token

import {-# SOURCE #-} HaskHOL.Core.Parser.Rep

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
newFlag "FlagTyInvWarning" False

{-|
  Flag indicating that the user should be warned if a type operator variable was
  invented during parsing.
-}
newFlag "FlagTyOpInvWarning" False

{-|
  Flag to say whether implicit type applications are to be added during parsing.
-}
newFlag "FlagAddTyAppsAuto" True

-- | Flag to indicate whether the interface should be reversed on printing.
newFlag "FlagRevInterface" True

{-| 
  Flag to indicate if the entirety of a theorem should be printed, as opposed
  to just the conclusion term.
-}
newFlag "FlagPrintAllThm" True

data BinderOps = BinderOps ![Text] deriving Typeable

deriveSafeCopy 0 'base ''BinderOps

initBinderOps :: [Text]
initBinderOps = ["\\"]

insertBinder :: Text -> Update BinderOps ()
insertBinder op =
    do (BinderOps ops) <- get
       put (BinderOps (op:ops))

removeBinder :: Text -> Update BinderOps ()
removeBinder op =
    do (BinderOps ops) <- get
       put (BinderOps (op `delete` ops))

getBinders :: Query BinderOps [Text]
getBinders =
    do (BinderOps ops) <- ask
       return ops

makeAcidic ''BinderOps ['insertBinder, 'removeBinder, 'getBinders]


data TyBinderOps = TyBinderOps ![Text] deriving Typeable

deriveSafeCopy 0 'base ''TyBinderOps

initTyBinderOps :: [Text]
initTyBinderOps = ["\\\\"]

insertTyBinder :: Text -> Update TyBinderOps ()
insertTyBinder op =
    do (TyBinderOps ops) <- get
       put (TyBinderOps (op:ops))

removeTyBinder :: Text -> Update TyBinderOps ()
removeTyBinder op =
    do (TyBinderOps ops) <- get
       put (TyBinderOps (op `delete` ops))

getTyBinders :: Query TyBinderOps [Text]
getTyBinders =
    do (TyBinderOps ops) <- ask
       return ops

makeAcidic ''TyBinderOps ['insertTyBinder, 'removeTyBinder, 'getTyBinders]

data PrefixOps = PrefixOps ![Text] deriving Typeable

deriveSafeCopy 0 'base ''PrefixOps

insertPrefix :: Text -> Update PrefixOps ()
insertPrefix op =
    do (PrefixOps ops) <- get
       put (PrefixOps (op:ops))

removePrefix :: Text -> Update PrefixOps ()
removePrefix op =
    do (PrefixOps ops) <- get
       put (PrefixOps (op `delete` ops))

getPrefixes :: Query PrefixOps [Text]
getPrefixes =
    do (PrefixOps ops) <- ask
       return ops

makeAcidic ''PrefixOps ['insertPrefix, 'removePrefix, 'getPrefixes]

data InfixOps = InfixOps ![(Text, (Int, Text))] deriving Typeable

deriveSafeCopy 0 'base ''InfixOps

initInfixOps :: [(Text, (Int, Text))]
initInfixOps = [("=", (12, "right"))]

insertInfix :: (Text, (Int, Text)) -> Update InfixOps ()
insertInfix i@(n, _) =
    do (InfixOps is) <- get 
       let is' = case find (\ (n', _) -> n == n') is of
                   Just _ -> is
                   _ -> 
                       sort (\ (s, (x, a)) (t, (y, b)) ->
                         x < y || x == y && a > b || x == y && a == b && s < t)
                         (i:is)
       put (InfixOps is')

removeInfix :: Text -> Update InfixOps ()
removeInfix op =
    do (InfixOps ops) <- get
       put (InfixOps (filter (\ (x, _) -> x /= op) ops))

getInfixes :: Query InfixOps [(Text, (Int, Text))]
getInfixes =
    do (InfixOps ops) <- ask
       return ops

makeAcidic ''InfixOps ['insertInfix, 'removeInfix, 'getInfixes]


data Interface = Interface ![(Text, (Text, HOLType))] deriving Typeable

deriveSafeCopy 0 'base ''Interface

insertInterface :: (Text, (Text, HOLType)) -> Update Interface ()
insertInterface x =
    do (Interface xs) <- get
       put (Interface (x:xs))

deleteInterface :: (Text, (Text, HOLType)) -> Update Interface ()
deleteInterface x =
    do (Interface xs) <- get
       put (Interface (x `delete` xs))

filterInterface :: Text -> Update Interface ()
filterInterface s =
    do (Interface xs) <- get
       put (Interface (filter (\ (x, _) -> x /= s) xs))

getInterfaces :: Query Interface [(Text, (Text, HOLType))]
getInterfaces =
    do (Interface xs) <- ask
       return xs

makeAcidic ''Interface 
    ['insertInterface, 'deleteInterface, 'filterInterface, 'getInterfaces]


data Overload = Overload !(Map Text HOLType) deriving Typeable

deriveSafeCopy 0 'base ''Overload

insertOverload :: Text -> HOLType -> Update Overload ()
insertOverload s ty =
    do (Overload xs) <- get
       put (Overload (mapInsert s ty xs))

getOverload :: Query Overload (Map Text HOLType)
getOverload =
    do (Overload xs) <- ask
       return xs

makeAcidic ''Overload ['insertOverload, 'getOverload]


data TypeAbbreviations = TypeAbbreviations !(Map Text HOLType) 
    deriving Typeable

deriveSafeCopy 0 'base ''TypeAbbreviations

insertTypeAbbreviation :: Text -> HOLType -> Update TypeAbbreviations ()
insertTypeAbbreviation x ty =
    do (TypeAbbreviations xs) <- get
       put (TypeAbbreviations (mapInsert x ty xs))

removeTypeAbbreviation :: Text -> Update TypeAbbreviations ()
removeTypeAbbreviation x =
    do (TypeAbbreviations xs) <- get
       put (TypeAbbreviations (x `mapDelete` xs))

getTypeAbbreviations :: Query TypeAbbreviations (Map Text HOLType)
getTypeAbbreviations =
    do (TypeAbbreviations xs) <- ask
       return xs

makeAcidic ''TypeAbbreviations 
    ['insertTypeAbbreviation, 'removeTypeAbbreviation, 'getTypeAbbreviations]


data Hidden = Hidden [Text] deriving Typeable

deriveSafeCopy 0 'base ''Hidden

insertHidden :: Text -> Update Hidden ()
insertHidden x =
    do (Hidden xs) <- get
       put (Hidden (x `insert` xs))

removeHidden :: Text -> Update Hidden ()
removeHidden x =
    do (Hidden xs) <- get
       put (Hidden (x `delete` xs))

getHiddens :: Query Hidden [Text]
getHiddens =
    do (Hidden xs) <- ask
       return xs

makeAcidic ''Hidden ['insertHidden, 'removeHidden, 'getHiddens]


data UnspacedBinops = UnspacedBinops ![Text] deriving Typeable

deriveSafeCopy 0 'base ''UnspacedBinops

initUnspaced :: [Text]
initUnspaced = [",", "..", "$"]

insertUnspaced :: Text -> Update UnspacedBinops ()
insertUnspaced op =
    do (UnspacedBinops ops) <- get
       put (UnspacedBinops (op:ops))

removeUnspaced :: Text -> Update UnspacedBinops ()
removeUnspaced op =
    do (UnspacedBinops ops) <- get
       put (UnspacedBinops (ops \\ [op]))

getUnspaced :: Query UnspacedBinops [Text]
getUnspaced =
    do (UnspacedBinops ops) <- ask
       return ops

makeAcidic ''UnspacedBinops ['insertUnspaced, 'removeUnspaced, 'getUnspaced]


data PrebrokenBinops = PrebrokenBinops ![Text] deriving Typeable

deriveSafeCopy 0 'base ''PrebrokenBinops

initPrebroken :: [Text]
initPrebroken = ["==>"]

insertPrebroken :: Text -> Update PrebrokenBinops ()
insertPrebroken op =
    do (PrebrokenBinops ops) <- get
       put (PrebrokenBinops (op:ops))

removePrebroken :: Text -> Update PrebrokenBinops ()
removePrebroken op =
    do (PrebrokenBinops ops) <- get
       put (PrebrokenBinops (ops \\ [op]))

getPrebroken :: Query PrebrokenBinops [Text]
getPrebroken =
    do (PrebrokenBinops ops) <- ask
       return ops

makeAcidic ''PrebrokenBinops ['insertPrebroken, 'removePrebroken, 'getPrebroken]

-- | The default 'PreType' to be used as a blank for the type inference engine.
dpty :: PreType
dpty = PTyComb (PTyCon textEmpty) []

-- | Converts a 'HOLType' to 'PreType'
pretypeOfType :: HOLType -> PreType
pretypeOfType (TyVar f v) = UTyVar f v 0
pretypeOfType (TyApp tyop args) =
    let (s, n) = destTypeOp tyop
        tyop' = if n == -1 then UTyVar False s (length args) else PTyCon s in
      PTyComb tyop' $ map pretypeOfType args
pretypeOfType (UType tv tb) = 
    PUTy (pretypeOfType tv) $ pretypeOfType tb
pretypeOfType _ = error "pretypeOfType: exhaustive warning."

{-| 
  An alias to a stateful 'GenParser' that carries a 'HOLContext', a list of
  known type operator variables with their arity, and a counter  The list of
  operators is used to guarantee that all instances of a type operator variable
  in a term have the same arity.  The counter is used to generate fresh names
  in a term.
-}
type MyParser cls thry = ParsecT Text (Map Text Int, Int) (HOL cls thry)

-- | The Parsec 'LanguageDef' for HaskHOL.
langDef :: Monad m => GenLanguageDef Text st m
langDef = LanguageDef
    { commentStart = ""
    , commentEnd = ""
    , commentLine = ""
    , nestedComments = True
    , identStart = alphaNum <|> char '_'
    , identLetter = alphaNum <|> oneOf "_'"
    , opStart = oneOf ",:!#$%&*+./<=>?@\\^|-~"
    , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedNames = [ "TYINST", "let", "and", "in", "if", "then", "else"
                      , "match", "with", "when", "function" ]
    , reservedOpNames = ["(", ")", "[", "]", "{", "}"
                        , "%", "_", "'", "->", ".", ":", ";", "|"]
    , caseSensitive = True
    }

-- | The Parsec token parser for HaskHOL.
lexer :: GenTokenParser Text (Map Text Int, Int) (HOL cls thry)
lexer = makeTokenParser langDef

-- | A version of 'symbol' for our language.
mysymbol :: String -> MyParser cls thry Text
mysymbol = liftM pack . symbol lexer

-- | A version of 'parens' for our language.
myparens :: MyParser cls thry a -> MyParser cls thry a
myparens = parens lexer

-- | A version of 'braces' for our language.
mybraces :: MyParser cls thry a -> MyParser cls thry a
mybraces = braces lexer

-- | A version of 'brackets' for our language.
mybrackets :: MyParser cls thry a -> MyParser cls thry a
mybrackets = brackets lexer

-- | A version of 'commaSep1' for our language.
mycommaSep1 :: MyParser cls thry a -> MyParser cls thry [a]
mycommaSep1 = commaSep1 lexer

-- | A version of 'semiSep' for our language.
mysemiSep :: MyParser cls thry a -> MyParser cls thry [a]
mysemiSep = semiSep lexer

-- | A version of 'semiSep1' for our language.
mysemiSep1 :: MyParser cls thry a -> MyParser cls thry [a]
mysemiSep1 = semiSep1 lexer

-- | A version of 'reserved' for our language.
myreserved :: String -> MyParser cls thry ()
myreserved = reserved lexer

-- | A version of 'identifier' for our language.
myidentifier :: MyParser cls thry Text
myidentifier = liftM pack $ identifier lexer

-- | A version of 'integer' for our language.
myinteger :: MyParser cls thry Integer
myinteger = integer lexer

-- | A version of 'operator' for our language.
myoperator :: MyParser cls thry Text
myoperator = liftM pack $ operator lexer

-- | A version of 'reservedOp' for our language.
myreservedOp :: String -> MyParser cls thry ()
myreservedOp = reservedOp lexer

-- | Selects the first matching symbol.
choiceSym :: [String] -> MyParser cls thry Text
choiceSym ops = choice $ map mysymbol ops

-- | Selects the first matching reserved operator.
choiceId :: [Text] -> MyParser cls thry Text
choiceId ops = choice $ map 
               (\ s -> try $ do s' <- myidentifier <|> myoperator
                                if s' == s
                                   then return s
                                   else fail "choiceId") ops

-- | A version of 'whiteSpace' for our language.
mywhiteSpace :: MyParser cls thry ()
mywhiteSpace = whiteSpace lexer

-- | A re-export of 'P.many'.
mymany :: MyParser cls thry a -> MyParser cls thry [a]
mymany = P.many

-- | A re-export of 'P.many1'.
mymany1 :: MyParser cls thry a -> MyParser cls thry [a]
mymany1 = P.many1

-- | A re-export of 'P.sepBy1'.
mysepBy1 :: MyParser cls thry a -> MyParser cls thry b -> MyParser cls thry [a]
mysepBy1 = P.sepBy1

-- | A re-export of 'P.try'.
mytry :: MyParser cls thry a -> MyParser cls thry a
mytry = P.try

-- | Specifies a 'Text' to be recognized as a term binder by the parser.
parseAsBinder :: Text -> HOL Theory thry ()
parseAsBinder op =
    do acid <- openLocalStateHOL (BinderOps initBinderOps)
       updateHOL acid (InsertBinder op)
       createCheckpointAndCloseHOL acid

-- | Specifies a 'Text' to be recognized as a type binder by the parser.
parseAsTyBinder :: Text -> HOL Theory thry ()
parseAsTyBinder op =
    do acid <- openLocalStateHOL (TyBinderOps initTyBinderOps)
       updateHOL acid (InsertTyBinder op)
       createCheckpointAndCloseHOL acid

-- | Specifies a 'Text' to be recognized as a prefix operator by the parser.
parseAsPrefix :: Text -> HOL Theory thry ()
parseAsPrefix op =
    do acid <- openLocalStateHOL (PrefixOps [])
       updateHOL acid (InsertPrefix op)
       createCheckpointAndCloseHOL acid

{-| 
  Specifies a 'Text' to be recognized as an infix operator by the parser with
  a given precedence level and associativity.
-}
parseAsInfix :: (Text, (Int, Text)) -> HOL Theory thry ()
parseAsInfix op =
    do acid <- openLocalStateHOL (InfixOps initInfixOps)
       updateHOL acid (InsertInfix op)
       createCheckpointAndCloseHOL acid

-- | Specifies a 'Text' for the parser to stop recognizing as a term binder.
unparseAsBinder :: Text -> HOL Theory thry ()
unparseAsBinder op =
    do acid <- openLocalStateHOL (BinderOps initBinderOps)
       updateHOL acid (RemoveBinder op)
       createCheckpointAndCloseHOL acid

-- | Specifies a 'Text' for the parser to stop recognizing as a type binder.
unparseAsTyBinder :: Text -> HOL Theory thry ()
unparseAsTyBinder op =
    do acid <- openLocalStateHOL (TyBinderOps initTyBinderOps)
       updateHOL acid (RemoveTyBinder op)
       createCheckpointAndCloseHOL acid

{-| 
  Specifies a 'Text' for the parser to stop recognizing as a prefix operator.
-}
unparseAsPrefix :: Text -> HOL Theory thry ()
unparseAsPrefix op =
    do acid <- openLocalStateHOL (PrefixOps [])
       updateHOL acid (RemovePrefix op)
       createCheckpointAndCloseHOL acid

{-| 
  Specifies a 'Text' for the parser to stop recognizing as an infix operator.
-}
unparseAsInfix :: Text -> HOL Theory thry ()
unparseAsInfix op =
    do acid <- openLocalStateHOL (InfixOps initInfixOps)
       updateHOL acid (RemoveInfix op)
       createCheckpointAndCloseHOL acid

-- | Predicate for 'Text's recognized as term binders by the parser.
parsesAsBinder :: Text -> HOL cls thry Bool
parsesAsBinder op = liftM (elem op) binders

-- | Predicate for 'Text's recognized as type binders by the parser.
parsesAsTyBinder :: Text -> HOL cls thry Bool
parsesAsTyBinder op = liftM (elem op) tyBinders

-- | Predicate for 'Text's recognized as prefix operators by the parser.
parsesAsPrefix :: Text -> HOL cls thry Bool
parsesAsPrefix op = liftM (elem op) prefixes

-- | Predicate for 'Text's recognized as infix operators by the parser.
parsesAsInfix :: Text -> HOL cls thry Bool
parsesAsInfix op = liftM (isJust . lookup op) infixes

-- | Returns all binders defined in the context.
binders :: HOL cls thry [Text]
binders = 
    do acid <- openLocalStateHOL (BinderOps initBinderOps)
       binds <- queryHOL acid GetBinders
       closeAcidStateHOL acid
       return binds

-- | Returns all type binders defined in the context.
tyBinders :: HOL cls thry [Text]
tyBinders = 
    do acid <- openLocalStateHOL (TyBinderOps initTyBinderOps)
       binds <- queryHOL acid GetTyBinders
       closeAcidStateHOL acid
       return binds

-- | Returns all prefix operators defined in the context.
prefixes :: HOL cls thry [Text]
prefixes = 
    do acid <- openLocalStateHOL (PrefixOps [])
       pfxs <- queryHOL acid GetPrefixes
       closeAcidStateHOL acid
       return pfxs

-- | Returns all infix operators defined in the context.
infixes :: HOL cls thry [(Text, (Int, Text))]
infixes = 
    do acid <- openLocalStateHOL (InfixOps initInfixOps)
       ifxs <- queryHOL acid GetInfixes
       closeAcidStateHOL acid
       return ifxs

-- Interface
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
         _ -> do acid1 <- openLocalStateHOL (Overload mapEmpty)
                 updateHOL acid1 (InsertOverload s gty)
                 createCheckpointAndCloseHOL acid1
                 acid2 <- openLocalStateHOL (Interface [])
                 updateHOL acid2 (FilterInterface s)
                 createCheckpointAndCloseHOL acid2

-- | Removes all instances of an overloaded symbol from the interface.
removeInterface :: Text -> HOL Theory thry ()
removeInterface sym =
    do acid <- openLocalStateHOL (Interface [])
       updateHOL acid (FilterInterface sym)
       createCheckpointAndCloseHOL acid

{-| 
  Removes a specific instance of an overloaded symbol from the interface.  
  Throws a 'HOLException' if the provided term is not a constant or varible term
  representing an instance of the overloaded symbol.
-}
reduceInterface :: Text -> HOLTerm -> HOL Theory thry ()
reduceInterface sym tm =
    do namty <- liftMaybe "reduceInterface: term not a constant or variable" $ 
                  destConst tm <|> destVar tm
       acid <- openLocalStateHOL (Interface [])
       updateHOL acid (DeleteInterface (sym, namty))
       createCheckpointAndCloseHOL acid

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
       let m = do acid <- openLocalStateHOL (Interface [])
                  updateHOL acid (FilterInterface sym)
                  updateHOL acid (InsertInterface (sym, namty))
                  createCheckpointAndCloseHOL acid
       overs <- getOverloads
       case sym `mapLookup` overs of
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
overloadInterface :: Text -> HOLTerm -> HOL Theory thry ()
overloadInterface sym tm =
    do overs <- getOverloads
       gty <- liftMaybe ("overloadInstace: symbol " ++ show sym ++ 
                         " is not overloadable.") $ mapLookup sym overs
       namty <- liftMaybe "overloadInstance: term not a constant or variable" $ 
                  destConst tm <|> destVar tm
       if isNothing $ typeMatch gty (snd namty) ([], [], [])
          then fail "overloadInstance: not an instance of type skeleton"
          else do acid <- openLocalStateHOL (Interface [])
                  let i = (sym, namty)
                  updateHOL acid (DeleteInterface i)
                  updateHOL acid (InsertInterface i)
                  createCheckpointAndCloseHOL acid

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

-- | Returns the list of all currently defined interface overloads.
getInterface :: HOL cls thry [(Text, (Text, HOLType))]
getInterface =
    do acid <- openLocalStateHOL (Interface [])
       iface <- queryHOL acid GetInterfaces
       closeAcidStateHOL acid
       return iface

{-| 
  Returns the list of all overloadable symbols paired with their most generic 
  types.
-}
getOverloads :: HOL cls thry (Map Text HOLType)
getOverloads =
    do acid <- openLocalStateHOL (Overload mapEmpty)
       ovrlds <- queryHOL acid GetOverload
       closeAcidStateHOL acid
       return ovrlds

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
       acid <- openLocalStateHOL (TypeAbbreviations mapEmpty)
       updateHOL acid (InsertTypeAbbreviation s ty)
       createCheckpointAndCloseHOL acid

{-| 
  Specifies a 'Text' for the parser to stop recognizing as a type 
  abbreviation.
-}
removeTypeAbbrev :: Text -> HOL Theory thry ()
removeTypeAbbrev s =
    do acid <- openLocalStateHOL (TypeAbbreviations mapEmpty)
       updateHOL acid (RemoveTypeAbbreviation s)
       createCheckpointAndCloseHOL acid

{-| 
  Returns all 'Text's currently acting as type abbreviations in the parser
  paired with their associated types.
-}
typeAbbrevs :: HOL cls thry (Map Text HOLType)
typeAbbrevs =
    do acid <- openLocalStateHOL (TypeAbbreviations mapEmpty)
       abvs <- queryHOL acid GetTypeAbbreviations
       closeAcidStateHOL acid
       return abvs

-- Hidden Constant Mapping
-- | Specifies a 'Text' for the parser to stop recognizing as a constant.
hideConstant :: Text -> HOL Theory thry ()
hideConstant c =
    do acid <- openLocalStateHOL (Hidden [])
       updateHOL acid (InsertHidden c)
       createCheckpointAndCloseHOL acid

-- | Specifies a 'Text' for the parser to resume recognizing as a constant.
unhideConstant :: Text -> HOL Theory thry ()
unhideConstant c =
    do acid <- openLocalStateHOL (Hidden [])
       updateHOL acid (RemoveHidden c)
       createCheckpointAndCloseHOL acid

-- | Returns all 'Text's currently acting as constants hidden from the parser.
getHidden :: HOL cls thry [Text]
getHidden =
    do acid <- openLocalStateHOL (Hidden [])
       hids <- queryHOL acid GetHiddens
       closeAcidStateHOL acid
       return hids

{-| 
  Specifies a symbol to be recognized as an unspaced, binary operator by the
  printer.  Applications involving these operators will be built with the '<>'
  combinator as opposed to '<+>'.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
addUnspacedBinop :: Text -> HOL Theory thry ()
addUnspacedBinop op =
    do acid <- openLocalStateHOL (UnspacedBinops initUnspaced)
       updateHOL acid (InsertUnspaced op)
       createCheckpointAndCloseHOL acid

{-| 
  Specifies a symbol to be recognized as a prebroken, binary operator by the
  printer.  Applications involving these operators will have their right-hand
  side argument printed on the next line using the 'hang' combinator.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
addPrebrokenBinop :: Text -> HOL Theory thry ()
addPrebrokenBinop op =
    do acid <- openLocalStateHOL (PrebrokenBinops initPrebroken)
       updateHOL acid (InsertPrebroken op)
       createCheckpointAndCloseHOL acid

{-| 
  Specifies a symbol to stop being recognized as an unspaced, binary operator 
  by the printer.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
removeUnspacedBinop :: Text -> HOL Theory thry ()
removeUnspacedBinop op =
    do acid <- openLocalStateHOL (UnspacedBinops initUnspaced)
       updateHOL acid (RemoveUnspaced op)
       createCheckpointAndCloseHOL acid

{-| 
  Specifies a symbol to stop being recognized as an prebroken, binary operator 
  by the printer.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
removePrebrokenBinop :: Text -> HOL Theory thry ()
removePrebrokenBinop op =
    do acid <- openLocalStateHOL (PrebrokenBinops initPrebroken)
       updateHOL acid (RemovePrebroken op)
       createCheckpointAndCloseHOL acid

{-| 
  Returns the list of all symbols current recognized as unspaced, binary
  operators by the printer.
-}
getUnspacedBinops :: HOL cls thry [Text]
getUnspacedBinops =
    do acid <- openLocalStateHOL (UnspacedBinops initUnspaced)
       ops <- queryHOL acid GetUnspaced
       closeAcidStateHOL acid
       return ops

{-| 
  Returns the list of all symbols current recognized as prebroken, binary
  operators by the printer.
-}
getPrebrokenBinops :: HOL cls thry [Text]
getPrebrokenBinops =
    do acid <- openLocalStateHOL (PrebrokenBinops initUnspaced)
       ops <- queryHOL acid GetPrebroken
       closeAcidStateHOL acid
       return ops

-- Re-exports
-- | A re-export of 'P.runParserT'.
runParserT :: Stream s m t => ParsecT s u m a -> u -> SourceName -> s 
           -> m (Either ParseError a)
runParserT = P.runParserT

-- | A re-export of 'P.getState'.
getState :: Monad m => P.ParsecT s u m u
getState = P.getState

-- | A re-export of 'P.setState'.
setState :: Monad m => u -> P.ParsecT s u m ()
setState = P.setState

-- | A re-export of 'P.updateState'.
updateState :: Monad m => (u -> u) -> P.ParsecT s u m ()
updateState = P.updateState
