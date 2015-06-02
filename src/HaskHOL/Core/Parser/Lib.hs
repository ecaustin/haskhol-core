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
    , ParseContext
    , runHOLParser
    , parseContext
    , initParseContext
    , runParser
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
    , gets
    , setState
    , updateState
    ) where

import HaskHOL.Core.Lib hiding ((<?>))
import HaskHOL.Core.Kernel
import HaskHOL.Core.State

import HaskHOL.Core.Parser.Prims

import Text.Parsec hiding (runParser, setState, getState, updateState
                          ,ParseError, (<|>))
import qualified Text.Parsec as P
import Text.Parsec.Language
import Text.Parsec.Token

import Control.Lens hiding (op)

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
type MyParser = Parsec Text (Map Text Int, Int, ParseContext)

{-| Runs a custom parser when provided with an input 'String' and a 
    'HOLContext'.
-}
runHOLParser :: MyParser a -> ParseContext -> Text -> Either String a
runHOLParser parser ctxt input =
    either (Left . show) return $ runParser parser (mapEmpty, 0, ctxt) "" input

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
lexer :: GenTokenParser Text (Map Text Int, Int, ParseContext) Identity
lexer = makeTokenParser langDef

-- | A version of 'symbol' for our language.
mysymbol :: String -> MyParser Text
mysymbol = liftM pack . symbol lexer

-- | A version of 'parens' for our language.
myparens :: MyParser a -> MyParser a
myparens = parens lexer

-- | A version of 'braces' for our language.
mybraces :: MyParser a -> MyParser a
mybraces = braces lexer

-- | A version of 'brackets' for our language.
mybrackets :: MyParser a -> MyParser a
mybrackets = brackets lexer

-- | A version of 'commaSep1' for our language.
mycommaSep1 :: MyParser a -> MyParser [a]
mycommaSep1 = commaSep1 lexer

-- | A version of 'semiSep' for our language.
mysemiSep :: MyParser a -> MyParser [a]
mysemiSep = semiSep lexer

-- | A version of 'semiSep1' for our language.
mysemiSep1 :: MyParser a -> MyParser [a]
mysemiSep1 = semiSep1 lexer

-- | A version of 'reserved' for our language.
myreserved :: String -> MyParser ()
myreserved = reserved lexer

-- | A version of 'identifier' for our language.
myidentifier :: MyParser Text
myidentifier = liftM pack $ identifier lexer

-- | A version of 'integer' for our language.
myinteger :: MyParser Integer
myinteger = integer lexer

-- | A version of 'operator' for our language.
myoperator :: MyParser Text
myoperator = liftM pack $ operator lexer

-- | A version of 'reservedOp' for our language.
myreservedOp :: String -> MyParser ()
myreservedOp = reservedOp lexer

-- | Selects the first matching symbol.
choiceSym :: [String] -> MyParser Text
choiceSym ops = choice $ map mysymbol ops

-- | Selects the first matching reserved operator.
choiceId :: [Text] -> MyParser Text
choiceId ops = choice $ map 
               (\ s -> try $ do s' <- myidentifier <|> myoperator
                                if s' == s
                                   then return s
                                   else fail "choiceId") ops

-- | A version of 'whiteSpace' for our language.
mywhiteSpace :: MyParser ()
mywhiteSpace = whiteSpace lexer

-- | A re-export of 'P.many'.
mymany :: MyParser a -> MyParser [a]
mymany = P.many

-- | A re-export of 'P.many1'.
mymany1 :: MyParser a -> MyParser [a]
mymany1 = P.many1

-- | A re-export of 'P.sepBy1'.
mysepBy1 :: MyParser a -> MyParser b -> MyParser [a]
mysepBy1 = P.sepBy1

-- | A re-export of 'P.try'.
mytry :: MyParser a -> MyParser a
mytry = P.try

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
runParser :: Stream s Identity t => Parsec s u a -> u -> SourceName -> s 
          -> Either ParseError a
runParser = P.runParser

-- | A re-export of 'P.getState'.
getState :: Monad m => P.ParsecT s u m u
getState = P.getState

-- | A version of 'getState' that retrieves a specified field of the context.
gets :: Monad m => (u3 -> a) -> P.ParsecT s (u1, u2, u3) m a
gets f =
    do (_, _, ctxt) <- getState
       return $! f ctxt

-- | A re-export of 'P.setState'.
setState :: Monad m => u -> P.ParsecT s u m ()
setState = P.setState

-- | A re-export of 'P.updateState'.
updateState :: Monad m => (u -> u) -> P.ParsecT s u m ()
updateState = P.updateState
