{-# LANGUAGE FlexibleContexts, PatternSynonyms, TypeFamilies #-}
{-|
  Module:    HaskHOL.Core.Parser.Lib
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
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
    , (<|>)
      -- * Parser state manipulations
    , getState
    , gets
    , setState
    , updateState
    ) where

import HaskHOL.Core.Lib hiding ((<|>), (<?>))
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

import Text.Parsec hiding 
    (runParser, setState, getState, updateState,ParseError)
import qualified Text.Parsec as P
import Text.Parsec.Language
import Text.Parsec.Token

import Control.Lens hiding (op)


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
runHOLParser :: MonadThrow m => MyParser a -> ParseContext -> Text -> m a
runHOLParser parser ctxt input =
    either (fail' . show) return $ runParser parser (mapEmpty, 0, ctxt) "" input

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
mysymbol s = pack `fmap` symbol lexer s

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
myidentifier = pack `fmap` identifier lexer

-- | A version of 'integer' for our language.
myinteger :: MyParser Integer
myinteger = integer lexer

-- | A version of 'operator' for our language.
myoperator :: MyParser Text
myoperator = pack `fmap` operator lexer

-- | A version of 'reservedOp' for our language.
myreservedOp :: String -> MyParser ()
myreservedOp = reservedOp lexer

-- | Selects the first matching symbol.
choiceSym :: [String] -> MyParser Text
choiceSym ops = choice $ map mysymbol ops

-- | Selects the first matching reserved operator.
choiceId :: [Text] -> MyParser Text
choiceId ops = choice $ map 
               (\ s -> mytry $ do s' <- myidentifier <|> myoperator
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
