{-|
  Module:    HaskHOL.Core.Parser.TermParser
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines the parser for 'HOLTerm's that satisfies the following BNF
  grammar:

@
  PRETERM            :: APPL_PRETERM binop APPL_PRETERM                     
                      | APPL_PRETERM                                        
                                                                           
  APPL_PRETERM       :: BINDER_PRETERM+                               
                      | BINDER_PRETERM : type                                  
                                                                           
  BINDER_PRETERM     :: tybinder small-type-variables . PRETERM             
                      | binder VARSTRUCT_PRETERM+ . PRETERM                 
                      | let PRETERM and ... and PRETERM in PRETERM          
                      | TYPED_PRETERM                                       
                                                                            
  TYPED_PRETERM      :: TYINST (tyop-var : PRETYPE)+ ATOMIC_PRETERM         
                      | ATOMIC_PRETERM                                   
                                                                            
  VARSTRUCT_PRETERM  :: ATOMIC_PRETERM : type                               
                      | ATOMIC_PRETERM                                      
                                                                           
  ATOMIC_PRETERM     :: ( PRETERM )                                         
                      | [: type]                                 
                      | [ PRETERM; .. ; PRETERM ]                  
                      | if PRETERM then PRETERM else PRETERM                
                      | identifier                                          
@                                                                          
 
  Note that arbitrary atomic preterms, typed or untyped, are allowed as     
  varstructs in order to simplify parsing.  We do not make the same 
  simplification for @TYINST@ terms in order to avoid the mixing of terms, 
  types, and type operators.   

  Also note that a number of advanced HOL term features, mostly relating to sets
  and patterns, are not currently supported by the parser.  These will be added
  in as the relevant logic libraries are added to the system.

  As a heads up, the error messages thrown by this parser leave much to be
  desired.
-}
module HaskHOL.Core.Parser.TermParser
    ( pterm
    ) where

import HaskHOL.Core.Lib hiding (many)
import HaskHOL.Core.State

import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.TypeParser

-- | Parser for HOL terms.
pterm :: MyParser thry PreTerm
pterm = 
    do mywhiteSpace
       ctxt <- getState
       buildExpressionParser (partitionOps ctxt) pappl

pappl :: MyParser thry PreTerm
pappl = 
    try (do prefs <- pprefixes
            if null prefs
               then fail "pappl: no prefix found"
               else do bod <- pappl
                       return $! foldr PComb bod prefs)
    <|> (do tms <- many1 pbinder
            let tm = mkPComb tms
            pas tm <|> return tm)

pbinder :: MyParser thry PreTerm
pbinder = 
    (do myreserved "let"
        tms <- pterm `sepBy1` myreserved "and"
        myreserved "in"
        bod <- pterm
        case mkLet tms bod of
          Nothing -> fail "pterm: invalid let construction"
          Just tm -> return tm)
    <|> do (ctxt, _) <- getState
           bind <- choiceOp $ binders ctxt
           (do vars <- many1 pvar
               myreservedOp "."
               bod <- pterm
               return $! mkBinders bind vars bod)
            <|> (return $! PVar bind dpty)
    <|> do (ctxt, _) <- getState
           bind <- choiceOp $ tyBinders ctxt
           (do vars <- many1 psmall
               myreservedOp "."
               bod <- pterm
               return $! mkTyBinders bind vars bod)
            <|> (return $! PVar bind dpty)
    <|> ptyped
    where 

ptyped :: MyParser thry PreTerm
ptyped = 
    (do myreserved "TYINST"
        vars <- many1 pinst
        tm <- patomic
        return $! PInst vars tm)
    <|> patomic
    where pinst :: MyParser thry (PreType, String)
          pinst = myparens $ do myreservedOp "_"
                                x <- myidentifier
                                myreservedOp ":"
                                ty <- ptype
                                return (ty, x)
                              

pvar :: MyParser thry PreTerm
pvar =
    do tm <- patomic
       pas tm <|> return tm

pas :: PreTerm -> MyParser thry PreTerm
pas ptm =
    do myreservedOp ":"
       ty <- ptype
       return $! PAs ptm ty

patomic :: MyParser thry PreTerm
patomic = 
    myparens (pterm <|> (do x <- myoperator
                            return $! PVar x dpty))
    <|> mybrackets 
         ((do myreservedOp ":"
              ty <- ptype
              return $! PApp ty)
          <|> (do tms <- mysemiSep pterm
                  return (foldr (\ x y -> PVar "CONS" dpty `PComb` 
                                          x `PComb` y)
                              (PVar "NILS" dpty) tms)))
    {- <|> mybraces
          ((do tms <- mycommaSep pterm
               return $! foldr (\ x y -> PComb (PComb (PVar "INSERT" dpty) x) y)
                           (PVar "EMPTY" dpty) tms)
           <|> (do tms <- mypipeSep pterm
                   if length tms == 2
                      then -- setabs
                      else if length tms == 3
                      then -- setcompr))
    -}
    <|> (do myreserved "if"
            c <- pterm
            myreserved "then"
            t <- pterm
            myreserved "else"
            e <- pterm
            return $! PComb (PComb (PComb (PVar "COND" dpty) c) t) e)
    <|> (do x <- myidentifier
            return $! PVar x dpty)

pprefixes :: MyParser thry [PreTerm]
pprefixes =
    do (ctxt, _) <- getState
       pref <- myoperator
       let prefOps = sortBy (\ x y -> compare (length y) (length x)) $ 
                       prefixes ctxt
       return $! splitPref pref prefOps []
    where splitPref :: String -> [String] -> [PreTerm] -> [PreTerm]
          splitPref _ [] acc = acc
          splitPref ops prefs@(p:ps) acc =
              case stripPrefix p ops of
                Nothing -> splitPref ops ps acc
                Just ops' -> 
                    let acc' = acc ++ [PVar p dpty] in
                      if null ops' then acc'
                      else splitPref ops' prefs acc'
                

-- helper functions
mkPComb :: [PreTerm] -> PreTerm
mkPComb (tm:[]) = tm
mkPComb (tm:tms) = foldr (flip PComb) tm (reverse tms)
mkPComb _ = error "parser: mkPComb used without many1 parser combinator"

pdestEq :: PreTerm -> Maybe (PreTerm, PreTerm)
pdestEq (PComb (PComb (PVar "=" _) l) r) = Just (l, r)
pdestEq (PComb (PComb (PVar "<=>" _) l) r) = Just (l, r)
pdestEq _ = Nothing

mkLet :: [PreTerm] -> PreTerm -> Maybe PreTerm
mkLet binds bod = case length tms of
                    0 -> Nothing
                    _ -> Just $ foldl PComb letstart tms
    where (vars, tms) = unzip $ mapMaybe pdestEq binds
          letend = PComb (PVar "LET_END" dpty) bod
          ab = foldr PAbs letend vars
          letstart = PComb (PVar "LET" dpty) ab

mkBinder :: String -> PreTerm -> PreTerm -> PreTerm
mkBinder "\\" v bod = PAbs v bod
mkBinder n v bod = PComb (PVar n dpty) $ PAbs v bod

mkBinders :: String -> [PreTerm] -> PreTerm -> PreTerm
mkBinders bind vars bod = foldr (mkBinder bind) bod vars

mkTyBinder :: String -> PreType -> PreTerm -> PreTerm
mkTyBinder "\\\\" v bod = TyPAbs v bod
mkTyBinder n v bod = PComb (PVar n dpty) $ TyPAbs v bod

mkTyBinders :: String -> [PreType] -> PreTerm -> PreTerm
mkTyBinders bind vars bod = foldr (mkTyBinder bind) bod vars

-- build op table for expression parser from context
-- Note: prefix operators are handled separately in pprefixes
partitionOps :: (HOLContext thry, [(String, Int)]) ->
                OperatorTable Char (HOLContext thry, [(String, Int)]) PreTerm
partitionOps (ctxt, _) = map (map mkOp) . 
                         group' (\ (_, (x, _)) (_, (y, _)) -> x == y) $ 
                         infixes ctxt
  where mkOp :: (String, (Int, Assoc)) -> 
                Operator Char (HOLContext thry, [(String, Int)]) PreTerm
        mkOp (name, (_, a)) = 
            Infix (do myreservedOp name
                      return (\ x y -> PComb (PComb (PVar name dpty) x) y)) a
