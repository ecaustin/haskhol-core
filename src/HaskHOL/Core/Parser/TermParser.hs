{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
                      | match PRETERM with CLAUSES
                      | function CLAUSES            
                      | identifier    

  CLAUSES            :: PATTERN -> PRETERM | .. | PATTERN -> PRETERM

  PATTERN            :: PRETERM when PRETERM
                      | PRETERM         
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

import HaskHOL.Core.Lib
import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.State (getConstType)

import Control.Monad.Trans (lift)

-- | Parser for HOL terms.
pterm :: MyParser cls thry PreTerm
pterm = 
    (do mywhiteSpace
        expressionParser ptyped)
    <|> (do s <- myidentifier <|> myoperator
            return $! PVar s dpty)

ptyped :: MyParser cls thry PreTerm
ptyped = pas =<< pappl

pappl :: MyParser cls thry PreTerm
pappl = 
    (do p <- pprefix
        tm <- pappl
        return $! PComb (PVar p dpty) tm)
    <|> do (tm:tms) <- mymany1 pbinder
           return $! foldr (flip PComb) tm (reverse tms)

pprefix :: MyParser cls thry Text
pprefix = choiceId =<< lift prefixes

pbinder :: MyParser cls thry PreTerm
pbinder = 
    (do myreserved "let"
        tms <- pterm `mysepBy1` myreserved "and"
        myreserved "in"
        bod <- pterm
        case mkLet tms bod of
          Nothing -> fail "pterm: invalid let construction"
          Just tm -> return tm)
    <|> (do bind <- choiceId =<< lift binders
            (do vars <- mymany1 pvar
                myreservedOp "."
                bod <- pterm
                return $! mkBinders bind vars bod)
             <|> (return $! PVar bind dpty))
    <|> (do bind <- choiceId =<< lift tyBinders
            (do vars <- mymany1 psmall
                myreservedOp "."
                bod <- pterm
                return $! mkTyBinders bind vars bod)
             <|> (return $! PVar bind dpty))
    <|> pinst
    where 

pinst :: MyParser cls thry PreTerm
pinst = 
    (do myreserved "TYINST"
        vars <- mymany1 pinst'
        tm <- patomic
        return $! PInst vars tm)
    <|> patomic
    where pinst' :: MyParser cls thry (PreType, Text)
          pinst' = myparens $ do myreservedOp "_"
                                 x <- myidentifier
                                 myreservedOp ":"
                                 ty <- ptype
                                 return (ty, x)
                              
pvar :: MyParser cls thry PreTerm
pvar = pas =<< patomic

pas :: PreTerm -> MyParser cls thry PreTerm
pas tm =
    (do myreservedOp ":"
        ty <- ptype
        return $! PAs tm ty) <|> return tm

patomic :: MyParser cls thry PreTerm
patomic =
    myparens ((do myreservedOp ":"
                  ty <- ptype
                  return $! PAs (PVar "UNIV" dpty) 
                                (PTyComb (PTyCon "fun") 
                                 [ty, PTyComb (PTyCon "bool") []]))
              <|> mytry pterm
              <|> (do s <- myidentifier <|> myoperator
                      return (PVar s dpty)))
    <|> (do myreserved "if"
            c <- pterm
            myreserved "then"
            t <- pterm
            myreserved "else"
            e <- pterm
            return $! PComb (PComb (PComb (PVar "COND" dpty) c) t) e)
    <|> mybrackets 
         ((do myreservedOp ":"
              ty <- ptype
              return $! PApp ty)
          <|> (do tms <- mysemiSep pterm
                  return (foldr (\ x y -> PVar "CONS" dpty `PComb` 
                                          x `PComb` y)
                              (PVar "NIL" dpty) tms)))
{-
    <|> mybraces
         ((do tms <- mycommaSep pterm
              return $! foldr (\ x y -> PComb (PComb (PVar "INSERT" dpty) x) y)
                          (PVar "EMPTY" dpty) tms)
          <|> (do tms <- pterm `sepBy1` myreservedOp "|"
                  case tms of
                    (l:r:[]) -> pmkSetAbs l r
                    (f:v:b:[]) -> pmkSetCompr f (pfrees vs []) b
                    _ -> fail "patomic: bad set construction."))
-}
    <|> (do myreserved "match"
            e <- pterm
            myreserved "with"
            c <- pclauses
            return $! PComb (PComb (PVar "_MATCH" dpty) e) c)
    <|> (do myreserved "function"
            c <- pclauses
            return $! PComb (PVar "_FUNCTION" dpty) c)  
    <|> mytry (do x <- myidentifier <|> myoperator
                  cond1 <- lift $ parsesAsPrefix x
                  cond2 <- lift $ parsesAsInfix x
                  cond3 <- lift $ parsesAsBinder x
                  cond4 <- lift $ parsesAsTyBinder x
                  if not (cond1 || cond2 || cond3 || cond4)
                     then return $! PVar x dpty
                     else fail "patomic")

pclauses :: MyParser cls thry PreTerm
pclauses =
    do c <- pclause `mysepBy1` myreservedOp "|"
       return $! foldr1 (\ s t -> PComb (PComb (PVar "_SEQPATTERN" dpty) s) t) c
  where pclause :: MyParser cls thry PreTerm
        pclause = do (pat:guards) <- pterm `mysepBy1` myreserved "when"
                     myreservedOp "->"
                     res <- pterm
                     mkPattern pat guards res

-- helper functions
pgenVar :: MyParser cls thry PreTerm
pgenVar = 
    do (ops, n) <- getState
       setState (ops, succ n)
       return $! PVar (pack $ "_GENPVAR_" ++ show n) dpty

pfrees :: PreTerm -> [PreTerm] -> MyParser cls thry [PreTerm]
pfrees ptm@(PVar v pty) acc
    | textNull v && pty == dpty = return acc
    | otherwise = 
          do cond1 <- lift $ can getConstType v
             let cond2 = isJust (numOfString (unpack v) :: Maybe Integer)
             cond3 <- lift $ liftM (isJust . lookup v) getInterface
             if cond1 || cond2 || cond3
                then return acc
                else return $! ptm `insert` acc
pfrees PConst{} acc = return acc
pfrees (PComb p1 p2) acc = pfrees p1 =<< pfrees p2 acc
pfrees (PAbs p1 p2) acc =
    do p1s <- pfrees p1 []
       p2s <- pfrees p2 acc
       return $! p2s \\ p1s
pfrees (PAs p _) acc = pfrees p acc
pfrees (PInst _ p) acc = pfrees p acc
pfrees PApp{} acc = return acc
pfrees (TyPAbs _ p) acc = pfrees p acc
pfrees (TyPComb p _ _) acc = pfrees p acc


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

mkBinder :: Text -> PreTerm -> PreTerm -> PreTerm
mkBinder "\\" v bod = PAbs v bod
mkBinder n v bod = PComb (PVar n dpty) $ PAbs v bod

mkBinders :: Text -> [PreTerm] -> PreTerm -> PreTerm
mkBinders bind vars bod = foldr (mkBinder bind) bod vars

mkTyBinder :: Text -> PreType -> PreTerm -> PreTerm
mkTyBinder "\\\\" v bod = TyPAbs v bod
mkTyBinder n v bod = PComb (PVar n dpty) $ TyPAbs v bod

mkTyBinders :: Text -> [PreType] -> PreTerm -> PreTerm
mkTyBinders bind vars bod = foldr (mkTyBinder bind) bod vars

mkPattern :: PreTerm -> [PreTerm] -> PreTerm -> MyParser cls thry PreTerm
mkPattern pat guards res =
    do x <- pgenVar
       y <- pgenVar
       vs <- pfrees pat []
       let bod = if null guards 
                 then PComb (PComb (PVar "_UNGUARDED_PATTERN" dpty) $ 
                               mkGEQ pat x) $ mkGEQ res y
                 else PComb (PComb (PComb (PVar "_GUARDED_PATTERN" dpty) $
                                      mkGEQ pat x) $ head guards) $ mkGEQ res y
       return . PAbs x . PAbs y $ foldr mkExists bod vs
  where mkGEQ :: PreTerm -> PreTerm -> PreTerm
        mkGEQ l = PComb (PComb (PVar "GEQ" dpty) l)

        mkExists :: PreTerm -> PreTerm -> PreTerm
        mkExists v ptm = PComb (PVar "?" dpty) $ PAbs v ptm
    

-- build expression parser from infix operators in context
expressionParser :: forall cls thry. MyParser cls thry PreTerm 
                 -> MyParser cls thry PreTerm
expressionParser prs = expressionParser' =<< lift infixes
    where expressionParser' :: [(Text, (Int, Text))] 
                            -> MyParser cls thry PreTerm
          expressionParser' [] = prs
          expressionParser' infxs@((_, (p, at)):_) =
              let (topins, rest) = partition (\ (_, pat') -> 
                                              pat' == (p, at)) infxs
                  parse' = if at == "right" then pRightBinary 
                           else pLeftBinary in
                parse' (expressionParser' rest)
                       (choiceId (map fst topins))
                       (\ op x y -> PComb (PComb (PVar op dpty) x) y)

sepPair :: MyParser cls thry Text -> MyParser cls thry PreTerm 
        -> MyParser cls thry [(Text, PreTerm)]
sepPair sep prs =
    mymany (do l <- sep
               r <- prs
               return (l, r))

pRightBinary :: MyParser cls thry PreTerm -> MyParser cls thry Text
             -> (Text -> PreTerm -> PreTerm -> PreTerm) 
             -> MyParser cls thry PreTerm
pRightBinary prs sep cns =
  do x <- prs
     opxs <- sepPair sep prs
     if null opxs
        then return x
        else let (ops, xs) = unzip opxs in
               case foldr2 cns (last xs) ops (x:init xs) of
                 Just res -> return res
                 _ -> fail "pRightBinary"

pLeftBinary :: MyParser cls thry PreTerm -> MyParser cls thry Text
            -> (Text -> PreTerm -> PreTerm -> PreTerm) 
            -> MyParser cls thry PreTerm
pLeftBinary prs sep cns =
  do x <- prs
     opxs <- sepPair sep prs
     let (ops, xs) = unzip opxs in
       case foldr2 (\ op l r -> cns op r l) x (reverse ops) (reverse xs) of
         Just res -> return res
         _ -> fail "pLeftBinary"
