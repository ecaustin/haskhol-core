{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-|
  Module:    HaskHOL.Core.Printer
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines pretty printers for 'HOLType's, 'HOLTerm's and 'HOLThm's. 
  Note that the printers for terms and theorems are context dependent as they 
  rely on the same theory extensions that the parsers utilize. 

  To make printing these objects easier within HOL computations, this module
  also defines the 'showHOL' and 'printHOL' methods which will automatically
  retrieve the current working theory to use for pretty printing.  Because the 
  pretty printer for 'HOLType's is not context dependent it has definitions for 
  both 'show' and 'showHOL'.

  Note that, like the parser, there are a number of HOL term forms that the
  printer does not currently support.  Again, these are mainly related to sets
  and patterns and will be added in when the HaskHOL system has libraries for
  them.
-}
module HaskHOL.Core.Printer
    ( -- * Pretty Printers
      ppType
    , ppTerm
    , ppThm
      -- * Printing in the 'HOL' Monad
    , ShowHOL(..)
    , printHOL
    ) where

import Prelude hiding ((<$>))
import HaskHOL.Core.Lib hiding (ask, empty, lefts, rights, base)
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics
import HaskHOL.Core.Parser hiding (getInterface)

import HaskHOL.Core.Printer.Prims

import Control.Lens hiding (Const, op, cons, snoc)
import Control.Monad.ST
import Data.STRef
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint


data PrintError
    = PrintError String

type PrintM s = ReaderT (STRef s PrintState) (CatchT (ST s))

data PrintState = PrintState
    { _prec :: !Integer
    , _printCtxt :: !PrintContext
    }

makeLenses ''PrintState

initPrintState :: PrintContext -> PrintState
initPrintState = PrintState 0

modPrintState :: (PrintState -> PrintState) -> PrintM s ()
modPrintState f = lift $
    do ref <- ask
       liftM $ modifySTRef' ref f

viewPrintState :: (PrintState -> a) -> PrintM s a
viewPrintState f = lift $
    do ref <- ask
       lift . liftM f $ readSTRef ref

-- utility functions
getPrec :: PrintM s Integer
getPrec = viewPrintState $ view prec

setPrec :: Integer -> PrintM s ()
setPrec = modPrintState . set prec

getInterface :: PrintM s [(Text, (Text, HOLType))]
getInterface = viewPrintState $ view (printCtxt . interface)

-- | Pretty printer for 'HOLType's.
ppType :: HOLType -> PrintM s TermDoc
ppType (TyVar False x) = pretty x
ppType (TyVar True x) = pretty $ '\'' `cons` x
ppType ty =
    case destUTypes ty of
      Just (tvs, bod) -> 
          do tvs' <- mapM ppType tvs
             bod' <- ppType bod
             return $! parens (char '%' <+> hsep tvs' <+> char '.' <+> bod')
      Nothing ->    
          do prec <- getPrec
             (op, tys) <- destType ty 
             let (name, ar) = destTypeOp op
                 name' = if ar < 0 then '_' `cons` name else name
             if null tys 
                then return $! pretty op
                else case (name', tys) of
                       ("fun", [ty1,ty2]) ->
                           do ty1' <- setPrec 1 >> ppType ty1
                              ty2' <- setPrec 0 >> ppType ty2
                              return $! ppTypeApp "->" (prec > 0) [ty1', ty2']
                       ("sum", [ty1,ty2]) -> 
                           do ty1' <- setPrec 3 >> ppType ty1
                              ty2' <- setPrec 2 >> ppType ty2
                              return $! ppTypeApp "+" (prec > 2) [ty1', ty2']
                       ("prod", [ty1,ty2]) -> 
                           do ty1' <- setPrec 5 >> ppType ty1
                              ty2' <- setPrec 4 >> ppType ty2
                              return $! ppTypeApp "#" (prec > 4) [ty1', ty2']
                       ("cart", [ty1,ty2]) -> 
                           do ty1' <- setPrec 6 >> ppType ty1
                              ty2' <- setPrec 7 >> ppType ty2
                              return $! ppTypeApp "^" (prec > 6) [ty1', ty2']
                       (bin, args) -> 
                           do args' <- mapM (setPrec 0 >> ppType) args
                              return $! ppTypeApp "," True args' <+> pretty bin
  where ppTypeApp :: String -> Bool -> [TermDoc] -> TermDoc
        ppTypeApp sepr flag ds =
            case tryFoldr1 (\ x y -> x <+> text sepr <+> y) ds of
              Nothing -> empty
              Just bod -> if flag then parens bod else bod


-- Printer for Terms
-- | Pretty printer for 'HOLTerm's.
ppTerm :: HOLTerm -> PrintM s TermDoc
ppTerm tm =
-- numeral case
    (pretty #<< destNumeral tm) <|>
-- List case
    brackets (ppTermSeq ";" 0 #<< destList tm) <|>
-- Type combination case
    (ppTyComb prec #<< destTyComb tm) <|>
-- Let case
    (ppLet prec #<< destLet tm) <|>
-- General abstraction case -- needs work
    if isGAbs tm then ppGAbs prec tm
    else let (hop, args) = stripComb tm in
-- Base term abstraction case
    if isAbs hop && null args then ppBinder prec "\\" False hop
-- Base type abstraction case
    else if isTyAbs hop && null args then ppBinder prec "\\\\" True hop
-- Reverse interface for other cases
    else let s0 = nameOf hop
             ty0 = typeOf hop in
    do s <- reverseInterface s0 ty0
-- Match terms
       if s == "_MATCH" && length args == 2 && 
          --not ideal from a performance aspect, but it makes things cleaner 
          isJust (destClauses $ args !! 1)
          then let (m:cs:_) = args in ppMatch prec m cs                       
-- Conditional case
          else if s == "COND" && length args == 3
          then let (c:t:e:_) = args in ppCond prec c t e                    
-- Prefix operator case           
          else do cond1 <- parsesAsPrefix s
                  if cond1 && length args == 1 
                     then ppPrefix prec s (head args)
-- Non-lambda term and type binder case
                     else ppBinders prec s hop args tm <|> 
                          (ppComb prec #<< destComb tm) <|>
                          text "ppTerm: printer error - unrecognized term"  
  where ppTermSeq :: Text -> Int -> [HOLTerm] -> TermDoc
        ppTermSeq sepr prec' = ppTermSeqRec
          where ppTermSeqRec [] = empty
                ppTermSeqRec [x] = ppTerm prec' x
                ppTermSeqRec (x:xs) =
                  ppTerm prec' x <+> text sepr <+> ppTermSeqRec xs     

ppBinders :: Int -> Text -> HOLTerm -> [HOLTerm] -> HOLTerm -> TermDoc
ppBinders prec s hop args tm =
     do cond2 <- parsesAsBinder s
        if cond2 && length args == 1 && isGAbs (head args)
        then ppBinder prec s False tm
-- Non-lambda type binder case
        else do cond3 <- parsesAsTyBinder s
                if cond3 && length args == 1 && isTyAbs (head args)
                then ppBinder prec s True tm
-- Infix operator case
                else ppOperators prec s hop args tm

ppOperators :: Int -> Text -> HOLTerm -> [HOLTerm] -> HOLTerm -> TermDoc
ppOperators prec s hop args tm =
    do getRight <- liftM (lookup s) rights
       getLeft <- liftM (lookup s) lefts
       if (isJust getRight || isJust getLeft) && length args == 2
          then do (barg:bargs) <- 
                     if isJust getRight
                     then do (tms, tmt) <- splitListM (destBinaryTm hop) tm
                             return $! tms ++ [tmt]
                     else do (tmt, tms) <- revSplitListM (destBinaryTm hop) tm
                             return $! tmt:tms 
                  let newprec = fromMaybe 0 (getRight <|> getLeft)
                      wrapper = if newprec <= prec then parens else id
                      sepr x y = 
                          do ops <- getUnspacedBinops
                             if s `elem` ops
                                then cat $ sequence [x, y] 
                                else sep $ sequence [x, y]
                      hanger x y = 
                          do ops <- getPrebrokenBinops
                             if s `elem` ops
                             then x `sepr` (text s <+> y)
                             else (x <+> text s) `sepr` y
                  wrapper $ foldr (\ x acc -> acc `hanger` 
                                              ppTerm newprec x)
                              (ppTerm newprec barg) $ reverse bargs
-- Base constant or variable case
          else ppConstants s hop args
  where destBinaryTm :: HOLTerm -> HOLTerm -> (HOLTerm, HOLTerm)
        destBinaryTm c t =
            do (il, r) <- liftO $ destComb t
               (i, l) <- liftO $ destComb il
               if i == c
                  then do i' <- liftO $ destConst i <|> destVar i
                          c' <- liftO $ destConst c <|> destVar c
                          i'' <- uncurry reverseInterface i'
                          c'' <- uncurry reverseInterface c'
                          if i'' == c''
                             then return (l, r)
                             else fail "destBinaryTm"
                  else fail "destBinaryTm"

ppConstants :: Text -> HOLTerm -> [HOLTerm] -> TermDoc
ppConstants s hop args
    | null args && (isConst hop || isVar hop) =
          do cond1 <- parsesAsBinder s
             cond2 <- parsesAsTyBinder s
             cond3 <- liftM (isJust . lookup s) rights
             cond4 <- liftM (isJust . lookup s) lefts
             cond5 <- parsesAsPrefix s
             if cond1 || cond2 || cond3 || cond4 || cond5
                then parens $ text s
                else text s
-- Base combination case 
    | otherwise = fail "ppConstants: fall back to ppComb case."          

nameOf :: HOLTerm -> Text
nameOf (Var x _) = x
nameOf (Const x _) = x
nameOf _ = textEmpty

reverseInterface :: Text -> HOLType -> PrintM s Text
reverseInterface s0 ty0 =
    do iface <- getInterface
       let s1 = find (\ (_, (s', ty)) -> 
                      s' == s0 && 
                      isJust (typeMatch ty ty0 ([], [], []))) iface
       return $! maybe s0 fst s1

grabInfix :: Text -> [(Text, (Int, Text))] -> [(Text, Int)]
grabInfix a = 
    mapMaybe $ \ (x, (n, a')) -> if a == a' then Just (x, n) else Nothing

lefts :: HOL cls thry [(Text, Int)]
lefts = liftM (grabInfix "left") infixes

rights :: HOL cls thry [(Text, Int)]
rights = liftM (grabInfix "right") infixes

ppTyComb :: Int -> (HOLTerm, HOLType) -> TermDoc
ppTyComb prec (t, ty) =
    let base = ppTerm 999 t <+> 
               brackets (char ':' <> ppType ty) in
      if prec == 1000 then parens base else base

ppLet :: ([(HOLTerm, HOLTerm)], HOLTerm) -> PrintM s TermDoc
ppLet (eqs@(_:_), bod) =
    do prec <- getPrec
       eqs' <- mapM ppLet' eqs
       bod' <- setPrec 0 >> ppTerm bod
       let base = (text "let" <+> sepBy (text "and") eqs' <+> text "in") `above`
                  indext 2 bod'
       return $! if prec == 0 then base else parens base 
  where ppLet' :: (HOLTerm, HOLTerm) -> PrintM s TermDoc
        ppLet' x =
            (do x' <- uncurry primMkEq x
                setPrec 0 >>= ppTerm x') `catchM` 
            (\ _ -> return $! text "<*bad let binding*>")
ppLet (_, bod) = ppTerm bod

ppGAbs :: HOLTerm -> PrintM s TermDoc
ppGAbs tm =
    let (vs, bod) = stripGAbs tm in
      do prec <- getPrec
         vs' <- mapM (\ x -> setPrec 999 >> ppTerm x) vs
         bod' <- setPrec 0 >> ppTerm bod
         let base = char '\\' <+> sep vs' <+> char '.' <+> bod'
         return $! if prec == 0 then base else parens base

ppBinder :: Text -> Bool -> HOLTerm -> PrintM s TermDoc
ppBinder prep f tm =
    let (vs, bod) = strip f ([], tm)
        bvs = pretty prep <> 
              foldr (\ x acc -> acc <+> pretty x) empty vs <> 
              char '.' in
      do prec <- getPrec
         bod' <- ppTerm bod
         let base = let ident = min (1 + length (show bvs)) 5 in
                      bvs <> nest ident bod'
         return $! if prec == 0 then base else parens base
  where strip :: Bool -> ([Text], HOLTerm) -> ([Text], HOLTerm)
        strip False pat@(acc, Comb (Var s _) (Abs (Var bv _) bod))
            | s == prep = strip False (bv:acc, bod)
            | otherwise = pat
        strip False pat@(acc, Comb (Const s _) (Abs (Var bv _) bod))
            | s == prep = strip False (bv:acc, bod)
            | otherwise = pat
        strip True pat@(acc, Comb (Var s _) (TyAbs (TyVar _ bv) bod))
            | s == prep = strip True (('\'' `cons` bv):acc, bod)
            | otherwise = pat
        strip True pat@(acc, Comb (Const s _) (TyAbs (TyVar _ bv) bod))
            | s == prep = strip True (('\'' `cons` bv):acc, bod)
            | otherwise = pat
        strip False (acc, Abs (Var bv _) bod) = 
            strip False (bv:acc, bod)
        strip True (acc, TyAbs (TyVar _ bv) bod) =
            strip True (('\'' `cons` bv):acc, bod)
        strip _ pat = pat

ppMatch :: HOLTerm -> HOLTerm -> PrintM s TermDoc
ppMatch m cls =
    do prec <- getPrec
       m' <- setPrec 0 >> ppTerm m
       cls' <- ppClauses =<< destClauses cls
       let base = text "match" <+> m' <+> text "with" <+> cls'
       return $! if prec == 0 then base else parens base
  where ppClauses :: [[HOLTerm]] -> PrintM s TermDoc
        ppClauses [c] = ppClause c
        ppClauses (c:cs) = 
            do c' <- ppClause c 
               cs' <- ppClauses cs
               return $! c' <+> char '|' <+> cs'
        ppClauses _ = return empty

        ppClause :: [HOLTerm] -> PrintM s TermDoc
        ppClause [p, r] = 
            do p' <- setPrec 1 >> ppTerm p
               r' <- setPrec 1 >> ppTerm r
               return $! p' <+> text "->" <+> r'
        ppClause [p, g, r] =
            do p' <- setPrec 1 >> ppTerm p
               g' <- setPrec 1 >> ppTerm g
               r' <- setPrec 1 >> ppTerm r
               return $! p' <+> "when" <+> g' <+> "->" <+> r'
        ppClause _ = return empty

destClauses :: MonadThrow m => HOLTerm -> m [[HOLTerm]]
destClauses tm =
    let (s, args) = stripComb tm in
      if nameOf s == "_SEQPATTERN" && length args == 2
      then do c <- destClause (head args)
              cs <- destClauses (args !! 1)
              return (c:cs)
      else do c <- destClause tm
              return [c]
  where destClause :: MonadThrow m => HOLTerm -> m [HOLTerm]
        destClause tm' =
            do (_, pbod) <- liftM stripExists $ body =<< body tm'
               let (s, args) = stripComb pbod
               if nameOf s == "_UNGUARDED_PATTERN" && length args == 2
                  then do tm'1 <- rand =<< rator (head args)
                          tm'2 <- rand =<< rator (args !! 1)
                          return [tm'1, tm'2]
                  else if nameOf s == "_GUARDED_PATTERN" && 
                          length args == 3
                       then do tm'1 <- rand =<< rator (head args)
                               let tm'2 = head $ tail args
                               tm'3 <- rand =<< rator (args !! 2)
                               return [tm'1, tm'2, tm'3]
                       else throwM $! HOLTermError "destClause"

ppCond :: HOLTerm -> HOLTerm -> HOLTerm -> PrintM s TermDoc
ppCond c t e =
    do prec <- getPrec
       c' <- setPrec 0 >> ppTerm c
       t' <- setPrec 0 >> ppTerm t
       e' <- setPrec 0 >> ppTerm e
       let base = text "if" <+> c' <+> text "then" <+> t' <+> text "else" <+> e'
       return $! if prec == 0 then base else parens base

ppPrefix :: Text -> HOLTerm -> PrintM s TermDoc
ppPrefix s arg =
    do prec <- getPrec
       arg' <- setPrec 999 >> ppTerm arg
       let base = pretty s <+> arg'
       return $! if prec == 1000 then parens base else base

ppComb :: (HOLTerm, HOLTerm) -> PrintM s TermDoc
ppComb (l, r) =
    do prec <- getPrec
       l' <- setPrec 999 >> ppTerm l
       r' <- setPrec 1000 >> ppTerm r
       let base = l' <+> r'
       return $! if prec == 1000 then parens base else base

-- Printer for Theorems

-- | Pretty printer for 'HOLThm's.	
ppThm :: HOLThm -> PrintM s TermDoc
ppThm (Thm [] c) = 
    do c' <- setPrec 0 >> ppTerm c
       return $! text "|-" <+> c'
ppThm (Thm asl c) =
    do c' <- setPrec 0 >> ppTerm c
       asl' <- mapM (\ x -> setPrec 0 >> ppTerm x) asl
       return $! encloseSep empty empty comma asl' <+> text "|-" <+> c'
ppThm _ = throwM $! HOLExhaustiveWarning "ppThm"

{-| 
  The @ShowHOL@ class is functionally equivalent to 'show' lifted to the 'HOL'
  monad.  It is used to retrieve the current working theory to be used with the
  context sensitive pretty printers for 'HOLTerm's and 'HOLType's.
-}
class ShowHOL a where
    {-| 
      A version of 'show' lifted to the 'HOL' monad for context sensitive pretty
      printers.
    -}
    showHOL :: a -> PrintM s TermDoc

    showHOLList :: [a] -> PrintM s TermDoc
    showHOLList = showHOLList' brackets comma <=< mapM showHOL

instance ShowHOL a => ShowHOL [a] where
    showHOL = showHOLList
    
instance (ShowHOL a, ShowHOL b) => ShowHOL (a, b) where
    showHOL (a, b) = showHOLList' parens comma =<< sequence
                       [showHOL a, showHOL b]

instance (ShowHOL a, ShowHOL b, ShowHOL c) => ShowHOL (a, b, c) where
    showHOL (a, b, c) = showHOLList' parens comma =<< sequence
                          [showHOL a, showHOL b, showHOL c]

instance (ShowHOL a, ShowHOL b,ShowHOL c, ShowHOL d) => 
         ShowHOL (a, b, c, d) where
    showHOL (a, b, c, d) = showHOLList' parens comma =<< sequence
                             [ showHOL a, showHOL b
                             , showHOL c, showHOL d ]

-- Prints a list of strings provided a wrapper function and seperator document.
showHOLList' :: (TermDoc -> TermDoc) -> TermDoc -> [TermDoc] -> PrintM s TermDoc
showHOLList' wrap sepr =
    return . wrap . sep . showHOLListRec sepr
  
-- Useful to have at top level for ppThm.
showHOLListRec :: TermDoc -> [TermDoc] -> [TermDoc]
showHOLListRec _ [] = [empty]
showHOLListRec _ [x] = [x]
showHOLListRec sepr (x:xs) = (x <> sepr <> space) : showHOLListRec sepr xs

-- orphan instances
instance ShowHOL HOLType where
    showHOL = liftM (char ':' <>) . ppType

instance ShowHOL HOLTerm where
    showHOL = ppTerm

instance ShowHOL HOLThm where
    showHOL = ppThm

{-| 
  Prints a HOL object with a new line.  A composition of 'putStrLnHOL' and
  'showHOL'.
-}
printHOL :: ShowHOL a => a -> HOL cls thry ()
printHOL x = 
    do ctxt <- printContext
       either (fail . show) putDocHOL $ runST $
         do ref <- newSTRef $ initPrintState ctxt
            runCatchT $ runReaderT (showHOL x) ref
