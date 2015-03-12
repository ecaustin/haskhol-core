{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, 
             TypeFamilies #-}
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

import HaskHOL.Core.Lib hiding ((<$>), empty, lefts, rights, base)
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics
import HaskHOL.Core.Parser

{- 
  Used for a number of pretty-printing primitives that don't really need to be
  exposed to the rest of the system.  Although no harm would come should we
  elect to move this to be re-exported by Core.Lib.
-}
import Text.PrettyPrint.Leijen.Text.Monadic

-- | Pretty printer for 'HOLType's.
ppType :: HOLType -> HOL cls thry Doc
ppType = ppTypeRec 0
  where ppTypeRec :: Int -> HOLType -> HOL cls thry Doc
        ppTypeRec _ (TyVar False x) = text x
        ppTypeRec _ (TyVar True x) = text $ '\'' `cons` x
        ppTypeRec prec ty =
            case destUTypes ty of
              Just (tvs, bod) -> 
                  let tvs' = foldr (\ x acc -> ppTypeRec prec x <+> acc) 
                               empty tvs in
                    parens $ char '%' <+> tvs' <+> char '.' <+> 
                               ppTypeRec prec bod
              Nothing ->    
                  case do (op, tys) <- destType ty 
                          let (name, ar) = destTypeOp op
                              name' = if ar < 0 then '_' `cons` name else name
                          return (name', tys) of
                    Just (op, []) -> text op
                    Just ("fun", [ty1,ty2]) ->
                        ppTypeApp "->" (prec > 0) [ ppTypeRec 1 ty1
                                                  , ppTypeRec 0 ty2]
                    Just ("sum", [ty1,ty2]) -> 
                        ppTypeApp "+" (prec > 2) [ ppTypeRec 3 ty1
                                                 , ppTypeRec 2 ty2]
                    Just ("prod", [ty1,ty2]) -> 
                        ppTypeApp "#" (prec > 4) [ ppTypeRec 5 ty1
                                                 , ppTypeRec 4 ty2]
                    Just ("cart", [ty1,ty2]) -> 
                        ppTypeApp "^" (prec > 6) [ ppTypeRec 6 ty1
                                                 , ppTypeRec 7 ty2]
                    Just (bin, args) -> 
                        ppTypeApp "," True (map (ppTypeRec 0) args) <+> text bin
                    _ -> text "ppType: printer error - unrecognized type"
  
        ppTypeApp :: Text -> Bool -> [HOL cls thry Doc] -> HOL cls thry Doc
        ppTypeApp sepr flag ds =
            case tryFoldr1 (\ x y -> x <+> text sepr <+> y) ds of
              Nothing -> empty
              Just bod -> if flag then parens bod else bod

-- Printer for Terms
-- | Pretty printer for 'HOLTerm's.
ppTerm :: Int -> HOLTerm -> HOL cls thry Doc
ppTerm prec tm =
-- numeral case
    (integer #<< destNumeral tm) <|>
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
  where ppTermSeq :: Text -> Int -> [HOLTerm] -> HOL cls thry Doc
        ppTermSeq sepr prec' = ppTermSeqRec
          where ppTermSeqRec [] = empty
                ppTermSeqRec [x] = ppTerm prec' x
                ppTermSeqRec (x:xs) =
                  ppTerm prec' x <+> text sepr <+> ppTermSeqRec xs           

ppBinders :: Int -> Text -> HOLTerm -> [HOLTerm] -> HOLTerm 
          -> HOL cls thry Doc
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

ppOperators :: Int -> Text -> HOLTerm -> [HOLTerm] -> HOLTerm 
            -> HOL cls thry Doc
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
  where destBinaryTm :: HOLTerm -> HOLTerm -> HOL cls thry (HOLTerm, HOLTerm)
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

ppConstants :: Text -> HOLTerm -> [HOLTerm] -> HOL cls thry Doc
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

reverseInterface :: Text -> HOLType -> HOL cls thry Text
reverseInterface s0 ty0 =
    do fl <- getBenignFlag FlagRevInterface
       if not fl
          then return s0
          else do iface <- getInterface
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

ppTyComb :: Int -> (HOLTerm, HOLType) -> HOL cls thry Doc
ppTyComb prec (t, ty) =
    let base = ppTerm 999 t <+> 
               brackets (char ':' <> ppType ty) in
      if prec == 1000 then parens base else base

ppLet :: Int -> ([(HOLTerm, HOLTerm)], HOLTerm) -> HOL cls thry Doc
ppLet prec (eq:eqs, bod) =
    let base = (text "let" <+> 
                foldr (\ eq' acc -> acc <+> text "and" <+> 
                                    ppLet' eq') (ppLet' eq) eqs <+>
                text "in") <$> indent 2 (ppTerm 0 bod) in
      if prec == 0 then base else parens base  
  where ppLet' :: (HOLTerm, HOLTerm) -> HOL cls thry Doc
        ppLet' x =
            case uncurry primMkEq x of
              Just x' -> ppTerm 0 x'
              _ -> text "<*bad let binding*>"
ppLet prec (_, bod) = ppTerm prec bod

ppGAbs :: Int -> HOLTerm -> HOL cls thry Doc
ppGAbs prec tm =
    let (vs, bod) = stripGAbs tm
        base = char '\\' <+> sep (mapM (ppTerm 999) vs) <+>
               char '.' <+> ppTerm 0 bod in
      if prec == 0 then base else parens base

ppBinder :: Int -> Text -> Bool -> HOLTerm -> HOL cls thry Doc
ppBinder prec prep f tm =
    let (vs, bod) = strip f ([], tm) in
      do bvs <- text prep <> 
                foldr (\ x acc -> acc <+> text x) empty vs <> 
                char '.'
         let base = let ident = min (1 + length (show bvs)) 5 in
                      cat $ sequence [ return bvs
                                     , nest ident $ ppTerm prec bod
                                     ]
         if prec == 0 
            then base 
            else parens base
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

ppMatch :: Int -> HOLTerm -> HOLTerm -> HOL cls thry Doc
ppMatch prec m cls =
    let base = text "match" <+> ppTerm 0 m <+>
               text "with" <+> 
               ppClauses (fromJust $ destClauses cls) in
      if prec == 0 then base else parens base
  where ppClauses :: [[HOLTerm]] -> HOL cls thry Doc
        ppClauses [c] = ppClause c
        ppClauses (c:cs) = ppClause c <+> char '|' <+> ppClauses cs
        ppClauses _ = empty

        ppClause :: [HOLTerm] -> HOL cls thry Doc
        ppClause [p, r] = 
            ppTerm 1 p <+> text "->" <+> ppTerm 1 r
        ppClause [p, g, r] =
            ppTerm 1 p <+> text "when" <+> ppTerm 1 g <+>
            text "->" <+> ppTerm 1 r
        ppClause _ = empty

destClauses :: HOLTerm -> Maybe [[HOLTerm]]
destClauses tm =
    let (s, args) = stripComb tm in
      if nameOf s == "_SEQPATTERN" && length args == 2
      then do c <- destClause (head args)
              cs <- destClauses (args !! 1)
              return (c:cs)
      else do c <- destClause tm
              return [c]
  where destClause :: HOLTerm -> Maybe [HOLTerm]
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
                       else Nothing

ppCond :: Int -> HOLTerm -> HOLTerm -> HOLTerm -> HOL cls thry Doc
ppCond prec c t e =
    let base = text "if" <+> ppTerm 0 c <+> 
               text "then" <+> ppTerm 0 t <+> 
               text "else" <+> ppTerm 0 e in
      if prec == 0 then base else parens base

ppPrefix :: Int -> Text -> HOLTerm -> HOL cls thry Doc
ppPrefix prec s arg =
    let base = text s <+> ppTerm 999 arg in
      if prec == 1000 then parens base else base

ppComb :: Int -> (HOLTerm, HOLTerm) -> HOL cls thry Doc
ppComb prec (l, r) =
    let base = ppTerm 999 l <+> ppTerm 1000 r in
      if prec == 1000 then parens base else base

-- Printer for Theorems
	
-- | Pretty printer for 'HOLThm's.	
ppThm :: HOLThm -> HOL cls thry Doc
ppThm (Thm [] c) = text "|-" <+> ppTerm 0 c
ppThm (Thm asl c) =
    do fl <- getBenignFlag FlagPrintAllThm
       let base = text "|-" <+> ppTerm 0 c
       if not fl
          then text "..." <> base
          else let asl' = encloseSep empty empty comma $ mapM (ppTerm 0) asl in
                 asl' <+> base
ppThm _ = error "ppThm: exhaustive warning."

{-| 
  The @ShowHOL@ class is functionally equivalent to 'show' lifted to the 'HOL'
  monad.  It is used to retrieve the current working theory to be used with the
  context sensitive pretty printers for 'HOLTerm's and 'HOLType's.
-}
class ShowHOL cls thry a where
    {-| 
      A version of 'show' lifted to the 'HOL' monad for context sensitive pretty
      printers.
    -}
    showHOL :: a -> HOL cls thry String
    showHOLList :: [a] -> HOL cls thry String
    showHOLList = showHOLList' brackets comma <=< mapM showHOL

instance ShowHOL cls thry a => ShowHOL cls thry [a] where
    showHOL = showHOLList
    
instance (ShowHOL cls thry a, ShowHOL cls thry b) => ShowHOL cls thry (a, b) where
    showHOL (a, b) = showHOLList' parens comma =<< sequence 
                       [showHOL a, showHOL b]

instance (ShowHOL cls thry a, ShowHOL cls thry b, ShowHOL cls thry c) => 
         ShowHOL cls thry (a, b, c) where
    showHOL (a, b, c) = showHOLList' parens comma =<< sequence 
                          [showHOL a, showHOL b, showHOL c]

instance (ShowHOL cls thry a, ShowHOL cls thry b
         ,ShowHOL cls thry c, ShowHOL cls thry d) => 
         ShowHOL cls thry (a, b, c, d) where
    showHOL (a, b, c, d) = showHOLList' parens comma =<< sequence
                             [showHOL a, showHOL b, showHOL c, showHOL d]

-- Prints a list of strings provided a wrapper function and seperator document.
showHOLList' :: (HOL cls thry Doc -> HOL cls thry Doc) -> HOL cls thry Doc 
             -> [String] -> HOL cls thry String
showHOLList' wrap sepr =
    liftM show . wrap . sep . sequence . showHOLListRec sepr
  
-- Useful to have at top level for ppThm.
showHOLListRec :: HOL cls thry Doc -> [String] -> [HOL cls thry Doc]
showHOLListRec _ [] = [empty]
showHOLListRec _ [x] = [text $ pack x]
showHOLListRec sepr (x:xs) = (text' x <> sepr <> space) : showHOLListRec sepr xs
   where text' = text . pack

-- orphan instances
instance ShowHOL cls thry HOLType where
    showHOL = liftM ((:) ':' . show ) . ppType

instance ShowHOL cls thry HOLTerm where
    showHOL = liftM show . ppTerm 0

instance ShowHOL cls thry HOLThm where
    showHOL = liftM show . ppThm

{-| 
  Prints a HOL object with a new line.  A composition of 'putStrLnHOL' and
  'showHOL'.
-}
printHOL :: ShowHOL cls thry a => a -> HOL cls thry ()
printHOL = putStrLnHOL <=< showHOL
