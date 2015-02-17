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

import HaskHOL.Core.Lib hiding (empty, lefts, rights, base)
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics
import HaskHOL.Core.Parser

{- 
  Used for a number of pretty-printing primitives that don't really need to be
  exposed to the rest of the system.  Although no harm would come should we
  elect to move this to be re-exported by Core.Lib.
-}
import Text.PrettyPrint

-- | Pretty printer for 'HOLType's.
ppType :: HOLType -> String
ppType = render . ppTypeRec 0
  where ppTypeRec :: Int -> HOLType -> Doc
        ppTypeRec _ (TyVar False x) = text $ unpack x
        ppTypeRec _ (TyVar True x) = text $ '\'' : unpack x
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
                          return (unpack name', tys) of
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
  
        ppTypeApp :: String -> Bool -> [Doc] -> Doc
        ppTypeApp sepr flag ds =
            case tryFoldr1 (\ x y -> x <+> text sepr <+> y) ds of
              Nothing -> empty
              Just bod -> if flag then parens bod else bod

-- Printer for Terms
-- | Pretty printer for 'HOLTerm's.
ppTerm :: HOLContext thry -> HOLTerm -> String
ppTerm ctxt = render . ppTermRec 0
  where ppTermRec :: Int -> HOLTerm -> Doc
        ppTermRec prec tm =
-- numeral case
          case destNumeral tm of
           Just x -> integer x
           Nothing ->
-- List case
            case destList tm of
             Just tms -> brackets $ ppTermSeq ";" 0 tms
             Nothing ->
-- Type combination case
              case destTyComb tm of
               Just (t, ty) -> 
                 let base = ppTermRec 999 t <+> 
                            brackets (char ':' <> text (ppType ty)) in
                   if prec == 1000 then parens base else base
               Nothing ->
-- Let case
                case destLet tm of
                 Just (eq:eqs, bod) ->
                     let ppLet x = case uncurry primMkEq x of
                                     Just x' -> ppTermRec 0 x'
                                     _ -> text "<*bad let binding*>"
                         base = hang
                                (text "let" <+> 
                                 foldr (\ eq' acc -> acc <+> text "and" <+> 
                                          ppLet eq') (ppLet eq) eqs <+>
                                 text "in") 2 $ ppTermRec 0 bod in
                       if prec == 0 then base else parens base       
                 _ ->
-- General abstraction case -- needs work
                  if isGAbs tm
                  then let (vs, bod) = stripGAbs tm
                           base = char '\\' <+> sep (map (ppTermRec 999) vs) <+>
                                  char '.' <+> ppTermRec 0 bod in
                         if prec == 0 then base else parens base
                  else let (hop, args) = stripComb tm in
-- Base term abstraction case
                       if isAbs hop && null args 
                       then ppBinder prec "\\" False hop
-- Base type abstraction case
                       else 
                       if isTyAbs hop && null args
                       then ppBinder prec "\\\\" True hop
-- Reverse interface for other cases
                       else let s0 = case hop of
                                       Var x _ -> x
                                       Const x _ -> x
                                       _ -> textEmpty
                                ty0 = typeOf hop
                                s = reverseInterface s0 ty0 
                                ss = unpack s in
-- Conditional case
                       if s == "COND" && length args == 3
                       then let (c:t:e:_) = args
                                base = text "if" <+> ppTermRec 0 c <+> 
                                       text "then" <+> ppTermRec 0 t <+> 
                                       text "else" <+> ppTermRec 0 e in
                              if prec == 0 then base else parens base
-- Prefix operator case           
                       else 
                       if s `elem` prefix && 
                          length args == 1
                       then let base = text ss <+> ppTermRec 999 (head args) in
                              if prec == 1000 then parens base else base
-- Non-lambda term binder case
                       else 
                       if s `elem` binds && 
                          length args == 1 && 
                          isGAbs (head args)
                       then ppBinder prec s False tm
-- Non-lambda type binder case
                       else
                       if s `elem` tybinds && 
                          length args == 1 &&
                          isTyAbs (head args)
                       then ppBinder prec s True tm
-- Infix operator case
                       else 
                       let getRight = s `lookup` rights
                           getLeft = s `lookup` lefts in
                         if (isJust getRight || isJust getLeft) &&
                            length args == 2
                         then let (barg:bargs) = 
                                   if isJust getRight
                                   then let (tms, tmt) = 
                                             splitList (destBinaryTm hop) tm in
                                          tms ++ [tmt]
                                   else 
                                    let (tmt, tms) = 
                                         revSplitList (destBinaryTm hop) tm in
                                      tmt:tms 
                                  newprec = fromMaybe 0 (getRight <|> getLeft)
                                  wrapper = 
                                      if newprec <= prec then parens else id
                                  sepr = 
                                    if s `elem` unspacedBinops ctxt
                                    then (\ x y -> cat [x, y]) 
                                    else (\ x y -> sep [x, y])
                                  hanger = 
                                    if s `elem` prebrokenBinops ctxt
                                    then (\ x y -> x `sepr` (text ss <+> y))
                                    else (\ x y -> (x <+> text ss) `sepr` y) in
                                  wrapper $ 
                                    foldr (\ x acc -> acc `hanger` 
                                                      ppTermRec newprec x)
                                    (ppTermRec newprec barg) $ reverse bargs
-- Base constant or variable case
                       else 
                       if null args && (isConst hop || isVar hop)
                       then if s `elem` binds || 
                               s `elem` tybinds ||
                               isJust (s `lookup` rights) || 
                               isJust (s `lookup` lefts) ||
                               s `elem` prefix
                            then parens $ text ss
                            else text ss
-- Base combination case                   
                       else case destComb tm of
                              Just (l, r) ->
                                let base = 
                                        ppTermRec 999 l <+> ppTermRec 1000 r in
                                  if prec == 1000 then parens base else base
                              _ -> text $ "ppTerm: printer error - " ++
                                          "unrecognized term"

        grabInfix :: Text -> [(Text, (Int, Text))] -> [(Text, Int)]
        grabInfix a = 
            mapMaybe (\ (x, (n, a')) -> if a == a' 
                                        then Just (x, n)
                                        else Nothing)
        binds :: [Text]
        binds = binders ctxt

        tybinds :: [Text]
        tybinds = tyBinders ctxt

        prefix :: [Text]
        prefix = prefixes ctxt

        lefts :: [(Text, Int)]
        lefts = grabInfix "left" $ infixes ctxt

        rights :: [(Text, Int)]
        rights = grabInfix "right" $ infixes ctxt

        ppTermSeq :: String -> Int -> [HOLTerm] -> Doc
        ppTermSeq sepr prec = ppTermSeqRec
          where ppTermSeqRec [] = empty
                ppTermSeqRec [x] = ppTermRec prec x
                ppTermSeqRec (x:xs) =
                  ppTermRec prec x <+> text sepr <+> ppTermSeqRec xs

        ppBinder :: Int -> Text -> Bool -> HOLTerm -> Doc
        ppBinder prec prep f tm =
            let (vs, bod) = if f then stripTy ([], tm) else stripTm ([], tm)
                base = let bvs = text (unpack prep) <> 
                                 foldr (\ x acc -> acc <+> text (unpack x)) 
                                   empty vs <> char '.'
                           indent = min (1 + length (render bvs)) 5 in
                         cat [ bvs
                             , nest indent $ ppTermRec prec bod
                             ] in
              if prec == 0 then base else parens base
          where stripTm :: ([Text], HOLTerm) -> ([Text], HOLTerm)
                stripTm (acc, Abs (Var bv _) bod) = 
                    stripTm (bv:acc, bod)
                stripTm pat@(acc, Comb (Const s _) 
                                 (Abs (Var bv _) bod))
                    | s == prep = stripTm (bv:acc, bod)
                    | otherwise = pat
                stripTm pat = pat
   
                stripTy :: ([Text], HOLTerm) -> ([Text], HOLTerm)
                stripTy (acc, TyAbs (TyVar _ bv) bod) =
                    stripTy (('\'' `cons` bv):acc, bod)
                stripTy pat@(acc, Comb (Const s _)
                                 (TyAbs (TyVar _ bv) bod))
                    | s == prep = stripTy (('\'' `cons` bv):acc, bod)
                    | otherwise = pat
                stripTy pat = pat

        destBinaryTm :: HOLTerm -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
        destBinaryTm c tm =
            do (il, r) <- destComb tm
               (i, l) <- destComb il
               if i == c
                  then do i' <- destConst i <|> destVar i
                          c' <- destConst c <|> destVar c
                          if uncurry reverseInterface i' == 
                             uncurry reverseInterface c'
                             then Just (l, r)
                             else Nothing
                  else Nothing

        reverseInterface :: Text -> HOLType -> Text
        reverseInterface s0 ty0
            | not (flagRevInterface ctxt) = s0
            | otherwise = fromMaybe s0 . liftM fst .
                            find (\ (_, (s', ty)) -> s' == s0 && 
                                  isJust (typeMatch ty ty0 ([], [], []))) $
                              getInterfaceCtxt ctxt

-- Printer for Theorems
	
-- | Pretty printer for 'HOLTheorem's.	
ppThm :: HOLContext thry -> HOLThm -> String
ppThm ctxt (Thm asl c) = render ppThmRec
  where ppThmRec :: Doc
        ppThmRec = 
          let c' = text $ ppTerm ctxt c
              asl'
                  | null asl = [empty]
                  | not (flagPrintAllThm ctxt) = [text "..."]
                  | otherwise = showHOLListRec comma $ map (ppTerm ctxt) asl in
            sep (asl' ++ [text "|-" <+> c'])
ppThm _ _ = error "ppThm: exhaustive warning."

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
    showHOL :: a -> HOL cls thry String
    showHOLList :: [a] -> HOL cls thry String
    showHOLList = liftM (showHOLList' brackets comma) . mapM showHOL

instance ShowHOL a => ShowHOL [a] where
    showHOL = showHOLList
    
instance (ShowHOL a, ShowHOL b) => ShowHOL (a, b) where
    showHOL (a, b) = liftM (showHOLList' parens comma) . sequence $ 
                       [showHOL a, showHOL b]

instance (ShowHOL a, ShowHOL b, ShowHOL c) => 
         ShowHOL (a, b, c) where
    showHOL (a, b, c) = liftM (showHOLList' parens comma) . sequence $ 
                          [showHOL a, showHOL b, showHOL c]

instance (ShowHOL a, ShowHOL b, ShowHOL c, ShowHOL d) => 
         ShowHOL (a, b, c, d) where
    showHOL (a, b, c, d) = liftM (showHOLList' parens comma) . sequence $ 
                             [showHOL a, showHOL b, showHOL c, showHOL d]

-- Prints a list of strings provided a wrapper function and seperator document.
showHOLList' :: (Doc -> Doc) -> Doc -> [String] -> String
showHOLList' wrap sepr = render . wrap . sep . showHOLListRec sepr
  
-- Useful to have at top level for ppThm.
showHOLListRec :: Doc -> [String] -> [Doc]
showHOLListRec _ [] = [empty]
showHOLListRec _ [x] = [text x]
showHOLListRec sepr (x:xs) = (text x <> sepr <> space) : showHOLListRec sepr xs

-- orphan instances
instance ShowHOL HOLType where
    showHOL ty = return $ ':' : ppType ty

instance ShowHOL HOLTerm where
    showHOL tm = do ctxt <- prepHOLContext
                    return $! ppTerm ctxt tm

instance ShowHOL HOLThm where
    showHOL thm = do ctxt <- prepHOLContext
                     return $! ppThm ctxt thm

{-| 
  Prints a HOL object with a new line.  A composition of 'putStrLnHOL' and
  'showHOL'.
-}
printHOL :: ShowHOL a => a -> HOL cls thry ()
printHOL = putStrLnHOL <=< showHOL
