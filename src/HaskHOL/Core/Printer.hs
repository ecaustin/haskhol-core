{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, TemplateHaskell, UndecidableInstances, 
             ViewPatterns #-}

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
    ( -- * Pretty Printer Flags
      FlagRevInterface(..)
    , FlagPrintAllThm(..)
      -- * Extensible Printer Operators
    , addUnspacedBinop     -- :: String -> HOL Theory thry ()
    , addPrebrokenBinop    -- :: String -> HOL Theory thry ()
    , removeUnspacedBinop  -- :: String -> HOL Theory thry ()
    , removePrebrokenBinop -- :: String -> HOL Theory thry ()
    , getUnspacedBinops    -- :: HOLContext thry -> [String]
    , getPrebrokenBinops   -- :: HOLContext thry -> [String]
      -- * Pretty Printers
    , ppType -- :: HOLType -> String
    , ppTerm -- :: HOLContext thry -> HOLTerm -> String
    , ppThm  -- :: HOLContext thry -> HOLThm -> String
      -- * Printing in the 'HOL' Monad
    , ShowHOL(..)
    , printHOL    -- :: ShowHOL a => a -> HOL cls thry ()
    ) where

import HaskHOL.Core.Lib hiding (empty, lefts, rights)
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

-- new flags and extensions
-- | Flag to indicate whether the interface should be reversed on printing.
newFlag "FlagRevInterface" True

{-| 
  Flag to indicate if the entirety of a theorem should be printed, as opposed
  to just the conclusion term.
-}
newFlag "FlagPrintAllThm" True

newExtension "UnspacedBinops" [| [",", "..", "$"] :: [String] |]

newExtension "PrebrokenBinops" [| ["==>"] :: [String] |]

{-| 
  Specifies a symbol to be recognized as an unspaced, binary operator by the
  printer.  Applications involving these operators will be built with the '<>'
  combinator as opposed to '<+>'.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
addUnspacedBinop :: String -> HOL Theory thry ()
addUnspacedBinop op =
    modifyExt (\ (UnspacedBinops ops) -> UnspacedBinops $ op `insert` ops)

{-| 
  Specifies a symbol to be recognized as a prebroken, binary operator by the
  printer.  Applications involving these operators will have their right-hand
  side argument printed on the next line using the 'hang' combinator.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
addPrebrokenBinop :: String -> HOL Theory thry ()
addPrebrokenBinop op =
    modifyExt (\ (PrebrokenBinops ops) -> PrebrokenBinops $ op `insert` ops)

{-| 
  Specifies a symbol to stop being recognized as an unspaced, binary operator 
  by the printer.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
removeUnspacedBinop :: String -> HOL Theory thry ()
removeUnspacedBinop op =
    modifyExt (\ (UnspacedBinops ops) -> UnspacedBinops $ ops \\ [op])

{-| 
  Specifies a symbol to stop being recognized as an prebroken, binary operator 
  by the printer.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
removePrebrokenBinop :: String -> HOL Theory thry ()
removePrebrokenBinop op =
    modifyExt (\ (PrebrokenBinops ops) -> PrebrokenBinops $ ops \\ [op])

{-| 
  Returns the list of all symbols current recognized as unspaced, binary
  operators by the printer.
-}
getUnspacedBinops :: HOLContext thry -> [String]
getUnspacedBinops ctxt =
    let (UnspacedBinops ops) = getExtCtxt ctxt in ops

{-| 
  Returns the list of all symbols current recognized as prebroken, binary
  operators by the printer.
-}
getPrebrokenBinops :: HOLContext thry -> [String]
getPrebrokenBinops ctxt =
    let (PrebrokenBinops ops) = getExtCtxt ctxt in ops

-- | Pretty printer for 'HOLType's.
ppType :: HOLType -> String
ppType = render . ppTypeRec 0
  where ppTypeRec :: Int -> HOLType -> Doc
        ppTypeRec _ (view -> TyVar False x) = text x
        ppTypeRec _ (view -> TyVar True x) = text $ '\'' : x
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
                              name' = if ar < 0 then '_':name else name
                          return (name', tys) of
                    Just (op, []) -> text op
                    Just ("fun", ty1:ty2:[]) ->
                        ppTypeApp "->" (prec > 0) [ ppTypeRec 1 ty1
                                                  , ppTypeRec 0 ty2]
                    Just ("sum", ty1:ty2:[]) -> 
                        ppTypeApp "+" (prec > 2) [ ppTypeRec 3 ty1
                                                 , ppTypeRec 2 ty2]
                    Just ("prod", ty1:ty2:[]) -> 
                        ppTypeApp "#" (prec > 4) [ ppTypeRec 5 ty1
                                                 , ppTypeRec 4 ty2]
                    Just ("cart", ty1:ty2:[]) -> 
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
                                     Right x' -> ppTermRec 0 x'
                                     _ -> text "<*bad let binding*>"
                         base = hang
                                (text "let" <+> 
                                 foldr (\ eq' acc -> acc <+> text "and" <+> 
                                          ppLet eq') (ppLet eq) eqs <+>
                                 text "in") 2 $ ppTermRec 0 bod in
                       if prec == 0 then base else parens base       
                 _ ->
                  let (hop, args) = stripComb tm in
-- Base term abstraction case
                    if isAbs hop && null args 
                    then ppBinder prec "\\" False hop
-- Base type abstraction case
                    else 
                    if isTyAbs hop && null args
                    then ppBinder prec "\\\\" True hop
-- Reverse interface for other cases
                    else let s0 = case view hop of
                                    Var x _ -> x
                                    Const x _ _ -> x
                                    _ -> ""
                             ty0 = typeOf hop
                             s = reverseInterface s0 ty0 in
-- General abstraction case
                    if s == "GABS"
                    then case destGAbs tm of
                           Nothing -> text "ppTerm: printer error - GAbs case"
                           Just (vs, bod) ->
                             let base = char '\\' <+> ppTermRec 999 vs <+> 
                                        char '.' <+> ppTermRec 0 bod in
                               if prec == 0 then base else parens base
-- Conditional case
                    else 
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
                    then let base = text s <+> ppTermRec 999 (head args) in
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
                                 else let (tmt, tms) = 
                                           revSplitList (destBinaryTm hop) tm in
                                        tmt:tms 
                               newprec = fromMaybe 0 (getRight <|> getLeft)
                               wrapper = if newprec <= prec then parens else id
                               sepr = 
                                 if s `elem` getUnspacedBinops ctxt 
                                 then (\ x y -> cat [x, y]) 
                                 else (\ x y -> sep [x, y])
                               hanger = 
                                 if s `elem` getPrebrokenBinops ctxt
                                 then (\ x y -> x `sepr` (text s <+> y))
                                 else (\ x y -> (x <+> text s) `sepr` y) in
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
                         then parens $ text s
                         else text s
-- Base combination case                   
                    else case destComb tm of
                           Just (l, r) ->
                             let base = ppTermRec 999 l <+> ppTermRec 1000 r in
                               if prec == 1000 then parens base else base
                           _ -> text "ppTerm: printer error - unrecognized term"

        grabInfix :: Assoc -> [(String, (Int, Assoc))] -> [(String, Int)]
        grabInfix a = 
            mapMaybe (\ (x, (n, a')) -> if a == a' 
                                        then Just (x, n)
                                        else Nothing)
        binds :: [String]
        binds = binders ctxt

        tybinds :: [String]
        tybinds = tyBinders ctxt

        prefix :: [String]
        prefix = prefixes ctxt

        lefts :: [(String, Int)]
        lefts = grabInfix AssocLeft $ infixes ctxt

        rights :: [(String, Int)]
        rights = grabInfix AssocRight $ infixes ctxt

        ppTermSeq :: String -> Int -> [HOLTerm] -> Doc
        ppTermSeq sepr prec = ppTermSeqRec
          where ppTermSeqRec [] = empty
                ppTermSeqRec (x:[]) = ppTermRec prec x
                ppTermSeqRec (x:xs) =
                  ppTermRec prec x <+> text sepr <+> ppTermSeqRec xs

        ppBinder :: Int -> String -> Bool -> HOLTerm -> Doc
        ppBinder prec prep f tm =
            let (vs, bod) = if f then stripTy ([], tm) else stripTm ([], tm)
                base = let bvs = text prep <> 
                                 foldr (\ x acc -> acc <+> text x) empty vs <>
                                 char '.'
                           indent = min (1 + length (render bvs)) 5 in
                         cat [ bvs
                             , nest indent $ ppTermRec prec bod
                             ] in
              if prec == 0 then base else parens base
          where stripTm :: ([String], HOLTerm) -> ([String], HOLTerm)
                stripTm (acc, view -> Abs (view -> Var bv _) bod) = 
                    stripTm (bv:acc, bod)
                stripTm pat@(acc, view -> Comb (view -> Const s _ _) 
                                 (view -> Abs (view -> Var bv _) bod))
                    | s == prep = stripTm (bv:acc, bod)
                    | otherwise = pat
                stripTm pat = pat
   
                stripTy :: ([String], HOLTerm) -> ([String], HOLTerm)
                stripTy (acc, view -> TyAbs (view -> TyVar _ bv) bod) =
                    stripTy (('\'':bv):acc, bod)
                stripTy pat@(acc, view -> Comb (view -> Const s _ _)
                                 (view -> TyAbs (view -> TyVar _ bv) bod))
                    | s == prep = stripTy (('\'':bv):acc, bod)
                    | otherwise = pat
                stripTy pat = pat

        destBinaryTm :: HOLTerm -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
        destBinaryTm c tm =
            do (il, r) <- destComb tm
               (i, l) <- destComb il
               if i == c
                  then do i' <- destConst i
                          c' <- destConst c
                          if uncurry reverseInterface i' == 
                             uncurry reverseInterface c'
                             then Just (l, r)
                             else Nothing
                  else Nothing

        reverseInterface :: String -> HOLType -> String
        reverseInterface s0 ty0
            | not (getBenignFlagCtxt FlagRevInterface ctxt) = s0
            | otherwise = fromMaybe s0 . liftM fst .
                            find (\ (_, (s', ty)) -> s' == s0 && 
                                  isJust (typeMatch ty ty0 ([], [], []))) $
                              getInterface ctxt

-- Printer for Theorems
	
-- | Pretty printer for 'HOLTheorem's.	
ppThm :: HOLContext thry -> HOLThm -> String
ppThm ctxt (view -> Thm asl c) = render ppThmRec
  where ppThmRec :: Doc
        ppThmRec = 
          let c' = text $ ppTerm ctxt c
              asl'
                  | null asl = [empty]
                  | not (getBenignFlagCtxt FlagPrintAllThm ctxt) = [text "..."]
                  | otherwise = showHOLListRec comma $ map (ppTerm ctxt) asl in
            sep (asl' ++ [text "|-" <+> c'])

{-| 
  The @ShowHOL@ class is functionally equivalent to 'show' lifted to the 'HOL'
  monad.  It is used to retrieve the current working theory to be used with the
  context sensitive pretty printers for 'HOLTerm's and 'HOLType's.
-}
class ShowHOL a thry where
    {-| 
      A version of 'show' lifted to the 'HOL' monad for context sensitive pretty
      printers.
    -}
    showHOL :: a -> HOL cls thry String
                             
instance ShowHOL String thry where
    showHOL = return

instance ShowHOL a thry => ShowHOL [a] thry where
    showHOL = liftM (showHOLList brackets comma) . mapM showHOL

instance (ShowHOL a thry, ShowHOL b thry) => ShowHOL (a, b) thry where
    showHOL (a, b) = liftM (showHOLList parens comma) . sequence $ 
                       [showHOL a, showHOL b]

instance (ShowHOL a thry, ShowHOL b thry, ShowHOL c thry) => 
         ShowHOL (a, b, c) thry where
    showHOL (a, b, c) = liftM (showHOLList parens comma) . sequence $ 
                          [showHOL a, showHOL b, showHOL c]

instance (ShowHOL a thry, ShowHOL b thry, ShowHOL c thry, ShowHOL d thry) => 
         ShowHOL (a, b, c, d) thry where
    showHOL (a, b, c, d) = liftM (showHOLList parens comma) . sequence $ 
                             [showHOL a, showHOL b, showHOL c, showHOL d]

-- Prints a list of strings provided a wrapper function and seperator document.
showHOLList :: (Doc -> Doc) -> Doc -> [String] -> String
showHOLList wrap sepr = render . wrap . sep . showHOLListRec sepr
  
-- Useful to have at top level for ppThm.
showHOLListRec :: Doc -> [String] -> [Doc]
showHOLListRec _ [] = [empty]
showHOLListRec _ (x:[]) = [text x]
showHOLListRec sepr (x:xs) = (text x <> sepr <> space) : showHOLListRec sepr xs

-- orphan instances
instance ShowHOL Assoc thry where
    showHOL (AssocNone) = return "None" 
    showHOL (AssocLeft) = return "Left"
    showHOL (AssocRight) = return "Right"

instance ShowHOL TypeOp thry where
    showHOL = return . show

instance ShowHOL HOLType thry where
    showHOL ty = return $ ':' : ppType ty

instance ShowHOL HOLTerm thry where
    showHOL tm = do ctxt <- get
                    return $! ppTerm ctxt tm

instance ShowHOL HOLThm thry where
    showHOL thm = do ctxt <- get
                     return $! ppThm ctxt thm

{-| 
  Prints a HOL object with a new line.  A composition of 'putStrLnHOL' and
  'showHOL'.
-}
printHOL :: ShowHOL a thry => a -> HOL cls thry ()
printHOL = putStrLnHOL <=< showHOL

deriveLiftMany [''UnspacedBinops, ''PrebrokenBinops]
