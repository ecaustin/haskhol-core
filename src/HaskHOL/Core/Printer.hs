{-# LANGUAGE ScopedTypeVariables #-}
{-|
  Module:    HaskHOL.Core.Printer
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
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
      -- * Extensible Printer Operators
    , addUnspacedBinop 
    , addPrebrokenBinop
    , removeUnspacedBinop
    , removePrebrokenBinop
    , getUnspacedBinops
    , getPrebrokenBinops
      -- * Printing in the 'HOL' Monad
    , PrintM
    , getPrec
    , setPrec
    , ShowHOL(..)
    , showHOL
    , printHOL
    ) where

import Prelude hiding ((<$>))
import HaskHOL.Core.Lib hiding (ask, base)
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad
import HaskHOL.Core.Basics

import Control.Lens hiding (Const, op, cons, snoc)
import Control.Monad.ST
import Data.STRef
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Text.PrettyPrint.ANSI.Leijen

{-| 
  The computational monad for our pretty-printer.  
  Exposed for use in later libraries.
-}
type PrintM s = ReaderT (STRef s PrintState) (CatchT (ST s))

data PrintState = PrintState
    { _precedence :: !Int
    , _printCtxt :: ParseContext
    }

makeLenses ''PrintState

initPrintState :: ParseContext -> PrintState
initPrintState = PrintState 0

modPrintState :: (PrintState -> PrintState) -> PrintM s ()
modPrintState f =
    do ref <- ask
       lift . lift $ modifySTRef' ref f

viewPrintState :: (PrintState -> a) -> PrintM s a
viewPrintState f =
    do ref <- ask
       lift . lift $ f `fmap` readSTRef ref

testPrintState :: (PrintState -> a) -> (a -> Bool) -> PrintM s Bool
testPrintState f p =
    do ref <- ask
       p `fmap` (lift . lift $ f `fmap` readSTRef ref)

-- utility functions
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
    overParseContext unspaced (\ ops -> nub (op:ops))

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
    overParseContext prebroken (\ ops -> nub (op:ops))

{-| 
  Specifies a symbol to stop being recognized as an unspaced, binary operator 
  by the printer.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
removeUnspacedBinop :: Text -> HOL Theory thry ()
removeUnspacedBinop op =
    overParseContext unspaced (delete op)

{-| 
  Specifies a symbol to stop being recognized as an prebroken, binary operator 
  by the printer.

  Note that technically this method should be considered benign, however, for
  simplicity of implementation it is defined using 'modifyExt' and thus must be
  tagged a 'Theory' computation.
-}
removePrebrokenBinop :: Text -> HOL Theory thry ()
removePrebrokenBinop op =
    overParseContext prebroken (delete op)

{-| 
  Returns the list of all symbols current recognized as unspaced, binary
  operators by the printer.
-}
getUnspacedBinops :: HOL cls thry [Text]
getUnspacedBinops =
    viewParseContext unspaced

{-| 
  Returns the list of all symbols current recognized as prebroken, binary
  operators by the printer.
-}
getPrebrokenBinops :: HOL cls thry [Text]
getPrebrokenBinops =
    viewParseContext prebroken

-- | Returns the current precedence value from the pretty-printer's state.
getPrec :: PrintM s Int
getPrec = viewPrintState $ view precedence

-- | Sets a new precedence value in the pretty-printer's state.
setPrec :: Int -> PrintM s ()
setPrec = modPrintState . set precedence

getInterface :: PrintM s [(Text, (Text, HOLType))]
getInterface = viewPrintState $ view (printCtxt . interface)

getLefts :: PrintM s [(Text, Int)]
getLefts = viewPrintState $ view (printCtxt . lefts)

getRights :: PrintM s [(Text, Int)]
getRights = viewPrintState $ view (printCtxt . rights)

parsesAsBinder :: Text -> PrintM s Bool
parsesAsBinder op = testPrintState (view (printCtxt . binders)) (elem op)

parsesAsTyBinder :: Text -> PrintM s Bool
parsesAsTyBinder op = testPrintState (view (printCtxt . tyBinders)) (elem op)

parsesAsPrefix :: Text -> PrintM s Bool
parsesAsPrefix op = testPrintState (view (printCtxt . prefixes)) (elem op)

unspacedBinops :: PrintM s [Text]
unspacedBinops = viewPrintState $ view (printCtxt . unspaced)

prebrokenBinops :: PrintM s [Text]
prebrokenBinops = viewPrintState $ view (printCtxt . prebroken)

-- | Pretty printer for 'HOLType's.
ppType :: HOLType -> PrintM s Doc
ppType (TyVar False x) = return $! pretty (unpack x)
ppType (TyVar True x) = return . pretty . unpack $ '\'' `cons` x
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
                then return . pretty $ unpack name
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
                           do args' <- mapM (\ x -> setPrec 0 >> ppType x) args
                              return $! ppTypeApp "," True args' <+> 
                                        pretty (unpack bin)
  where ppTypeApp :: String -> Bool -> [Doc] -> Doc
        ppTypeApp sepr flag ds =
            case tryFoldr1 (\ x y -> x <+> text sepr <+> y) ds of
              Nothing -> empty
              Just bod -> if flag then parens bod else bod


-- Printer for Terms
-- | Pretty printer for 'HOLTerm's.
ppTerm :: HOLTerm -> PrintM s Doc
ppTerm tm =
-- numeral case
    (pretty `fmap` destNumeral tm) <|>
-- List case
    (encloseSep lbracket rbracket semi `fmap` 
       (mapM (\ x -> setPrec 0 >> ppTerm x) =<< destList tm)) <|>
-- Type combination case
    (ppTyComb =<< destTyComb tm) <|>
-- Let case
    (ppLet =<< destLet tm) <|>
-- General abstraction case -- needs work
    if isGAbs tm then ppGAbs tm
    else let (hop, args) = stripComb tm in
-- Base term abstraction case
    if isAbs hop && null args then ppBinder "\\" False hop
-- Base type abstraction case
    else if isTyAbs hop && null args then ppBinder "\\\\" True hop
-- Reverse interface for other cases
    else let s0 = nameOf hop
             ty0 = typeOf hop in
    do s <- reverseInterface s0 ty0
-- Match terms
       if s == "_MATCH" && length args == 2 && 
          --not ideal from a performance aspect, but it makes things cleaner 
          test' (destClauses $ args !! 1)
          then let (m:cs:_) = args in ppMatch m cs                       
-- Conditional case
          else if s == "COND" && length args == 3
          then let (c:t:e:_) = args in ppCond c t e                    
-- Prefix operator case           
          else do cond1 <- parsesAsPrefix s
                  if cond1 && length args == 1 
                     then ppPrefix s (head args)
-- Non-lambda term and type binder case
                     else ppBinders s hop args tm <|> 
                          (ppComb =<< destComb tm) <|>
                          fail' "ppTerm: printer error - unrecognized term"     

ppBinders :: Text -> HOLTerm -> [HOLTerm] -> HOLTerm -> PrintM s Doc
ppBinders s hop args tm =
     do cond2 <- parsesAsBinder s
        if cond2 && length args == 1 && isGAbs (head args)
        then ppBinder s False tm
-- Non-lambda type binder case
        else do cond3 <- parsesAsTyBinder s
                if cond3 && length args == 1 && isTyAbs (head args)
                then ppBinder s True tm
-- Infix operator case
                else ppOperators s hop args tm

ppOperators :: Text -> HOLTerm -> [HOLTerm] -> HOLTerm -> PrintM s Doc
ppOperators s hop args tm =
    do getRight <- assoc s `fmap` getRights
       getLeft <- assoc s `fmap` getLefts
       if (test' getRight || test' getLeft) && length args == 2
          then do args' <- 
                     if test' getRight
                     then do (tms, tmt) <- splitListM (destBinaryTm hop) tm
                             return $! tms ++ [tmt]
                     else do (tmt, tms) <- revSplitListM (destBinaryTm hop) tm
                             return $! tmt:tms 
                  prec <- getPrec
                  uops <- unspacedBinops
                  pops <- prebrokenBinops
                  let newprec = tryd 0 (getRight <|> getLeft)
                      wrapper = if newprec <= prec then parens else id
                      sepr = if s `elem` uops then (<>) else (<+>)
                      hanger x y = if s `elem` pops
                                   then x `sepr` (pretty (unpack s) <+> y)
                                   else (x <+> pretty (unpack s)) `sepr` y
                  (barg:bargs) <- mapM (\x -> setPrec newprec >> ppTerm x) args'
                  return . wrapper . foldr (flip hanger) barg $ reverse bargs
-- Base constant or variable case
          else ppConstants s hop args
  where destBinaryTm :: HOLTerm -> HOLTerm -> PrintM s (HOLTerm, HOLTerm)
        destBinaryTm c t =
            do (il, r) <- destComb t
               (i, l) <- destComb il
               if i == c
                  then do i' <- destConst i <|> destVar i
                          c' <- destConst c <|> destVar c
                          i'' <- uncurry reverseInterface i'
                          c'' <- uncurry reverseInterface c'
                          if i'' == c''
                             then return (l, r)
                             else fail "destBinaryTm"
                  else fail "destBinaryTm"

ppConstants :: Text -> HOLTerm -> [HOLTerm] -> PrintM s Doc
ppConstants s hop args
    | null args && (isConst hop || isVar hop) =
          do cond1 <- parsesAsBinder s
             cond2 <- parsesAsTyBinder s
             cond3 <- (test' . assoc s) `fmap` getRights
             cond4 <- (test' . assoc s) `fmap` getLefts
             cond5 <- parsesAsPrefix s
             let base = text $ unpack s
             return $! if cond1 || cond2 || cond3 || cond4 || cond5
                       then parens base
                       else base
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
                      test' (typeMatch ty ty0 ([], [], []))) iface
       return $! maybe s0 fst s1

ppTyComb :: (HOLTerm, HOLType) -> PrintM s Doc
ppTyComb (t, ty) =
    do prec <- getPrec
       t' <- setPrec 999 >> ppTerm t
       ty' <- setPrec prec >> ppType ty
       let base = t' <+> brackets (char ':' <> ty')
       return $! if prec == 1000 then parens base else base

ppLet :: ([(HOLTerm, HOLTerm)], HOLTerm) -> PrintM s Doc
ppLet (eqs@(_:_), bod) =
    do prec <- getPrec
       eqs' <- mapM ppLet' eqs
       bod' <- setPrec 0 >> ppTerm bod
       let base = (text "let" <+> encloseSep empty empty (text "and") eqs' <+> 
                   text "in") <$> indent 2 bod'
       return $! if prec == 0 then base else parens base 
  where ppLet' :: (HOLTerm, HOLTerm) -> PrintM s Doc
        ppLet' x =
            (do x' <- uncurry primMkEq x
                setPrec 0 >> ppTerm x') <|> 
            (return $! text "<*bad let binding*>")
ppLet (_, bod) = ppTerm bod

ppGAbs :: HOLTerm -> PrintM s Doc
ppGAbs tm =
    let (vs, bod) = stripGAbs tm in
      do prec <- getPrec
         vs' <- mapM (\ x -> setPrec 999 >> ppTerm x) vs
         bod' <- setPrec 0 >> ppTerm bod
         let base = char '\\' <+> sep vs' <+> char '.' <+> bod'
         return $! if prec == 0 then base else parens base

ppBinder :: Text -> Bool -> HOLTerm -> PrintM s Doc
ppBinder prep f tm =
    let (vs, bod) = strip f ([], tm)
        bvs = pretty (unpack prep) <> 
              foldr (\ x acc -> acc <+> pretty (unpack x)) empty vs <> 
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

ppMatch :: HOLTerm -> HOLTerm -> PrintM s Doc
ppMatch m cls =
    do prec <- getPrec
       m' <- setPrec 0 >> ppTerm m
       cls' <- ppClauses =<< destClauses cls
       let base = text "match" <+> m' <+> text "with" <+> cls'
       return $! if prec == 0 then base else parens base
  where ppClauses :: [[HOLTerm]] -> PrintM s Doc
        ppClauses [c] = ppClause c
        ppClauses (c:cs) = 
            do c' <- ppClause c 
               cs' <- ppClauses cs
               return $! c' <+> char '|' <+> cs'
        ppClauses _ = return empty

        ppClause :: [HOLTerm] -> PrintM s Doc
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

destClauses :: forall m. MonadThrow m => HOLTerm -> m [[HOLTerm]]
destClauses tm =
    let (s, args) = stripComb tm in
      if nameOf s == "_SEQPATTERN" && length args == 2
      then do c <- destClause (head args)
              cs <- destClauses (args !! 1)
              return (c:cs)
      else do c <- destClause tm
              return [c]
  where destClause :: HOLTerm -> m [HOLTerm]
        destClause tm' =
            do (_, pbod) <- stripExists' `fmap` (body =<< body tm')
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
                       else fail' "destClause"

        stripExists' :: HOLTerm -> ([HOLTerm], HOLTerm)
        stripExists' = splitList (destBinder "?")

ppCond :: HOLTerm -> HOLTerm -> HOLTerm -> PrintM s Doc
ppCond c t e =
    do prec <- getPrec
       c' <- setPrec 0 >> ppTerm c
       t' <- setPrec 0 >> ppTerm t
       e' <- setPrec 0 >> ppTerm e
       let base = text "if" <+> c' <+> text "then" <+> t' <+> text "else" <+> e'
       return $! if prec == 0 then base else parens base

ppPrefix :: Text -> HOLTerm -> PrintM s Doc
ppPrefix s arg =
    do prec <- getPrec
       arg' <- setPrec 999 >> ppTerm arg
       let base = text (unpack s) <+> arg'
       return $! if prec == 1000 then parens base else base

ppComb :: (HOLTerm, HOLTerm) -> PrintM s Doc
ppComb (l, r) =
    do prec <- getPrec
       l' <- setPrec 999 >> ppTerm l
       r' <- setPrec 1000 >> ppTerm r
       let base = l' <+> r'
       return $! if prec == 1000 then parens base else base

-- Printer for Theorems

-- | Pretty printer for 'HOLThm's.	
ppThm :: HOLThm -> PrintM s Doc
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
    buildDoc :: a -> PrintM s Doc

    buildDocList :: [a] -> PrintM s Doc
    buildDocList = buildDocList' brackets comma <=< mapM buildDoc

instance ShowHOL a => ShowHOL [a] where
    buildDoc = buildDocList
    
instance (ShowHOL a, ShowHOL b) => ShowHOL (a, b) where
    buildDoc (a, b) = buildDocList' parens comma =<< sequence
                       [buildDoc a, buildDoc b]

instance (ShowHOL a, ShowHOL b, ShowHOL c) => ShowHOL (a, b, c) where
    buildDoc (a, b, c) = buildDocList' parens comma =<< sequence
                          [buildDoc a, buildDoc b, buildDoc c]

instance (ShowHOL a, ShowHOL b,ShowHOL c, ShowHOL d) => 
         ShowHOL (a, b, c, d) where
    buildDoc (a, b, c, d) = buildDocList' parens comma =<< sequence
                             [ buildDoc a, buildDoc b
                             , buildDoc c, buildDoc d ]

-- Prints a list of strings provided a wrapper function and seperator document.
buildDocList' :: (Doc -> Doc) -> Doc -> [Doc] -> PrintM s Doc
buildDocList' wrap sepr =
    return . wrap . sep . buildDocListRec sepr
  
-- Useful to have at top level for ppThm.
buildDocListRec :: Doc -> [Doc] -> [Doc]
buildDocListRec _ [] = [empty]
buildDocListRec _ [x] = [x]
buildDocListRec sepr (x:xs) = (x <> sepr <> space) : buildDocListRec sepr xs

-- orphan instances
instance ShowHOL HOLType where
    buildDoc ty = (char ':' <>) `fmap` ppType ty

instance ShowHOL HOLTerm where
    buildDoc = ppTerm

instance ShowHOL HOLThm where
    buildDoc = ppThm

showHOL :: ShowHOL a => a -> HOL cls thry String
showHOL x = 
    do ctxt <- parseContext
       either (fail . show) (return . show) $ runST
         (do ref <- newSTRef $ initPrintState ctxt
             runCatchT $ runReaderT (buildDoc x) ref)

{-| 
  Prints a HOL object with a new line.  A composition of 'putStrLnHOL' and
  'buildDoc'.
-}
printHOL :: ShowHOL a => a -> HOL cls thry ()
printHOL x = 
    do ctxt <- parseContext
       either (fail . show) putDocHOL $ runST
         (do ref <- newSTRef $ initPrintState ctxt
             runCatchT $ runReaderT (buildDoc x) ref)
