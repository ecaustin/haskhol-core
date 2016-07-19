{-# LANGUAGE PatternSynonyms, ScopedTypeVariables #-}
{-|
  Module:    HaskHOL.Core.Basics
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module defines common utility functions that depend on data types
  introduced by HaskHOL. See the "HaskHOL.Core.Lib" module for utility functions
  that do not have this dependence.
-}
module HaskHOL.Core.Basics
    ( -- * Common Type Functions
      occursIn
    , tysubst
    , alphaUtype
      -- * Common Term Functions
    , mkEq
    , freeIn
    , variables
    , subst   
    , alpha     
    , alphaTyabs
    , findTerm   
    , findTermM
    , findTerms
    , findTermsM
    , findPath   
    , followPath
      -- * Common Theorem Functions
    , typeVarsInThm
    , thmFrees     
      -- * Derived Destructors and Constructors for Basic Terms
    , listMkComb  
    , listMkTyComb
    , listMkAbs
    , listMkTyAbs   
    , mkArgs     
    , rator       
    , rand      
    , bndvar     
    , body       
    , bndvarTyabs
    , bodyTyabs  
    , stripComb
    , stripTyComb 
    , stripAbs
    , stripTyAbs
      -- * Type Matching Functions
    , mkIComb
      -- * Predicates, Constructors, and Destructors for Binary Terms
    , isBinary 
    , isBinop
    , destBinary
    , pattern Binary
    , destBinop
    , pattern Binop
    , mkBinop
    , listMkBinop
    , binops
      -- * Predicates, Constructors, and Destructors for Complex Abstractions
    , isGAbs
    , isBinder 
    , isTyBinder
    , destGAbs
    , destBinder
    , pattern Bind
    , destBinderWhole
    , pattern BindWhole
    , destTyBinder
    , pattern TyBind
    , destTyBinderWhole
    , pattern TyBindWhole
    , stripGAbs
      -- * Predicates, Constructors, and Destructors for Propositions
    , isIff
    , isConj
    , isImp
    , isForall
    , isExists
    , isDisj
    , isNeg
    , isUExists
    , isTyAll
    , isTyEx
    , destIff
    , pattern (:<=>)
    , destConj
    , pattern (:/\)
    , destImp
    , pattern (:==>)
    , destForall
    , pattern Forall
    , destExists
    , pattern Exists
    , destDisj
    , pattern (:\/)
    , destNeg
    , pattern Neg
    , destUExists
    , pattern UExists
    , destTyAll
    , pattern TyAll
    , destTyEx
    , pattern TyEx
    , conjuncts
    , disjuncts
    , stripForall
    , stripExists
    , stripTyAll
    , stripTyEx
      -- * Predicates, Constructors, and Destructors for Other Terms
    , isCons
    , isList
    , isLet
    , destCons
    , destList
    , destLet
    , destNumeral
      -- * Term Nets
    , module HaskHOL.Core.Basics.Nets
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.Basics.Nets

-- Term Generation
mkVarInt :: Int -> HOLType -> HOLTerm
mkVarInt n = mkVar ('_' `cons` textShow n)

-- functions for manipulating types
{-| 
  Checks to see if the first type occurs in the second type.  Note that the
  predicate is also satisfied if the two types are equal.
-}
occursIn :: HOLType -> HOLType -> Bool
occursIn ty bigTy
  | ty == bigTy = True
  | otherwise = case bigTy of
                  TyApp _ args -> any (occursIn ty) args
                  _ -> False

{-| 
  Basic type substitution that ignores type operators and prunes the 
  substitution environment of bound variables rather than handle renaming.
  Works for all types, variable and non-variable alike.  Fails with 'Left' when 
  the substitution would result in an invalid type construction.

  Note that the order of the elements of the substitution pairs matches other 
  environments in the systems, such that for the pair @(A, B)@ @B@ will be 
  substituted for all instances of @A@.
-}
tysubst :: MonadCatch m => HOLTypeEnv -> HOLType -> m HOLType
tysubst env ty =
  assoc ty env <|> 
     case ty of
       TyVar{} -> return ty
       TyApp tycon tyvars -> 
         (tyApp tycon =<< mapM (tysubst env) tyvars) <?>
           "tysubst: bad type application"
       UType bv bod -> 
         (mkUType bv =<< tysubst (filter (\ (x, _) -> x /= bv) env) bod) <?>
           "tysubst: bad universal type"
       _ -> throwM $! HOLExhaustiveWarning "tysubst"

{-|
  Alpha conversion for universal types.  Renames a bound type variable to match
  the name of a provided type variable.  Fails with 'Left' in the following
  cases:

  * First type is not a small type variable.

  * Second type is not a universal type.

  * The type variable is free in the body of the universal type.
-}
alphaUtype :: MonadCatch m => HOLType -> HOLType -> m HOLType
alphaUtype tv@(TyVar True _) ty@(UType tv0 bod)
    | tv == tv0 = return ty
    | tv `elem` tyVars bod = 
        throwM $! HOLTypeError ty "alphaUtype: variable free in body of type."
    | otherwise =
        mkUType tv (typeSubst [(tv0, tv)] bod) <?>
          "alphaUtype: construction of universal type failed."
alphaUtype tv UType{} =
   throwM $! HOLTypeError tv "alphaUtype: first type not a small type variable."
alphaUtype _ ty = 
    throwM $! HOLTypeError ty "alphaUtype: not a universal type."

-- functions for manipulating terms
{-| 
  Safely creates an equality between two terms using the left hand side argument
  to perform the required instantiation.  Throws a 'HOLException' in the case 
  when the types of the two terms do not agree.
-}
mkEq :: MonadCatch m => HOLTerm -> HOLTerm -> m HOLTerm
mkEq l r = 
    (do eq <- mkComb (tmEq $ typeOf l) l
        mkComb eq r) <?> "mkEq"

{-| 
  Predicate to check if the first term is free in the second modulo
  alpha-equivalence.
-}
freeIn :: HOLTerm -> HOLTerm -> Bool
freeIn tm1 tm2 =
  (tm1 `aConv` tm2) ||
  (case tm2 of
     Comb l r -> freeIn tm1 l || freeIn tm1 r
     Abs bv bod -> not (varFreeIn bv tm1) && freeIn tm1 bod
     TyAbs bv bod -> bv `elem` typeVarsInTerm tm1 && freeIn tm1 bod
     TyComb tm _ -> freeIn tm1 tm
     _ -> False)

-- | Returns all variables in a term, free and bound both.
variables :: HOLTerm -> [HOLTerm]
variables = vars []
  where vars :: [HOLTerm] -> HOLTerm -> [HOLTerm]
        vars acc tm@Var{} = insert tm acc
        vars acc Const{} = acc
        vars acc (Abs v bod) = vars (insert v acc) bod
        vars acc (Comb l r) = vars (vars acc l) r
        vars _ _ = error "variables: exhaustive warning."

{-| 
  Basic term substitution.  
  Fails when the substitution would result in an invalid term construction.

  Note that the order of the elements of the substitution pairs matches other 
  environments in the systems, such that for the pair @(A, B)@ @B@ will be 
  substituted for all instances of @A@.  
-}
subst :: MonadThrow m => HOLTermEnv -> HOLTerm -> m HOLTerm
subst ilist tm =
  let (xs, ts) = unzip ilist
      gs = map (\ (n, x) -> mkVarInt n $ typeOf x) $ zip [0..] xs in
    do tm' <- ssubst (zip xs gs) tm
       if tm' == tm
          then return tm
          else varSubst (zip gs ts) tm'
  where ssubst :: MonadThrow m => HOLTermEnv -> HOLTerm -> m HOLTerm
        ssubst [] t = return t
        ssubst env t = 
          case find (\ (t', _) -> t `aConv` t') env of
            Just (_, res) -> return res
            Nothing ->
              case t of
                Comb f x -> 
                    do f' <- ssubst env f
                       mkComb f' =<< ssubst env x
                Abs bv bod -> 
                  mkAbs bv =<< 
                    ssubst (filter (not . varFreeIn bv . fst) env) bod
                TyAbs ty bod ->
                  mkTyAbs ty =<< 
                    ssubst (filter (\ (_, x) -> ty `notElem` 
                                                typeVarsInTerm x) env) bod
                TyComb bod ty ->
                    do bod' <- ssubst env bod
                       mkTyComb bod' ty
                _ -> return t

{-|
  Alpha conversion for term abstractions.  Renames a bound variable to match
  the name of a provided variable.  Fails with 'Left' in the following cases:

  * First term is not a variable.

  * Second term is not an abstraction.

  * The types of the variable and bound variable do no agree.

  * The variable is free in the body of the abstraction.
-}
alpha :: MonadCatch m => HOLTerm -> HOLTerm -> m HOLTerm
alpha v@(Var _ ty) tm@(Abs v0@(Var _ ty0) bod)
    | v == v0 = return tm
    | ty /= ty0 =
        throwM $! HOLTermError tm "alpha: types of variables not equal."
    | v `varFreeIn` bod =
        throwM $! HOLTermError tm "alpha: variable free in body of abstraction."
    | otherwise =
        (mkAbs v =<< varSubst [(v0, v)] bod) <?> 
          "alpha: construction of abstraction failed."
alpha v Abs{} = 
    throwM $! HOLTermError v "alpha: first term not a variable."
alpha _ tm = throwM $! HOLTermError tm "alpha: second term not an abstraction."

{-|
  Alpha conversion for type abstractions.  Renames a bound type variable to
  match the name of a provided type variable.  Fails with 'Left' in the 
  following cases:

  * The provided type is not a small type variable.

  * The provided term is not a type abstraction.

  * The type is free in the body of the type abstraction.
-}
alphaTyabs :: MonadThrow m => HOLType -> HOLTerm -> m HOLTerm
alphaTyabs ty@(TyVar True _) tm@(TyAbs ty0 bod)
    | ty == ty0 = return tm
    | ty `elem` typeVarsInTerm bod =
        throwM $! HOLTermError tm 
                    "alphaTyabs: type free in body of type abstraction."
    | otherwise = mkTyAbs ty $ inst [(ty0, ty)] bod
alphaTyabs ty TyAbs{} = 
    throwM $! HOLTypeError ty "alphaTyabs: type not a small type variable."
alphaTyabs _ tm = 
    throwM $! HOLTermError tm "alphaTyabs: term not a type abstraction."

-- searching for terms
{-| 
  Searches a term for a subterm that satisfies a given predicate.  
  Fails if no such term is found.
-}
findTerm :: MonadCatch m => (HOLTerm -> Bool) -> HOLTerm -> m HOLTerm
findTerm p tm
    | p tm = return tm
    | otherwise =
        case tm of
          Abs _ bod -> findTerm p bod
          Comb l r -> findTerm p l <|> findTerm p r
          TyAbs _ bod -> findTerm p bod
          TyComb tm' _ -> findTerm p tm'
          _ -> fail' "findTerm"

-- | The monadic version of 'findTerm'.
findTermM :: MonadCatch m => (HOLTerm -> m Bool) -> HOLTerm -> m HOLTerm
findTermM p tm =
    do c <- p tm 
       if c
          then return tm
          else case tm of
                 Abs _ bod -> findTermM p bod
                 Comb l r -> findTermM p l <|> findTermM p r
                 TyAbs _ bod -> findTermM p bod
                 TyComb tm' _ -> findTermM p tm'
                 _ -> fail' "findTermM"

-- | Searches a term for all unique subterms that satisfy a given predicate.
findTerms :: (HOLTerm -> Bool) -> HOLTerm -> [HOLTerm]
findTerms p = findRec []
  where findRec :: [HOLTerm] -> HOLTerm -> [HOLTerm]
        findRec tl tm =
            let tl' = if p tm then insert tm tl else tl in
              case tm of
                Abs _ bod -> findRec tl' bod
                Comb l r -> findRec (findRec tl' l) r
                TyAbs _ bod -> findRec tl' bod
                TyComb tm' _ -> findRec tl' tm'
                _ -> tl'

-- | The monadic version of 'findTerms'.
findTermsM :: forall m. MonadCatch m 
           => (HOLTerm -> m Bool) -> HOLTerm -> m [HOLTerm]
findTermsM p = findRec []
  where findRec :: [HOLTerm] -> HOLTerm -> m [HOLTerm]
        findRec tl tm =
            do c <- p tm <|> return False
               let tl' = if c then insert tm tl else tl
               case tm of
                 Abs _ bod -> findRec tl' bod
                 Comb l r -> 
                     do l' <- findRec tl' l
                        findRec l' r
                 TyAbs _ bod -> findRec tl' bod
                 TyComb tm' _ -> findRec tl' tm'
                 _ -> return tl'

-- Director strings down a term
{-|
  Searches a term for a subterm that satisfies a given predicate, returning
  a string that indicates the path to that subterm:

  * @\'b\'@ - Take the body of an abstraction.
  
  * @\'t\'@ - Take the body of a type abstraction.
  
  * @\'l\'@ - Take the left path in a term combination.
  
  * @\'r\'@ - Take the right path in a term combination.
  
  * @\'c\'@ - Take the body in a type combination.

  Fails with 'Nothing' if there is no satisfying subterm.
-}
findPath :: MonadCatch m => (HOLTerm -> Bool) -> HOLTerm -> m String
findPath p tm
    | p tm = return []
    | otherwise =
        case tm of
          Abs _ bod -> (:) 'b' `fmap` findPath p bod
          TyAbs _ bod -> (:) 't' `fmap` findPath p bod
          Comb l r -> (:) 'r' `fmap` findPath p r <|>
                      (:) 'l' `fmap` findPath p l
          TyComb bod _ -> (:) 'c' `fmap` findPath p bod
          _ -> fail' "findPath"

{-|
  Returns the subterm found by following a 'String' path as produced by 
  'findPath'.  Fails if the provided term does not a suitable 
  subterm for the given path.
-}
followPath :: MonadThrow m => String -> HOLTerm -> m HOLTerm
followPath [] tm = return tm
followPath ('l':t) (Comb l _) = followPath t l
followPath ('r':t) (Comb _ r) = followPath t r
followPath ('c':t) (TyComb tm _) = followPath t tm
followPath ('b':t) (Abs _ bod) = followPath t bod
followPath ('t':t) (TyAbs _ bod) = followPath t bod
followPath _ _ = fail' "followPath"

-- theorem manipulators
-- | Returns the list of all free type variables in a theorem.
typeVarsInThm :: HOLThm -> [HOLType]
typeVarsInThm (Thm asl c) =
    foldr (union . typeVarsInTerm) (typeVarsInTerm c) asl
typeVarsInThm _ = error "typeVarsInThm: exhaustive warning."

-- | Returns the list of all free term variables in a theorem.
thmFrees :: HOLThm -> [HOLTerm]
thmFrees (Thm asl c) = 
    foldr (union . frees) (frees c) asl
thmFrees _ = error "typeVarsInThm: exhaustive warning."

-- more syntax
{-|
  Constructs a complex combination that represents the application of a 
  function to a list of arguments.  Fails with 'Left' if any internal call to 
  'mkComb' fails.
-}
listMkComb :: MonadThrow m => HOLTerm -> [HOLTerm] -> m HOLTerm
listMkComb = foldlM mkComb

{-|
  Constructs a complex type combination that represents the application of a 
  term to a list of type arguments.  Fails with 'Left' if any internal call to 
  'mkComb' fails.
-}
listMkTyComb :: MonadThrow m => HOLTerm -> [HOLType] -> m HOLTerm
listMkTyComb = foldlM mkTyComb

{-|
  Constructs a complex abstraction that represents a term with multiple
  bound variables.  Fails with 'Left' if any internal call to 'mkAbs' fails.
-}
listMkAbs :: MonadThrow m => [HOLTerm] -> HOLTerm -> m HOLTerm
listMkAbs = flip (foldrM mkAbs)

{-|
  Constructs a complex type abstraction that represents a term with multiple
  bound variables.  Fails with 'Left' if any internal call to 'mkAbs' fails.
-}
listMkTyAbs :: MonadThrow m => [HOLType] -> HOLTerm -> m HOLTerm
listMkTyAbs = flip (foldrM mkTyAbs)

-- Useful function to create stylized arguments using numbers
{-|
  Constructs a list of term variables of a given prefix.  Names are adjusted
  as necessary with 'variant' to avoid clashing with the provided list of term
  variables.  The number and types of the resultant variables is directed by 
  the provided list of types, i.e.

  > mkArgs "x" avoids [ty1, ... tyn] === [x1:ty1, ..., xn:tyn] where {x1, ..., xn} are not elements of avoids
-}
mkArgs :: Text -> [HOLTerm] -> [HOLType] -> [HOLTerm]
mkArgs s avoid [ty] = [variant avoid $ mkVar s ty]
mkArgs s avoid tys = mkRec 0 s avoid tys
  where mkRec :: Int -> Text -> [HOLTerm] -> [HOLType] -> [HOLTerm]
        mkRec _ _ _ [] = []
        mkRec n x avs (y:ys) =
          let v' = variant avs $ mkVar (x `append` pack (show n)) y
              vs = mkRec (n + 1) x (v':avs) ys in
            (v':vs)

{-| 
  Returns the left term of a combination.  Fails with 'Nothing' if the provided
  term is not a combination.
-}
rator :: MonadThrow m => HOLTerm -> m HOLTerm
rator (Comb l _) = return l
rator tm = throwM $! HOLTermError tm "rator"

{-|
  Returns the right term of a combination.  Fails with 'Nothing' if the provided
  term is not a combination.
-}
rand :: MonadThrow m => HOLTerm -> m HOLTerm
rand (Comb _ r) = return r
rand tm = throwM $! HOLTermError tm "rand"

{-|
  Returns the bound term of an abstraction.  Fails with 'Nothing' if the
  provided term is not an abstraction.
-}
bndvar :: MonadThrow m => HOLTerm -> m HOLTerm
bndvar (Abs bv _) = return bv
bndvar tm = throwM $! HOLTermError tm "bndvar"

{-|
  Returns the body term of an abstraction.  Fails with 'Nothing' if the
  provided term is not an abstraction.
-}
body :: MonadThrow m => HOLTerm -> m HOLTerm
body (Abs _ bod) = return bod
body tm = throwM $! HOLTermError tm "body"

{-|
  Returns the bound type of a type abstraction.  Fails with 'Nothing' if the
  provided term is not a type abstraction.
-}
bndvarTyabs :: MonadThrow m => HOLTerm -> m HOLType
bndvarTyabs (TyAbs bv _) = return bv
bndvarTyabs tm = throwM $! HOLTermError tm "bndvarTyabs"

{-|
  Returns the body term of a type abstraction.  Fails with 'Nothing' if the
  provided term is not a type abstraction.
-}
bodyTyabs :: MonadThrow m => HOLTerm -> m HOLTerm
bodyTyabs (TyAbs _ bod) = return bod
bodyTyabs tm = throwM $! HOLTermError tm "bodyTyabs"

{-|
  Destructs a complex combination returning its function term and its list of
  argument terms.
-}
stripComb :: HOLTerm -> (HOLTerm, [HOLTerm])
stripComb = revSplitList destComb

{-|
  Destructs a complex type combination returning its function term and its 
  list of argument types.
-}
stripTyComb :: HOLTerm -> (HOLTerm, [HOLType])
stripTyComb = revSplitList destTyComb

{-|
  Destructs a complex abstraction returning its list of bound variables and its
  body term.
-}
stripAbs :: HOLTerm -> ([HOLTerm], HOLTerm)
stripAbs = splitList destAbs

{-|
  Destructs a complex type abstraction returning its list of bound variables and
  its body term.
-}
stripTyAbs :: HOLTerm -> ([HOLType], HOLTerm)
stripTyAbs = splitList destTyAbs

{-|
  A version of 'mkComb' that instantiates the type variables in the left hand
  argument.  Relies internally on 'typeMatch' in order to provide a match
  between the domain type of the function and the type of the argument.  Fails
  with 'Nothing' if instantiation is impossible.
-}
mkIComb :: MonadCatch m => HOLTerm -> HOLTerm -> m HOLTerm
mkIComb tm1 tm2 =
    do (ty, _) <- destFunTy $ typeOf tm1
       mat <- typeMatch ty (typeOf tm2) ([], [], [])
       mkComb (instFull mat tm1) tm2
                                          
-- syntax for binary operators
{-| 
  Predicate that tests if a term is a binary application whose operator has the
  given name.
-}
isBinary :: Text -> HOLTerm -> Bool
isBinary s (Comb (Comb (Const s' _) _) _) = s == s'
isBinary _ _ = False

-- | A version of 'isBinary' that tests for operator terms, not strings.
isBinop :: HOLTerm -> HOLTerm -> Bool
isBinop op (Comb (Comb op' _) _) = op' == op
isBinop _ _ = False

{-|
  Destructs a binary application returning its left and right arguments.  
  Fails if the provided term is not a binary application with the 
  specified operator name.
-}
destBinary :: MonadThrow m => Text -> HOLTerm -> m (HOLTerm, HOLTerm)
destBinary s tm@(Binary s' l r)
    | s == s' = return (l, r)
    | otherwise = 
        throwM $! HOLTermError tm "destBinary: operator does not match."
destBinary _ tm = 
    throwM $! HOLTermError tm "destBinary: not a binary application."

-- | The pattern synonym equivalent of 'destBinary'.
pattern Binary :: Text -> HOLTerm -> HOLTerm -> HOLTerm
pattern Binary s l r <- Comb (Comb (Const s _) l) r

-- | A version of 'destBinary' that tests for operator terms, not strings.
destBinop :: MonadThrow m => HOLTerm -> HOLTerm -> m (HOLTerm, HOLTerm)
destBinop op tm@(Binop op' l r)
    | op' == op = return (l, r)
    | otherwise = 
        throwM $! HOLTermError tm "destBinop: operator does not match."
destBinop _ tm = throwM $! HOLTermError tm "destBinop: not a binop application."

-- | The pattern synonym equivalent of 'destBinop'.
pattern Binop :: HOLTerm -> HOLTerm -> HOLTerm -> HOLTerm
pattern Binop op l r <- Comb (Comb op l) r

{-| 
  A version of 'mkBinary' that accepts the operator as a pre-constructed term.
-}
mkBinop :: MonadCatch m => HOLTerm -> HOLTerm -> HOLTerm -> m HOLTerm
mkBinop op tm1 tm2 = 
    (do tm <- mkComb op tm1
        mkComb tm tm2) <?> "mkBinop"

{-| 
  Iteratively builds a complex combination using 'mkBinop', i.e.
 
  > listMkBinop (/\) [T, F, T] === T /\ F /\ T
-} 
listMkBinop :: MonadCatch m => HOLTerm -> [HOLTerm] -> m HOLTerm
listMkBinop op = foldr1M (mkBinop op)

{-|
  The inverse of 'listMkBinop'.  Destructs a complex combination built with
  a binary operator into its list of arguments.
-}
binops :: HOLTerm -> HOLTerm -> [HOLTerm]
binops = stripList . destBinop
                      
-- | Predicate for generalized abstractions.  See 'mkGAbs' for more details.
isGAbs :: HOLTerm -> Bool
isGAbs = test' . destGAbs

-- | Predicate that tests if a term is an abstraction of specified binder name.
isBinder :: Text -> HOLTerm -> Bool
isBinder s (Comb (Const s' _) Abs{}) = s == s'
isBinder _ _ = False

{-| 
  Predicate that tests if a term is an abtraction of a specified type binder
  name.
-}
isTyBinder :: Text -> HOLTerm -> Bool
isTyBinder s (Comb (Const s' _) TyAbs{}) = s == s'
isTyBinder _ _ = False

{-| 
  Destructor for generalized abstractions.  Fails with 'Nothing' if the provided
  term is not an abstraction or generalized abstraction.  See 'mkGAbs' for more 
  details.
-}
destGAbs :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destGAbs tm@Abs{} = destAbs tm
destGAbs (Comb (Const "GABS" _) (Abs _ bod)) =
    do (ltm, rtm) <- destBinary "GEQ" . snd $ stripForall bod
       ltm' <- rand ltm
       return (ltm', rtm)
destGAbs tm = throwM $! HOLTermError tm "destGAbs" 

{-|
  Destructs an abstraction of specified binder name into its bound variable and
  its body term.  Fails with 'Nothing' if the provided term is not an
  abstraction with the specified binder name.
-}
destBinder :: MonadThrow m => Text -> HOLTerm -> m (HOLTerm, HOLTerm)
destBinder s tm@(Bind s' bv t)
    | s == s' = return (bv, t)
    | otherwise = 
        throwM $! HOLTermError tm "destBinder: binder does not match."
destBinder _ tm = 
    throwM $! HOLTermError tm "destBinder: not a binder application." 

-- | The pattern synonym equivalent of 'destBinder'.
pattern Bind :: Text -> HOLTerm -> HOLTerm -> HOLTerm
pattern Bind s bv tm <- Comb (Const s _) (Abs bv tm)

{-|
  A version of 'destBinder' that returns the body abstraction in its entirety.
-}
destBinderWhole :: MonadThrow m => Text -> HOLTerm -> m HOLTerm
destBinderWhole s (BindWhole s' tm@Abs{})
    | s == s' = return tm
    | otherwise =
        throwM $! HOLTermError tm "destBinderWhole: binder does not match."
destBinderWhole _ tm =
    throwM $! HOLTermError tm "destBinderWhole: not a binder application."

-- | A version of 'Bind' that returns the body abstraction in its entirety.
pattern BindWhole :: Text -> HOLTerm -> HOLTerm
pattern BindWhole s tm <- Comb (Const s _) tm

{-|
  Destructs a type abstraction of specified binder name into its bound type
  variable and its body term.  Fails with 'Nothing' if the provided term is not
  a type abstraction with the specified type binder name.
-}
destTyBinder :: MonadThrow m => Text -> HOLTerm -> m (HOLType, HOLTerm)
destTyBinder s tm@(TyBind s' bv t)
    | s == s' = return (bv, t)
    | otherwise = 
        throwM $! HOLTermError tm "destTyBinder: binder does not match."
destTyBinder _ tm = 
    throwM $! HOLTermError tm "destTyBinder: not a type binder application."

-- | The pattern synonym equivalent of 'destTyBinder'.
pattern TyBind :: Text -> HOLType -> HOLTerm -> HOLTerm
pattern TyBind s ty tm <- Comb (Const s _) (TyAbs ty tm)

{-|
  A version of 'destTyBinder' that returns the body abstraction in its entirety.
-}
destTyBinderWhole :: MonadThrow m => Text -> HOLTerm -> m HOLTerm
destTyBinderWhole s (TyBindWhole s' tm@TyAbs{})
    | s == s' = return tm
    | otherwise =
        throwM $! HOLTermError tm "destTyBinderWhole: binder does not match."
destTyBinderWhole _ tm =
   throwM $! HOLTermError tm "destTyBinderWhole: not a type binder application."

-- | A version of 'TyBind' that returns the body abstraction in its entirety.
pattern TyBindWhole :: Text -> HOLTerm -> HOLTerm
pattern TyBindWhole s tm <- Comb (Const s _) tm

-- | A specific version of 'stripAbs' for general abstractions.
stripGAbs :: HOLTerm -> ([HOLTerm], HOLTerm)
stripGAbs = splitList destGAbs

-- common special cases of binary ops
-- | Predicate for biconditionals.
isIff :: HOLTerm -> Bool
isIff (Comb (Comb (Const "=" (TyBool :-> _)) _) _) = True
isIff _ = False

-- | Predicate for boolean conjunctions.
isConj :: HOLTerm -> Bool
isConj = isBinary "/\\"

-- | Predicate for boolean implications.
isImp :: HOLTerm -> Bool
isImp = isBinary "==>"

-- | Predicate for universal term quantification.
isForall :: HOLTerm -> Bool
isForall = isBinder "!"

-- | Predicate for existential term quantification.
isExists :: HOLTerm -> Bool
isExists = isBinder "?"

-- | Predicate for boolean disjunctions.
isDisj :: HOLTerm -> Bool
isDisj = isBinary "\\/"

-- | Predicate for boolean negations.
isNeg :: HOLTerm -> Bool
isNeg (Comb (Const "~" _) _) = True
isNeg _ = False

-- | Predicate for unique, existential quantification.
isUExists :: HOLTerm -> Bool
isUExists = isBinder "?!"

-- | Predicate for term-level universal type quantification.
isTyAll :: HOLTerm -> Bool
isTyAll = isTyBinder "!!"

-- | Predicate for term-level existential type quantification.
isTyEx :: HOLTerm -> Bool
isTyEx = isTyBinder "??"

-- | Destructor for biconditionals.
destIff :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destIff (Comb (Comb (Const "=" (TyBool :-> _)) l) r) = return (l, r)
destIff tm = throwM $! HOLTermError tm "destIff"

-- | The pattern synonym equivalent of 'destIff'.
pattern (:<=>) :: HOLTerm -> HOLTerm -> HOLTerm
pattern l :<=> r <- Comb (Comb (Const "=" (TyBool :-> _)) l) r

-- | Destructor for boolean conjunctions.
destConj :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm) 
destConj = destBinary "/\\"

-- | The pattern synonym equivalent of 'destConj'.
pattern (:/\) :: HOLTerm -> HOLTerm -> HOLTerm
pattern l :/\ r <- Binary "/\\" l r

-- | Destructor for boolean implications.
destImp :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destImp = destBinary "==>"

-- | The pattern synonym equivalent of 'destImp'.
pattern (:==>) :: HOLTerm -> HOLTerm -> HOLTerm
pattern l :==> r <- Binary "==>" l r

-- | Destructor for universal term quantification.
destForall :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destForall = destBinder "!"

-- | The pattern synonym equivalent of 'destForall'.
pattern Forall :: HOLTerm -> HOLTerm -> HOLTerm
pattern Forall bv tm <- Bind "!" bv tm

-- | Destructor for existential term quantification.
destExists :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destExists = destBinder "?"

-- | The pattern synonym equivalent of 'destExists'.
pattern Exists :: HOLTerm -> HOLTerm -> HOLTerm
pattern Exists bv tm <- Bind "?" bv tm

-- | Destructor for boolean disjunctions.
destDisj :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destDisj = destBinary "\\/"

-- | The pattern synonym equivalent of 'destDisj'.
pattern (:\/) :: HOLTerm -> HOLTerm -> HOLTerm
pattern l :\/ r <- Binary "\\/" l r

-- | Destructor for boolean negations.
destNeg :: MonadThrow m => HOLTerm -> m HOLTerm
destNeg (Comb (Const "~" _) p) = return p
destNeg tm = throwM $! HOLTermError tm "destNeg"

-- | The pattern synonym equivalent of 'destNeg'.
pattern Neg :: HOLTerm -> HOLTerm
pattern Neg tm <- (Comb (Const "~" _) tm)

-- | Destructor for unique, existential quantification.
destUExists :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destUExists = destBinder "?!"

-- | The pattern synonym equivalent of 'destUExists'.
pattern UExists :: HOLTerm -> HOLTerm -> HOLTerm
pattern UExists bv tm <- Bind "?!" bv tm

-- | Destructor for term-level universal type quantification.
destTyAll :: MonadThrow m => HOLTerm -> m (HOLType, HOLTerm)
destTyAll = destTyBinder "!!"

-- | The pattern synonym equivalent of 'destTyAll'.
pattern TyAll :: HOLType -> HOLTerm -> HOLTerm
pattern TyAll ty tm <- TyBind "!!" ty tm

-- | Destructor for term-level existential type quantification.
destTyEx :: MonadThrow m => HOLTerm -> m (HOLType, HOLTerm)
destTyEx = destTyBinder "??"

-- | The pattern synonym equivalent of 'destTyEx'.
pattern TyEx :: HOLType -> HOLTerm -> HOLTerm
pattern TyEx ty tm <- TyBind "??" ty tm

-- | Returns the list of propositions in a complex conjunction.
conjuncts :: HOLTerm -> [HOLTerm]
conjuncts = stripList destConj

-- | Returns the list of propositions in a complex disjunction.
disjuncts :: HOLTerm -> [HOLTerm]
disjuncts = stripList destDisj

-- | A specific version of 'stripAbs' for universal term quantification.
stripForall :: HOLTerm -> ([HOLTerm], HOLTerm)
stripForall = splitList destForall

-- | A specific version of 'stripAbs' for existential term quantification.
stripExists :: HOLTerm -> ([HOLTerm], HOLTerm)
stripExists = splitList destExists

{-| 
  A specific version of 'stripAbs' for term-level universal type quantification.
-}
stripTyAll :: HOLTerm -> ([HOLType], HOLTerm)
stripTyAll = splitList destTyAll

{-| 
  A specific version of 'stripAbs' for term-level existential type 
  quantification.
-}
stripTyEx :: HOLTerm -> ([HOLType], HOLTerm)
stripTyEx = splitList destTyEx

-- syntax for other terms
-- | Predicate for list @CONS@.
isCons :: HOLTerm -> Bool
isCons = isBinary "CONS"

-- | Predicate for list terms.
isList :: HOLTerm -> Bool
isList = test' . destList

-- | Predicate for let binding terms.
isLet :: HOLTerm -> Bool
isLet = test' . destLet

-- | Destructor for list @CONS@.
destCons :: MonadThrow m => HOLTerm -> m (HOLTerm, HOLTerm)
destCons = destBinary "CONS"

{-|
  Destructor for list terms.  Returns a list of the elements in the term.  Fails
  with 'Nothing' if internall the term is not of the form

  > x1 `CONS` .... xn `CONS` NIL
-}
destList :: MonadCatch m => HOLTerm -> m [HOLTerm]
destList tm =
    do (tms, nil) <- splitListM destCons tm 
       case nil of
         (Const "NIL" _) -> return tms
         _ -> throwM $! HOLTermError tm "destList"

{-|
  Destructs a let binding term into a list of its name and value pairs and its
  body term.  Fails with 'Nothing' if internally the term is not of the form

  > LET (x1, v1) ... (xn, vn) LET_END
-}
destLet :: MonadThrow m => HOLTerm -> m ([(HOLTerm, HOLTerm)], HOLTerm)
destLet tm =
  case stripComb tm of
    (Const "LET" _, a:args) ->
      let (vars, lebod) = stripGAbs a
          eqs = zip vars args in
        case lebod of
          Comb (Const "LET_END" _) bod -> return (eqs, bod)
          _ -> throwM $! HOLTermError tm "destLet: missing LET_END."
    _ -> throwM $! HOLTermError tm "destLet: missing LET."

{-|
  Converts a numeral term to an 'Integer'.  
  Fails if internally the term is not of the form

  > NUMERAL bits _0, where bits is a series of BIT0 and BIT1 terms  
-} 
destNumeral :: MonadThrow m => HOLTerm -> m Integer
destNumeral (Comb (Const "NUMERAL" _) r) = destNum r
  where destNum :: (MonadThrow m, Num a) => HOLTerm -> m a
        destNum (Const "_0" _) = return 0
        destNum (Comb (Const "BIT0" _) r') =
          (2 *) `fmap` destNum r'
        destNum (Comb (Const "BIT1" _) r') =
          (\ x -> 1 + 2 * x) `fmap` destNum r'
        destNum tm = throwM $! HOLTermError tm "destNum"
destNumeral tm = throwM $! HOLTermError tm "destNumeral"
