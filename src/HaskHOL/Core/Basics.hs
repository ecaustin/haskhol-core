{-# LANGUAGE PatternSynonyms, ScopedTypeVariables #-}

{-|
  Module:    HaskHOL.Core.Basics
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines common utility functions that depend on data types
  introduced by HaskHOL. See the "HaskHOL.Core.Lib" module for utility functions
  that do not have this dependence.
-}
module HaskHOL.Core.Basics
    ( -- * Variable Term and Type Generation
      genVarWithName
    , genVar
    , genSmallTyVar
      -- * Common Type Functions
    , occursIn
    , tysubst
    , alphaUtype
      -- * Common Term Functions
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
    , typeMatch
    , mkMConst
    , mkIComb
    , listMkIComb
      -- * Predicates, Constructors, and Destructors for Binary Terms
    , isBinary 
    , isBinop
    , destBinary
    , pattern Binary
    , destBinop
    , mkBinary
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
    , destTyBinder
    , pattern TyBind
    , mkGAbs
    , mkBinder
    , mkTyBinder
    , listMkGAbs
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
    , mkIff
    , mkConj
    , mkImp
    , mkForall
    , mkExists
    , mkDisj
    , mkNeg
    , mkUExists
    , mkTyAll
    , mkTyEx
    , listMkConj
    , listMkDisj
    , listMkForall
    , listMkExists
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
    , mkLet
    , destNumeral
      -- * Term Nets
    , module HaskHOL.Core.Basics.Nets
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics.Nets

-- Term and Type Generation
{-|  
  Generates a new term variable consisting of a given prefix and the next value
  in the fresh term counter.
-}
genVarWithName :: Text -> HOLType -> HOL cls thry HOLTerm
genVarWithName n ty =
    do count <- tickTermCounter
       return $! mkVar (n `append` pack (show count)) ty

-- | A version of 'genVarWithName' that defaults to the prefix \"_\".
genVar :: HOLType -> HOL cls thry HOLTerm
genVar = genVarWithName "_"

{-|
  Generates a new small, type variable with a name built using the fresh type
  counter.
-}
genSmallTyVar :: HOL cls thry HOLType
genSmallTyVar =
    do count <- tickTypeCounter
       liftEither "genSmallTyVar" . mkSmall $ mkVarType (pack $ '_':show count)

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
tysubst :: HOLTypeEnv -> HOLType -> Either String HOLType
tysubst env ty =
  note "tysubst" (lookup ty env) 
  <|> case ty of
        TyVar{} -> return ty
        TyApp tycon tyvars -> 
          (tyApp tycon =<< mapM (tysubst env) tyvars) <?>
            "tysubst: bad type application"
        UType bv bod -> 
          (mkUType bv =<< tysubst (filter (\ (x, _) -> x /= bv) env) bod) <?>
            "tysubst: bad universal type"
        _ -> error "tysubst: exhaustive warning."

{-|
  Alpha conversion for universal types.  Renames a bound type variable to match
  the name of a provided type variable.  Fails with 'Left' in the following
  cases:

  * First type is not a small type variable.

  * Second type is not a universal type.

  * The type variable is free in the body of the universal type.
-}
alphaUtype :: HOLType -> HOLType -> Either String HOLType
alphaUtype tv@(TyVar True _) ty@(UType tv0 bod)
    | tv == tv0 = Right ty
    | tv `elem` tyVars bod = Left "alphaUtype: variable free in body of type."
    | otherwise = mkUType tv (typeSubst [(tv0, tv)] bod) <?>
                    "alphaUtype: construction of universal type failed."
alphaUtype _ UType{} = 
  Left "alphaUtype: first type not a small type variable."
alphaUtype _ _ = Left "alphaUtype: second type not a universal type."

-- functions for manipulating terms
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
  Basic term substitution.  Throws a 'HOLException' when the substitution would 
  result in an invalid term construction.

  Note that the order of the elements of the substitution pairs matches other 
  environments in the systems, such that for the pair @(A, B)@ @B@ will be 
  substituted for all instances of @A@.  
-}
subst :: HOLTermEnv -> HOLTerm -> Maybe HOLTerm
subst ilist tm =
    let (xs, ts) = unzip ilist
        gs = map (unsafeGenVar . typeOf) xs in
      do tm' <- hush $ ssubst (zip xs gs) tm
         if tm' == tm
            then return tm
            else varSubst (zip gs ts) tm'
  where ssubst :: HOLTermEnv -> HOLTerm -> Either String HOLTerm
        ssubst [] t = Right t
        ssubst env t = 
          case find (\ (t', _) -> t `aConv` t') env of
            Just (_, res) -> Right res
            Nothing ->
              case t of
                Comb f x -> 
                  liftM1 mkComb (ssubst env f) =<< ssubst env x
                Abs bv bod -> 
                  mkAbs bv =<< 
                    ssubst (filter (not . varFreeIn bv . fst) env) bod
                TyAbs ty bod ->
                  mkTyAbs ty =<< 
                    ssubst (filter (\ (_, x) -> ty `notElem` 
                                                typeVarsInTerm x) env) bod
                TyComb bod ty ->
                  liftM1 mkTyComb (ssubst env bod) ty
                _ -> Right t

{-|
  Alpha conversion for term abstractions.  Renames a bound variable to match
  the name of a provided variable.  Fails with 'Left' in the following cases:

  * First term is not a variable.

  * Second term is not an abstraction.

  * The types of the variable and bound variable do no agree.

  * The variable is free in the body of the abstraction.
-}
alpha :: HOLTerm -> HOLTerm -> Either String HOLTerm
alpha v@(Var _ ty) tm@(Abs v0@(Var _ ty0) bod)
    | v == v0 = Right tm
    | ty /= ty0 = Left "alpha: types of variables not equal."
    | v `varFreeIn` bod = Left "alpha: variable free in body of abstraction."
    | otherwise = (mkAbs v #<< varSubst [(v0, v)] bod) <?> 
                    "alpha: construction of abstraction failed."
alpha _ Abs{} = Left "alpha: first term not a variable."
alpha _ _ = Left "alpha: second term not an abstraction."

{-|
  Alpha conversion for type abstractions.  Renames a bound type variable to
  match the name of a provided type variable.  Fails with 'Left' in the 
  following cases:

  * The provided type is not a small type variable.

  * The provided term is not a type abstraction.

  * The type is free in the body of the type abstraction.
-}
alphaTyabs :: HOLType -> HOLTerm -> Either String HOLTerm
alphaTyabs ty@(TyVar True _) tm@(TyAbs ty0 bod)
    | ty == ty0 = Right tm
    | ty `elem` typeVarsInTerm bod = 
        Left "alphaTyabs: type free in body of type abstraction."
    | otherwise = mkTyAbs ty $ inst [(ty0, ty)] bod
alphaTyabs _ TyAbs{} = 
    Left "alphaTyabs: type not a small type variable."
alphaTyabs _ _ = 
    Left "alphaTyabs: term not a type abstraction."

-- searching for terms
{-| 
  Searches a term for a subterm that satisfies a given predicate.  Fails with
  'Nothing' if no such term is found.
-}
findTerm :: (HOLTerm -> Bool) -> HOLTerm -> Maybe HOLTerm
findTerm p tm
    | p tm = Just tm
    | otherwise =
        case tm of
          Abs _ bod -> findTerm p bod
          Comb l r -> findTerm p l <|> findTerm p r
          TyAbs _ bod -> findTerm p bod
          TyComb tm' _ -> findTerm p tm'
          _ -> Nothing

-- | The monadic version of 'findTerm'.
findTermM :: (Alternative m, Monad m) => (HOLTerm -> m Bool) -> HOLTerm 
          -> m HOLTerm
findTermM p tm =
    do c <- p tm 
       if c
          then return tm
          else case tm of
                 Abs _ bod -> findTermM p bod
                 Comb l r -> findTermM p l <|> findTermM p r
                 TyAbs _ bod -> findTermM p bod
                 TyComb tm' _ -> findTermM p tm'
                 _ -> fail "findTermM"

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
findTermsM :: forall m. (Alternative m, Monad m) => (HOLTerm -> m Bool) 
           -> HOLTerm -> m [HOLTerm]
findTermsM p = findRec []
  where findRec :: (Alternative m, Monad m) => [HOLTerm] -> HOLTerm 
                -> m [HOLTerm]
        findRec tl tm =
            do c <- (p tm <|> return False)
               let tl' = if c then insert tm tl else tl
               case tm of
                 Abs _ bod -> findRec tl' bod
                 Comb l r -> liftM1 findRec (findRec tl' l) r
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
findPath :: (HOLTerm -> Bool) -> HOLTerm -> Maybe String
findPath p tm
    | p tm = Just []
    | otherwise =
        case tm of
          Abs _ bod -> liftM ((:) 'b') $ findPath p bod
          TyAbs _ bod -> liftM ((:) 't') $ findPath p bod
          Comb l r -> liftM ((:) 'r') (findPath p r) <|>
                      liftM ((:) 'l') (findPath p l)
          TyComb bod _ -> liftM ((:) 'c') $ findPath p bod
          _ -> Nothing

{-|
  Returns the subterm found by following a 'String' path as produced by 
  'findPath'.  Fails with 'Nothing' if the provided term does not a suitable 
  subterm for the given path.
-}
followPath :: String -> HOLTerm -> Maybe HOLTerm
followPath [] tm = Just tm
followPath ('l':t) (Comb l _) = followPath t l
followPath ('r':t) (Comb _ r) = followPath t r
followPath ('c':t) (TyComb tm _) = followPath t tm
followPath ('b':t) (Abs _ bod) = followPath t bod
followPath ('t':t) (TyAbs _ bod) = followPath t bod
followPath _ _ = Nothing

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
listMkComb :: HOLTerm -> [HOLTerm] -> Either String HOLTerm
listMkComb = foldlM mkComb

{-|
  Constructs a complex type combination that represents the application of a 
  term to a list of type arguments.  Fails with 'Left' if any internal call to 
  'mkComb' fails.
-}
listMkTyComb :: HOLTerm -> [HOLType] -> Either String HOLTerm
listMkTyComb = foldlM mkTyComb

{-|
  Constructs a complex abstraction that represents a term with multiple
  bound variables.  Fails with 'Left' if any internal call to 'mkAbs' fails.
-}
listMkAbs :: [HOLTerm] -> HOLTerm -> Either String HOLTerm
listMkAbs = flip (foldrM mkAbs)

{-|
  Constructs a complex type abstraction that represents a term with multiple
  bound variables.  Fails with 'Left' if any internal call to 'mkAbs' fails.
-}
listMkTyAbs :: [HOLType] -> HOLTerm -> Either String HOLTerm
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
mkArgs s avoid (ty:[]) = [variant avoid $ mkVar s ty]
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
rator :: HOLTerm -> Maybe HOLTerm
rator (Comb l _) = Just l
rator _ = Nothing

{-|
  Returns the right term of a combination.  Fails with 'Nothing' if the provided
  term is not a combination.
-}
rand :: HOLTerm -> Maybe HOLTerm
rand (Comb _ r) = Just r
rand _ = Nothing

{-|
  Returns the bound term of an abstraction.  Fails with 'Nothing' if the
  provided term is not an abstraction.
-}
bndvar :: HOLTerm -> Maybe HOLTerm
bndvar (Abs bv _) = Just bv
bndvar _ = Nothing

{-|
  Returns the body term of an abstraction.  Fails with 'Nothing' if the
  provided term is not an abstraction.
-}
body :: HOLTerm -> Maybe HOLTerm
body (Abs _ bod) = Just bod
body _ = Nothing

{-|
  Returns the bound type of a type abstraction.  Fails with 'Nothing' if the
  provided term is not a type abstraction.
-}
bndvarTyabs :: HOLTerm -> Maybe HOLType
bndvarTyabs (TyAbs bv _) = Just bv
bndvarTyabs _ = Nothing

{-|
  Returns the body term of a type abstraction.  Fails with 'Nothing' if the
  provided term is not a type abstraction.
-}
bodyTyabs :: HOLTerm -> Maybe HOLTerm
bodyTyabs (TyAbs _ bod) = Just bod
bodyTyabs _ = Nothing

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

-- type matching
{-|
  Computes a tiplet of substitution environments that can be used to make two
  types match.  The triplet argument can be used to constrain the match, or
  its three environments can be left empty to find the most general match.
  Fails with 'Nothing' in the event that a match cannot be found that satisfies
  the provided constraint.
-}
typeMatch :: HOLType -> HOLType -> SubstTrip -> Maybe SubstTrip
typeMatch vty cty sofar =
    return snd <*> typeMatchRec vty cty ([], sofar)
  where typeMatchRec :: HOLType -> HOLType -> ([HOLType], SubstTrip) ->
                        Maybe ([HOLType], SubstTrip)
        typeMatchRec v@TyVar{} c acc@(env, (sfar, opTys, opOps))
            | v `elem` env = Just acc
            | otherwise =
                case lookup v sfar of
                  Just c'
                    | c' == c -> Just acc
                    | otherwise -> Nothing
                  Nothing -> Just (env, ((v, c):sfar, opTys, opOps))
        typeMatchRec (UType tvv varg) 
                     (UType tvc carg) (env, sfar) =
            let carg' = if tvv == tvc then carg 
                        else typeSubst [(tvc, tvv)] carg in
              typeMatchRec varg carg' (tvv:env, sfar)
        typeMatchRec (TyApp vop vargs) 
                     (TyApp cop cargs) acc@(env, (sfar, opTys, opOps))
            | vop == cop = foldr2M typeMatchRec acc vargs cargs
            | isTypeOpVar vop && isTypeOpVar cop =
                do copTy <- hush . uTypeFromTypeOpVar cop $ length cargs
                   case lookup vop opTys of
                     Just cop'
                         | cop' == copTy -> 
                             foldr2M typeMatchRec acc vargs cargs
                         | otherwise -> Nothing
                     Nothing -> 
                         foldr2M typeMatchRec 
                           (env, (sfar, (vop, copTy):opTys, opOps)) vargs cargs 
            | isTypeOpVar vop =
                case lookup vop opOps of
                  Just cop'
                    | cop' == cop -> foldr2M typeMatchRec acc vargs cargs
                    | otherwise -> Nothing
                  Nothing -> 
                      foldr2M typeMatchRec 
                        (env, (sfar, opTys, (vop, cop):opOps)) vargs cargs
            | otherwise = Nothing
        typeMatchRec _ _ _ = Nothing

-- matching version of mkConst
{-|
  Constructs an instance of a constant of the provided name and type.  Relies
  internally on 'typeMatch' in order to provide a match between the most general
  type of the constant and the provided type.  Throws a 'HOLException' in the
  following cases:

  * The provided string is not the name of a defined constant.

  * Type matching fails.
-}
mkMConst :: Text -> HOLType -> HOL cls thry HOLTerm
mkMConst name ty = 
  do uty <- getConstType name <?> "mkMConst: not a constant name"
     (mkConstFull name . fromJust $ typeMatch uty ty ([], [], [])) <?>
       "mkMConst: generic type cannot be instantiated"

{-|
  A version of 'mkComb' that instantiates the type variables in the left hand
  argument.  Relies internally on 'typeMatch' in order to provide a match
  between the domain type of the function and the type of the argument.  Fails
  with 'Nothing' if instantiation is impossible.
-}
mkIComb :: HOLTerm -> HOLTerm -> Maybe HOLTerm
mkIComb tm1 tm2 =
    do (ty, _) <- destFunTy $ typeOf tm1
       mat <- typeMatch ty (typeOf tm2) ([], [], [])
       hush $ mkComb (instFull mat tm1) tm2

{-|
  An iterative version of 'mkIComb' that builds a complex combination given a
  constant name and a list of arguments, attempting to find a correct
  instantiation at every step.  Throws a 'HOLException' in the following cases:

  * The provided name is not a currently defiend constant.

  * Any internal call to mkIComb fails.
-}
listMkIComb :: Text -> [HOLTerm] -> HOL cls thry HOLTerm
listMkIComb cname args =
    do cnst <- mkConst cname ([]::HOLTypeEnv) <?> 
                 "listMkIComb: not a constant name"
       liftMaybe "listMkIComb: type cannot be instantiated" $
         foldlM mkIComb cnst args
                                          
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
  Destructs a binary application returning its left and right arguments.  Fails 
  with 'Nothing' if the provided term is not a binary application with the 
  specified operator name.
-}
destBinary :: Text -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
destBinary s (Comb (Comb (Const s' _) l) r)
    | s == s' = Just (l, r)
    | otherwise = Nothing
destBinary _ _ = Nothing

-- | The pattern synonym equivalent of 'destBinary'.
pattern Binary s l r <- Comb (Comb (Const s _) l) r

-- | A version of 'destBinary' that tests for operator terms, not strings.
destBinop :: HOLTerm -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
destBinop op (Comb (Comb op' l) r)
    | op' == op = Just (l, r)
    | otherwise = Nothing
destBinop _ _ = Nothing

{-|
  Constructs a binary application given a constant name and two argument terms.
  Note that no instantiation is performed, thus the constant must be monomorphic
  or the provided arguments must match the constant's general type.  Throws a
  'HOLException' if any of the internal calls to 'mkConst' or 'mkComb' fail.
-}
mkBinary :: Text -> HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkBinary s l r = 
  (do c <- mkConst s ([]::HOLTypeEnv)
      let c' = inst [(tyA, typeOf l), (tyB, typeOf r)] c
      fromRightM $ mkComb (fromRight $ mkComb c' l) r)
  <?> "mkBinary: " ++ show s

{-| 
  A version of 'mkBinary' that accepts the operator as a pre-constructed term.
-}
mkBinop :: HOLTerm -> HOLTerm -> HOLTerm -> Either String HOLTerm
mkBinop op tm1 tm2 =
  liftM1 mkComb (mkComb op tm1) tm2 <?> "mkBinop"

{-| 
  Iteratively builds a complex combination using 'mkBinop', i.e.
 
  > listMkBinop (/\) [T, F, T] === T /\ F /\ T
-} 
listMkBinop :: HOLTerm -> [HOLTerm] -> Either String HOLTerm
listMkBinop = foldr1M . mkBinop

{-|
  The inverse of 'listMkBinop'.  Destructs a complex combination built with
  a binary operator into its list of arguments.
-}
binops :: HOLTerm -> HOLTerm -> [HOLTerm]
binops = stripList . destBinop

-- syntax for complex abstractions
-- | Predicate for generalized abstractions.  See 'mkGAbs' for more details.
isGAbs :: HOLTerm -> Bool
isGAbs = isJust . destGAbs

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
destGAbs :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destGAbs tm@Abs{} = destAbs tm
destGAbs (Comb (Const "GABS" _) (Abs _ bod)) =
    do (ltm, rtm) <- destGeq . snd $ stripForall bod
       ltm' <- rand ltm
       return (ltm', rtm)
  where destGeq :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
        destGeq = destBinary "GEQ"
destGAbs _ = Nothing

{-|
  Destructs an abstraction of specified binder name into its bound variable and
  its body term.  Fails with 'Nothing' if the provided term is not an
  abstraction with the specified binder name.
-}
destBinder :: Text -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
destBinder s (Comb (Const s' _) (Abs bv t))
    | s == s' = Just (bv, t)
    | otherwise = Nothing
destBinder _ _ = Nothing

-- | The pattern synonym equivalent of 'destBinder'.
pattern Bind s bv tm <- Comb (Const s _) (Abs bv tm)

{-|
  Destructs a type abstraction of specified binder name into its bound type
  variable and its body term.  Fails with 'Nothing' if the provided term is not
  a type abstraction with the specified type binder name.
-}
destTyBinder :: Text -> HOLTerm -> Maybe (HOLType, HOLTerm)
destTyBinder s (Comb (Const s' _) (TyAbs bv t))
    | s == s' = Just (bv, t)
    | otherwise = Nothing
destTyBinder _ _ = Nothing

-- | The pattern synonym equivalent of 'destTyBinder'.
pattern TyBind s ty tm <- Comb (Const s _) (TyAbs ty tm)

{-|
  Constructor for generalized abstractions.  Generalized abstractions extend
  term abstractions to the more general of notion of a function mapping some
  structure to some term.  This allows us to bind patterns more complicated
  than a variable, i.e. binding pairs

  > \ (x:num, y:num) -> x + y

  or lists

  > \ CONS x xs -> x

  Note that in the case where the pattern to bind is simply a variable 'mkGAbs'
  just calls 'mkAbs'.
-}
mkGAbs :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkGAbs tm1@Var{} tm2 =
    mkAbs tm1 tm2 <#?> "mkGAbs: simple abstraction failed"
mkGAbs tm1 tm2 = 
    let fvs = frees tm1 in
      (do fTy <- mkFunTy (typeOf tm1) $ typeOf tm2
          let f = variant (frees tm1++frees tm2) $ mkVar "f" fTy
          bodIn <- listMkForall fvs =<< mkGEq (fromRight $ mkComb f tm1) tm2
          bndr <- mkConst "GABS" [(tyA, fTy)]
          fromRightM $ mkComb bndr =<< mkAbs f bodIn)
      <?> "mkGAbs"
  where mkGEq :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
        mkGEq t1 t2 = 
          do p <- mkConst "GEQ" [(tyA, typeOf t1)]
             fromRightM $ mkBinop p t1 t2

{-|
  Constructs an abstraction given a binder name and two argument terms.  Throws
  a 'HOLException' if any of the internal calls to 'mkConst', 'mkAbs', or 
  'mkComb' fail.

  Note that the given string can actually be any constant name of type 
  @(A -> *) -> *@, such that a well-typed term of the form @c (\\x . t)@ can be
  produced.
-}
mkBinder :: Text -> HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkBinder op v tm = 
    (do c <- mkConst op [(tyA, typeOf v)]
        fromRightM $ mkComb c =<< mkAbs v tm)
    <?> "mkBinder: " ++ show op

{-|
  Constructs a type abstraction given a type binder name, a type variable to
  find, and a body term.  Throws a 'HOLException' if any of the internal calls
  to 'mkConst', 'mkTyAbs', or 'mkComb' fail.

  Note that the given string can actually be any constant name of type
  @(% 'a . *) -> *@, such that a well-typed term of the form @c (\\\\x . t)@ can
  be produced.
-}
mkTyBinder :: Text -> HOLType -> HOLTerm -> HOL cls thry HOLTerm
mkTyBinder op v tm =
  (do c <- mkConst op ([]::HOLTypeEnv)
      fromRightM $ mkComb c =<< mkTyAbs v tm)
  <?> "mkTyBinder: " ++ show op

-- | A specific version of 'listMkAbs' for general abstractions.
listMkGAbs :: [HOLTerm] -> HOLTerm -> HOL cls thry HOLTerm
listMkGAbs = flip (foldrM mkGAbs)

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
destIff :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destIff (Comb (Comb (Const "=" (TyBool :-> _)) l) r) = Just (l, r)
destIff _ = Nothing

-- | The pattern synonym equivalent of 'destIff'.
pattern l :<=> r <- Comb (Comb (Const "=" (TyBool :-> TyBool :-> TyBool)) l) r

-- | Destructor for boolean conjunctions.
destConj :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destConj = destBinary "/\\"

-- | The pattern synonym equivalent of 'destConj'.
pattern l :/\ r <- Binary "/\\" l r

-- | Destructor for boolean implications.
destImp :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destImp = destBinary "==>"

-- | The pattern synonym equivalent of 'destImp'.
pattern l :==> r <- Binary "==>" l r

-- | Destructor for universal term quantification.
destForall :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destForall = destBinder "!"

-- | The pattern synonym equivalent of 'destForall'.
pattern Forall bv tm <- Bind "!" bv tm

-- | Destructor for existential term quantification.
destExists :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destExists = destBinder "?"

-- | The pattern synonym equivalent of 'destExists'.
pattern Exists bv tm <- Bind "?" bv tm

-- | Destructor for boolean disjunctions.
destDisj :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destDisj = destBinary "\\/"

-- | The pattern synonym equivalent of 'destDisj'.
pattern l :\/ r <- Binary "\\/" l r

-- | Destructor for boolean negations.
destNeg :: HOLTerm -> Maybe HOLTerm
destNeg (Comb (Const "~" _) p) = Just p
destNeg _ = Nothing

-- | The pattern synonym equivalent of 'destNeg'.
pattern Neg tm <- (Comb (Const "~" _) tm)

-- | Destructor for unique, existential quantification.
destUExists :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destUExists = destBinder "?!"

-- | The pattern synonym equivalent of 'destUExists'.
pattern UExists bv tm <- Bind "?!" bv tm

-- | Destructor for term-level universal type quantification.
destTyAll :: HOLTerm -> Maybe (HOLType, HOLTerm)
destTyAll = destTyBinder "!!"

-- | The pattern synonym equivalent of 'destTyAll'.
pattern TyAll ty tm <- TyBind "!!" ty tm

-- | Destructor for term-level existential type quantification.
destTyEx :: HOLTerm -> Maybe (HOLType, HOLTerm)
destTyEx = destTyBinder "??"

-- | The pattern synonym equivalent of 'destTyEx'.
pattern TyEx ty tm <- TyBind "??" ty tm

{-|
  Constructor for boolean conjunctions.  Throws a 'HOLException' if the internal
  calls to 'mkComb' fail.
-}
mkIff :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkIff l r =
    (do bicond <- mkConst "=" [(tyA, tyBool)]
        fromRightM $ flip mkComb r =<< mkComb bicond l)
    <?> "mkIff"

{-|
  Constructor for boolean conjunctions.  Throws a 'HOLException' if the internal
  call to 'mkBinary' fails.
-}
mkConj :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkConj = mkBinary "/\\"

{-|
  Constructor for boolean implications.  Throws a 'HOLException' if the internal
  call to 'mkBinary' fails.
-}
mkImp :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkImp = mkBinary "==>"

{-| 
  Constructor for universal term quantification.  Throws a 'HOLException' if the
  internal call to 'mkBinder' fails.
-}
mkForall :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkForall = mkBinder "!"

{-| 
  Constructor for existential term quantification.  Throws a 'HOLException' if 
  the internal call to 'mkBinder' fails.
-}
mkExists :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkExists = mkBinder "?"

{-|
  Constructor for boolean disjunctions.  Throws a 'HOLException' if the internal
  call to 'mkBinary' fails.
-}
mkDisj :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkDisj = mkBinary "\\/"

{-|
  Constructor for boolean negations.  Throws a 'HOLException' if any of the 
  internal calls to 'mkConst' or 'mkComb' fail.
-}
mkNeg :: HOLTerm -> HOL cls thry HOLTerm
mkNeg tm = 
    (do c <- mkConst "~" ([]::HOLTypeEnv)
        fromRightM $ mkComb c tm)
    <?> "mkNeg"

{-| 
  Constructor for unique, existential term quantification.  Throws a 
  'HOLException' if the internal call to 'mkBinder' fails.
-}
mkUExists :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkUExists = mkBinder "?!"

{-|
  Constructor for term-level universal type quantification.  Throws a 
  'HOLException' if the internal call to 'mkTyBinder' fails.
-}
mkTyAll :: HOLType -> HOLTerm -> HOL cls thry HOLTerm
mkTyAll = mkTyBinder "!!"

{-|
  Constructor for term-level existential type quantification.  Throws a 
  'HOLException' if the internal call to 'mkTyBinder' fails.
-}
mkTyEx :: HOLType -> HOLTerm -> HOL cls thry HOLTerm
mkTyEx = mkTyBinder "??"

-- | Constructs a complex conjunction from a given list of propositions.
listMkConj :: [HOLTerm] -> HOL cls thry HOLTerm
listMkConj = foldr1M mkConj

-- | A specific version of 'listMkAbs' for universal term quantification.
listMkForall :: [HOLTerm] -> HOLTerm -> HOL cls thry HOLTerm
listMkForall = flip (foldrM mkForall)

-- | A specific version of 'listMkAbs' for existential term quantification.
listMkExists :: [HOLTerm] -> HOLTerm -> HOL cls thry HOLTerm
listMkExists vs bod = foldrM mkExists bod vs

-- | Constructs a complex disjunction from a given list of propositions.
listMkDisj :: [HOLTerm] -> HOL cls thry HOLTerm
listMkDisj = foldr1M mkDisj

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
isList = isJust . destList

-- | Predicate for let binding terms.
isLet :: HOLTerm -> Bool
isLet = isJust . destLet

-- | Destructor for list @CONS@.
destCons :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destCons = destBinary "CONS"

{-|
  Destructor for list terms.  Returns a list of the elements in the term.  Fails
  with 'Nothing' if internall the term is not of the form

  > x1 `CONS` .... xn `CONS` NIL
-}
destList :: HOLTerm -> Maybe [HOLTerm]
destList tm =
    let (tms, nil) = splitList destCons tm in
      case nil of
        (Const "NIL" _) -> Just tms
        _ -> Nothing

{-|
  Destructs a let binding term into a list of its name and value pairs and its
  body term.  Fails with 'Nothing' if internally the term is not of the form

  > LET (x1, v1) ... (xn, vn) LET_END
-}
destLet :: HOLTerm -> Maybe ([(HOLTerm, HOLTerm)], HOLTerm)
destLet tm =
  case stripComb tm of
    (Const "LET" _, a:args) ->
      let (vars, lebod) = stripGAbs a
          eqs = zip vars args in
        case lebod of
          Comb (Const "LET_END" _) bod -> Just (eqs, bod)
          _ -> Nothing
    _ -> Nothing

{-|
  Constructs a let binding term provided a list of variable/value pairs and a
  body term.
-}
mkLet :: [(HOLTerm, HOLTerm)] -> HOLTerm -> HOL cls thry HOLTerm
mkLet assigs bod =
    do tmLetEnd <- mkConst "LET_END" [(tyA, typeOf bod)]
       let (ls, rs) = unzip assigs
           lend = fromRight $ mkComb tmLetEnd bod
       lbod <- listMkGAbs ls lend
       let (ty1, ty2) = fromJust . destFunTy $ typeOf lbod
       tmLet <- mkConst "LET" [(tyA, ty1), (tyB, ty2)]
       liftO $ listMkComb tmLet (lbod:rs)

{-|
  Converts a numeral term to an 'Integer'.  Fails with 'Nothing' if internally
  the term is not of the form

  > NUMERAL bits _0, where bits is a series of BIT0 and BIT1 terms  
-} 
destNumeral :: HOLTerm -> Maybe Integer
destNumeral (Comb (Const "NUMERAL" _) r) = destNum r
  where destNum :: Num a => HOLTerm -> Maybe a
        destNum (Const "_0" _) = Just 0
        destNum (Comb (Const "BIT0" _) r') =
          liftM (2 *) $ destNum r'
        destNum (Comb (Const "BIT1" _) r') =
          liftM (\ x -> 1 + 2 * x) $ destNum r'
        destNum _ = Nothing
destNumeral _ = Nothing
