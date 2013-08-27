{-# LANGUAGE ViewPatterns #-}

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
    ( -- * Variable Term Generation
      genVarWithName -- :: String -> HOLType -> HOL cls thry HOLTerm
    , genVar         -- :: HOLType -> HOL cls thry HOLTerm
      -- * Common Type Functions
    , occursIn   -- :: HOLType -> HOLType -> Bool
    , tysubst    -- :: HOLTypeEnv -> HOLType -> Either String HOLType
    , alphaUtype -- :: HOLType -> HOLType -> Either String HOLType
      -- * Common Term Functions
    , freeIn     -- :: HOLTerm -> HOLTerm -> Bool
    , subst      -- :: HOLTermEnv -> HOLTerm -> HOL cls thry HOLTerm
    , alpha      -- :: HOLTerm -> HOLTerm -> Either String HOLTerm
    , alphaTyabs -- :: HOLType -> HOLTerm -> Either String HOLTerm
    , findTerm   -- :: (HOLTerm -> Bool) -> HOLTerm -> Maybe HOLTerm
    , findTerms  -- :: (HOLTerm -> Bool) -> HOLTerm -> [HOLTerm]
    , findPath   -- :: (HOLTerm -> Bool) -> HOLTerm -> Maybe String
    , followPath -- :: String -> HOLTerm -> Maybe HOLTerm
      -- * Common Theorem Functions
    , typeVarsInThm -- :: HOLThm -> [HOLType]
    , thmFrees      -- :: HOLThm -> [HOLTerm]
      -- * Derived Destructors and Constructors for Basic Terms
    , listMkComb  -- :: HOLTerm -> [HOLTerm] -> Either String HOLTerm
    , listMkAbs   -- :: [HOLTerm] -> HOLTerm -> Either String HOLTerm
    , mkArgs      -- :: String -> [HOLTerm] -> [HOLType] -> [HOLTerm]
    , rator       -- :: HOLTerm -> Maybe HOLTerm
    , rand        -- :: HOLTerm -> Maybe HOLTerm
    , bndvar      -- :: HOLTerm -> Maybe HOLTerm
    , body        -- :: HOLTerm -> Maybe HOLTerm
    , bndvarTyabs -- :: HOLTerm -> Maybe HOLType
    , bodyTyabs   -- :: HOLTerm -> Maybe HOLTerm
    , stripComb   -- :: HOLTerm -> (HOLTerm, [HOLTerm])
    , stripAbs    -- :: HOLTerm -> ([HOLTerm], HOLTerm)
      -- * Type Matching Functions
    , typeMatch   -- :: HOLType -> HOLType -> SubstTrip -> Maybe SubstTrip
    , mkMConst    -- :: String -> HOLType -> HOL cls thry HOLTerm
    , mkIComb     -- :: HOLTerm -> HOLTerm -> Maybe HOLTerm
    , listMkIComb -- :: String -> [HOLTerm] -> HOL cls thry HOLTerm
      -- * Predicates, Constructors, and Destructors for Binary Terms
    , isBinary    -- :: String -> HOLTerm -> Bool
    , isBinop     -- :: HOLTerm -> HOLTerm -> Bool
    , destBinary  -- :: String -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destBinop   -- :: HOLTerm -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , mkBinary    -- :: String -> HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkBinop     -- :: HOLTerm -> HOLTerm -> HOLTerm -> Either String HOLTerm
    , listMkBinop -- :: HOLTerm -> [HOLTerm] -> Either String HOLTerm
    , binops      -- :: HOLTerm -> HOLTerm -> [HOLTerm]
      -- * Predicates, Constructors, and Destructors for Complex Abstractions
    , isGAbs       -- :: HOLTerm -> Bool
    , isBinder     -- :: String -> HOLTerm -> Bool
    , isTyBinder   -- :: String -> HOLTerm -> Bool
    , destGAbs     -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destBinder   -- :: String -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destTyBinder -- :: String -> HOLTerm -> Maybe (HOLType, HOLTerm)
    , mkGAbs       -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkBinder     -- :: String -> HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkTyBinder   -- :: String -> HOLType -> HOLTerm -> HOL cls thry HOLTerm
    , listMkGAbs   -- :: [HOLTerm] -> HOLTerm -> HOL cls thry HOLTerm
    , stripGAbs    -- :: HOLTerm -> ([HOLTerm], HOLTerm)
      -- * Predicates, Constructors, and Destructors for Propositions
    , isConj       -- :: HOLTerm -> Bool
    , isImp        -- :: HOLTerm -> Bool
    , isForall     -- :: HOLTerm -> Bool
    , isExists     -- :: HOLTerm -> Bool
    , isDisj       -- :: HOLTerm -> Bool
    , isNeg        -- :: HOLTerm -> Bool
    , isUExists    -- :: HOLTerm -> Bool
    , isTyAll      -- :: HOLTerm -> Bool
    , isTyEx       -- :: HOLTerm -> Bool
    , destConj     -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destImp      -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destForall   -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destExists   -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destDisj     -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destNeg      -- :: HOLTerm -> Maybe HOLTerm
    , destUExists  -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destTyAll    -- :: HOLTerm -> Maybe (HOLType, HOLTerm)
    , destTyEx     -- :: HOLTerm -> Maybe (HOLType, HOLTerm)
    , mkConj       -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkImp        -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkForall     -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkExists     -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkDisj       -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkNeg        -- :: HOLTerm -> HOL cls thry HOLTerm
    , mkUExists    -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    , mkTyAll      -- :: HOLType -> HOLTerm -> HOL cls thry HOLTerm
    , mkTyEx       -- :: HOLType -> HOLTerm -> HOL cls thry HOLTerm
    , listMkConj   -- :: [HOLTerm] -> HOL cls thry HOLTerm
    , listMkDisj   -- :: [HOLTerm] -> HOL cls thry HOLTerm
    , listMkForall -- :: [HOLTerm] -> HOLTerm -> HOL cls thry HOLTerm
    , listMkExists -- :: [HOLTerm] -> HOLTerm -> HOL cls thry HOLTerm
    , conjuncts    -- :: HOLTerm -> [HOLTerm]
    , disjuncts    -- :: HOLTerm -> [HOLTerm]
    , stripForall  -- :: HOLTerm -> ([HOLTerm], HOLTerm)
    , stripExists  -- :: HOLTerm -> ([HOLTerm], HOLTerm)
    , stripTyAll   -- :: HOLTerm -> ([HOLType], HOLTerm)
    , stripTyEx    -- :: HOLTerm -> ([HOLType], HOLTerm)
      -- * Predicates, Constructors, and Destructors for Other Terms
    , isCons      -- :: HOLTerm -> Bool
    , isList      -- :: HOLTerm -> Bool
    , isLet       -- :: HOLTerm -> Bool
    , destCons    -- :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
    , destList    -- :: HOLTerm -> Maybe [HOLTerm]
    , destLet     -- :: HOLTerm -> Maybe ([(HOLTerm, HOLTerm)], HOLTerm)
    , destNumeral -- :: HOLTerm -> Maybe Integer
      -- * Term Nets
    , module HaskHOL.Core.Basics.Nets
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics.Nets

-- Term Generation
{-|  
  Generates a new term variable consisting of a given prefix and the next value
  in the fresh term counter.
-}
genVarWithName :: String -> HOLType -> HOL cls thry HOLTerm
genVarWithName n ty =
    do count <- tickTermCounter
       return $! mkVar (n ++ show count) ty

-- | A version of 'genVarWithName' that defaults to the prefix \"_\".
genVar :: HOLType -> HOL cls thry HOLTerm
genVar = genVarWithName "_"

-- functions for manipulating types
{-| 
  Checks to see if the first type occurs in the second type.  Note that the
  predicate is also satisfied if the two types are equal.
-}
occursIn :: HOLType -> HOLType -> Bool
occursIn ty bigTy
  | ty == bigTy = True
  | otherwise = case view bigTy of
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
  <|> case view ty of
        TyVar {} -> return ty
        TyApp tycon tyvars -> 
          (tyApp tycon =<< mapM (tysubst env) tyvars) <?>
            "tysubst: bad type application"
        UType bv bod -> 
          (mkUType bv =<< tysubst (filter (\ (x, _) -> x /= bv) env) bod) <?>
            "tysubst: bad universal type"

{-|
  Alpha conversion for universal types.  Renames a bound type variable to match
  the name of a provided type variable.  Fails with 'Left' in the following
  cases:

  * First type is not a small type variable.

  * Second type is not a universal type.

  * The type variable is free in the body of the universal type.
-}
alphaUtype :: HOLType -> HOLType -> Either String HOLType
alphaUtype tv@(view -> TyVar True _) ty@(view -> UType tv0 bod)
    | tv == tv0 = Right ty
    | tv `elem` tyVars bod = Left "alphaUtype: variable free in body of type."
    | otherwise = mkUType tv (typeSubst [(tv0, tv)] bod) <?>
                    "alphaUtype: construction of universal type failed."
alphaUtype _ (view -> UType{}) = 
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
  (case view tm2 of
     Comb l r -> freeIn tm1 l || freeIn tm1 r
     Abs bv bod -> not (varFreeIn bv tm1) && freeIn tm1 bod
     TyAbs bv bod -> bv `elem` typeVarsInTerm tm1 && freeIn tm1 bod
     TyComb tm _ -> freeIn tm1 tm
     _ -> False)

{-| 
  Basic term substitution.  Throws a 'HOLException' when the substitution would 
  result in an invalid term construction.

  Note that the order of the elements of the substitution pairs matches other 
  environments in the systems, such that for the pair @(A, B)@ @B@ will be 
  substituted for all instances of @A@.  
-}
subst :: HOLTermEnv -> HOLTerm -> HOL cls thry HOLTerm
subst ilist tm =
    let (xs, ts) = unzip ilist in
      do gs <- mapM (genVar . typeOf) xs
         tm' <- liftEither "subst" $ ssubst (zip xs gs) tm
         if tm' == tm
            then return tm
            else return $! varSubst (zip gs ts) tm'
  where ssubst :: HOLTermEnv -> HOLTerm -> Either String HOLTerm
        ssubst [] t = Right t
        ssubst env t = 
          case find (\ (t', _) -> t `aConv` t') env of
            Just (_, res) -> Right res
            Nothing ->
              case view t of
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
alpha v@(view -> Var _ ty) tm@(view -> Abs v0@(view -> Var _ ty0) bod)
    | v == v0 = Right tm
    | ty /= ty0 = Left "alpha: types of variables not equal."
    | v `varFreeIn` bod = Left "alpha: variable free in body of abstraction."
    | otherwise = mkAbs v (varSubst [(v0, v)] bod) <?> 
                    "alpha: construction of abstraction failed."
alpha _ (view -> Abs{}) = Left "alpha: first term not a variable."
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
alphaTyabs ty@(view -> TyVar True _) tm@(view -> TyAbs ty0 bod)
    | ty == ty0 = Right tm
    | ty `elem` typeVarsInTerm bod = 
        Left "alphaTyabs: type free in body of type abstraction."
    | otherwise = mkTyAbs ty $ inst [(ty0, ty)] bod
alphaTyabs _ (view -> TyAbs{}) = 
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
        case view tm of
          Abs _ bod -> findTerm p bod
          Comb l r -> findTerm p l <|> findTerm p r
          TyAbs _ bod -> findTerm p bod
          TyComb tm' _ -> findTerm p tm'
          _ -> Nothing

-- | Searches a term for all unique subterms that satisfy a given predicate.
findTerms :: (HOLTerm -> Bool) -> HOLTerm -> [HOLTerm]
findTerms = findRec []
  where findRec :: [HOLTerm] -> (HOLTerm -> Bool) -> HOLTerm -> [HOLTerm]
        findRec tl p tm =
            let tl' = if p tm then insert tm tl else tl in
              case view tm of
                Abs _ bod -> findRec tl' p bod
                Comb l r -> findRec (findRec tl' p l) p r
                TyAbs _ bod -> findRec tl' p bod
                TyComb tm' _ -> findRec tl' p tm'
                _ -> tl'

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
        case view tm of
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
followPath ('l':t) (view -> Comb l _) = followPath t l
followPath ('r':t) (view -> Comb _ r) = followPath t r
followPath ('c':t) (view -> TyComb tm _) = followPath t tm
followPath ('b':t) (view -> Abs _ bod) = followPath t bod
followPath ('t':t) (view -> TyAbs _ bod) = followPath t bod
followPath _ _ = Nothing

-- theorem manipulators
-- | Returns the list of all free type variables in a theorem.
typeVarsInThm :: HOLThm -> [HOLType]
typeVarsInThm (view -> Thm asl c) =
    foldr (union . typeVarsInTerm) (typeVarsInTerm c) asl

-- | Returns the list of all free term variables in a theorem.
thmFrees :: HOLThm -> [HOLTerm]
thmFrees (view -> Thm asl c) = 
    foldr (union . frees) (frees c) asl

-- more syntax
{-|
  Constructs a complex combination that represents the application of a 
  function to a list of arguments.  Fails with 'Left' if any internal call to 
  'mkComb' fails.
-}
listMkComb :: HOLTerm -> [HOLTerm] -> Either String HOLTerm
listMkComb = foldlM mkComb

{-|
  Constructs a complex abstraction that represents a term with multiple
  bound variables.  Fails with 'Left' if any internal call to 'mkAbs' fails.
-}
listMkAbs :: [HOLTerm] -> HOLTerm -> Either String HOLTerm
listMkAbs = flip (foldrM mkAbs)

-- Useful function to create stylized arguments using numbers
{-|
  Constructs a list of term variables of a given prefix.  Names are adjusted
  as necessary with 'variant' to avoid clashing with the provided list of term
  variables.  The number and types of the resultant variables is directed by 
  the provided list of types, i.e.

  > mkArgs "x" avoids [ty1, ... tyn] === [x1:ty1, ..., xn:tyn] where {x1, ..., xn} are not elements of avoids
-}
mkArgs :: String -> [HOLTerm] -> [HOLType] -> [HOLTerm]
mkArgs s avoid (ty:[]) = [variant avoid $ mkVar s ty]
mkArgs s avoid tys = mkRec 0 s avoid tys
  where mkRec :: Int -> String -> [HOLTerm] -> [HOLType] -> [HOLTerm]
        mkRec _ _ _ [] = []
        mkRec n x avs (y:ys) =
          let v' = variant avs $ mkVar (x ++ show n) y
              vs = mkRec (n + 1) x (v':avs) ys in
            (v':vs)

{-| 
  Returns the left term of a combination.  Fails with 'Nothing' if the provided
  term is not a combination.
-}
rator :: HOLTerm -> Maybe HOLTerm
rator (view -> Comb l _) = Just l
rator _ = Nothing

{-|
  Returns the right term of a combination.  Fails with 'Nothing' if the provided
  term is not a combination.
-}
rand :: HOLTerm -> Maybe HOLTerm
rand (view -> Comb _ r) = Just r
rand _ = Nothing

{-|
  Returns the bound term of an abstraction.  Fails with 'Nothing' if the
  provided term is not an abstraction.
-}
bndvar :: HOLTerm -> Maybe HOLTerm
bndvar (view -> Abs bv _) = Just bv
bndvar _ = Nothing

{-|
  Returns the body term of an abstraction.  Fails with 'Nothing' if the
  provided term is not an abstraction.
-}
body :: HOLTerm -> Maybe HOLTerm
body (view -> Abs _ bod) = Just bod
body _ = Nothing

{-|
  Returns the bound type of a type abstraction.  Fails with 'Nothing' if the
  provided term is not a type abstraction.
-}
bndvarTyabs :: HOLTerm -> Maybe HOLType
bndvarTyabs (view -> TyAbs bv _) = Just bv
bndvarTyabs _ = Nothing

{-|
  Returns the body term of a type abstraction.  Fails with 'Nothing' if the
  provided term is not a type abstraction.
-}
bodyTyabs :: HOLTerm -> Maybe HOLTerm
bodyTyabs (view -> TyAbs _ bod) = Just bod
bodyTyabs _ = Nothing

{-|
  Destructs a complex combination returning its function term and its list of
  argument terms.
-}
stripComb :: HOLTerm -> (HOLTerm, [HOLTerm])
stripComb = revSplitList destComb

{-|
  Destructs a complex abstraction returning its list of bound variables and its
  body term.
-}
stripAbs :: HOLTerm -> ([HOLTerm], HOLTerm)
stripAbs = splitList destAbs

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
        typeMatchRec v@(view -> TyVar{}) c acc@(env, (sfar, opTys, opOps))
            | v `elem` env = Just acc
            | otherwise =
                case lookup v sfar of
                  Just c'
                    | c' == c -> Just acc
                    | otherwise -> Nothing
                  Nothing -> Just (env, ((v, c):sfar, opTys, opOps))
        typeMatchRec (view -> UType tvv varg) 
                     (view -> UType tvc carg) (env, sfar) =
            let carg' = if tvv == tvc then carg 
                        else typeSubst [(tvc, tvv)] carg in
              typeMatchRec varg carg' (tvv:env, sfar)
        typeMatchRec (view -> TyApp vop vargs) 
                     (view -> TyApp cop cargs) acc@(env, (sfar, opTys, opOps))
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
mkMConst :: String -> HOLType -> HOL cls thry HOLTerm
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
listMkIComb :: String -> [HOLTerm] -> HOL cls thry HOLTerm
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
isBinary :: String -> HOLTerm -> Bool
isBinary s (view -> Comb (view -> Comb (view -> Const s' _ _) _) _) = s == s'
isBinary _ _ = False

-- | A version of 'isBinary' that tests for operator terms, not strings.
isBinop :: HOLTerm -> HOLTerm -> Bool
isBinop op (view -> Comb (view -> Comb op' _) _) = op' == op
isBinop _ _ = False

{-|
  Destructs a binary application returning its left and right arguments.  Fails 
  with 'Nothing' if the provided term is not a binary application with the 
  specified operator name.
-}
destBinary :: String -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
destBinary s (view -> Comb (view -> Comb (view -> Const s' _ _) l) r)
    | s == s' = Just (l, r)
    | otherwise = Nothing
destBinary _ _ = Nothing

-- | A version of 'destBinary' that tests for operator terms, not strings.
destBinop :: HOLTerm -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
destBinop op (view -> Comb (view -> Comb op' l) r)
    | op' == op = Just (l, r)
    | otherwise = Nothing
destBinop _ _ = Nothing

{-|
  Constructs a binary application given a constant name and two argument terms.
  Note that no instantiation is performed, thus the constant must be monomorphic
  or the provided arguments must match the constant's general type.  Throws a
  'HOLException' if any of the internal calls to 'mkConst' or 'mkComb' fail.
-}
mkBinary :: String -> HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkBinary s l r = 
  (do c <- mkConst s ([]::HOLTypeEnv)
      fromRightM $ mkComb (fromRight $ mkComb c l) r)
  <?> "mkBinary: " ++ s

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
isBinder :: String -> HOLTerm -> Bool
isBinder s (view -> Comb (view -> Const s' _ _) (view -> Abs{})) = s == s'
isBinder _ _ = False

{-| 
  Predicate that tests if a term is an abtraction of a specified type binder
  name.
-}
isTyBinder :: String -> HOLTerm -> Bool
isTyBinder s (view -> Comb (view -> Const s' _ _) (view -> TyAbs{})) = s == s'
isTyBinder _ _ = False

{-| 
  Destructor for generalized abstractions.  Fails with 'Nothing' if the provided
  term is not an abstraction or generalized abstraction.  See 'mkGAbs' for more 
  details.
-}
destGAbs :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destGAbs tm@(view -> Abs{}) = destAbs tm
destGAbs (view -> Comb (view -> Const "GABS" _ _) (view -> Abs _ bod)) =
    firstM rand =<< (destGeq . snd $ stripForall bod)
  where destGeq :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
        destGeq = destBinary "GEQ"
destGAbs _ = Nothing

{-|
  Destructs an abstraction of specified binder name into its bound variable and
  its body term.  Fails with 'Nothing' if the provided term is not an
  abstraction with the specified binder name.
-}
destBinder :: String -> HOLTerm -> Maybe (HOLTerm, HOLTerm)
destBinder s (view -> Comb (view -> Const s' _ _) (view -> Abs bv t))
    | s == s' = Just (bv, t)
    | otherwise = Nothing
destBinder _ _ = Nothing

{-|
  Destructs a type abstraction of specified binder name into its bound type
  variable and its body term.  Fails with 'Nothing' if the provided term is not
  a type abstraction with the specified type binder name.
-}
destTyBinder :: String -> HOLTerm -> Maybe (HOLType, HOLTerm)
destTyBinder s (view -> Comb (view -> Const s' _ _) (view -> TyAbs bv t))
    | s == s' = Just (bv, t)
    | otherwise = Nothing
destTyBinder _ _ = Nothing

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
mkGAbs tm1@(view -> Var{}) tm2 =
    liftEither "mkGAbs: simple abstraction failed" $ mkAbs tm1 tm2
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
mkBinder :: String -> HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkBinder op v tm = 
    (do c <- mkConst op [(tyA, typeOf v)]
        fromRightM $ mkComb c =<< mkAbs v tm)
    <?> "mkBinder: " ++ op

{-|
  Constructs a type abstraction given a type binder name, a type variable to
  find, and a body term.  Throws a 'HOLException' if any of the internal calls
  to 'mkConst', 'mkTyAbs', or 'mkComb' fail.

  Note that the given string can actually be any constant name of type
  @(% 'a . *) -> *@, such that a well-typed term of the form @c (\\\\x . t)@ can
  be produced.
-}
mkTyBinder :: String -> HOLType -> HOLTerm -> HOL cls thry HOLTerm
mkTyBinder op v tm =
  (do c <- mkConst op ([]::HOLTypeEnv)
      fromRightM $ mkComb c =<< mkTyAbs v tm)
  <?> "mkTyBinder: " ++ op

-- | A specific version of 'listMkAbs' for general abstractions.
listMkGAbs :: [HOLTerm] -> HOLTerm -> HOL cls thry HOLTerm
listMkGAbs = flip (foldrM mkGAbs)

-- | A specific version of 'stripAbs' for general abstractions.
stripGAbs :: HOLTerm -> ([HOLTerm], HOLTerm)
stripGAbs = splitList destGAbs

-- common special cases of binary ops
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
isNeg (view -> Comb (view -> Const "~" _ _) _) = True
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

-- | Destructor for boolean conjunctions.
destConj :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destConj = destBinary "/\\"

-- | Destructor for boolean implications.
destImp :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destImp = destBinary "==>"

-- | Destructor for universal term quantification.
destForall :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destForall = destBinder "!"

-- | Destructor for existential term quantification.
destExists :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destExists = destBinder "?"

-- | Destructor for boolean disjunctions.
destDisj :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destDisj = destBinary "\\/"

-- | Destructor for boolean negations.
destNeg :: HOLTerm -> Maybe HOLTerm
destNeg (view -> Comb (view -> Const "~" _ _) p) = Just p
destNeg _ = Nothing

-- | Destructor for unique, existential quantification.
destUExists :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destUExists = destBinder "?!"

-- | Destructor for term-level universal type quantification.
destTyAll :: HOLTerm -> Maybe (HOLType, HOLTerm)
destTyAll = destTyBinder "!!"

-- | Destructor for term-level existential type quantification.
destTyEx :: HOLTerm -> Maybe (HOLType, HOLTerm)
destTyEx = destTyBinder "??"

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
      case view nil of
        (Const "NIL" _ _) -> Just tms
        _ -> Nothing

{-|
  Destructs a let binding term into a list of its name and value pairs and its
  body term.  Fails with 'Nothing' if internally the term is not of the form

  > LET (x1, v1) ... (xn, vn) LET_END
-}
destLet :: HOLTerm -> Maybe ([(HOLTerm, HOLTerm)], HOLTerm)
destLet tm =
  case stripComb tm of
    (view -> Const "LET" _ _, a:args) ->
      let (vars, lebod) = stripGAbs a
          eqs = zip vars args in
        case view lebod of
          Comb (view -> Const "LET_END" _ _) bod -> Just (eqs, bod)
          _ -> Nothing
    _ -> Nothing

{-|
  Converts a numeral term to an 'Integer'.  Fails with 'Nothing' if internally
  the term is not of the form

  > NUMERAL bits _0, where bits is a series of BIT0 and BIT1 terms  
-} 
destNumeral :: HOLTerm -> Maybe Integer
destNumeral (view -> Comb (view -> Const "NUMERAL" _ _) r) = destNum r
  where destNum :: HOLTerm -> Maybe Integer
        destNum (view -> Const "_0" _ _) = Just 0
        destNum (view -> Comb (view -> Const "BIT0" _ _) r') =
          liftM (2 *) $ destNum r'
        destNum (view -> Comb (view -> Const "BIT1" _ _) r') =
          liftM (\ x -> 1 + 2 * x) $ destNum r'
        destNum _ = Nothing
destNumeral _ = Nothing
