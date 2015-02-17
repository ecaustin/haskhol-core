{-# LANGUAGE MultiParamTypeClasses, PatternSynonyms #-}

{-|
  Module:    HaskHOL.Core.Kernel.Terms
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module exports a safe view of HOL terms for HaskHOL.  It also defines
  the primitive functions related to terms.  For clarity, these functions have
  been seperated based on their influential system: HOL Light, Stateless HOL,
  and HOL2P.

  Note that, per the stateless approach, any stateful, but still primitive,
  functions related to terms have been relocated to the "HaskHOL.Core.State"
  module.
-}
module HaskHOL.Core.Kernel.Terms
    ( -- * A View of HOL Terms
       -- ** A Quick Note on View Patterns
        -- $ViewPatterns
       -- ** A High-Level Overview of HOL Terms
        -- $HOLTerms
      HOLTerm
    , pattern Var
    , pattern Const
    , pattern Comb
    , pattern Abs
    , pattern TyComb
    , pattern TyAbs
    , ConstTag
    , pattern Primitive
    , pattern Defined
    , pattern MkAbstract
    , pattern DestAbstract
    , HOLTermEnv
      -- * HOL Light Term Primitives
       -- ** Alpha-Equivalence of Terms
    , alphaOrder
    , aConv
       -- ** Predicates, Constructors, and Destructors for Basic Terms
    , isVar
    , isConst
    , isAbs
    , isComb
    , mkVar
    , mkAbs
    , mkComb
    , destVar
    , destConst
    , destComb
    , destAbs
       -- ** Term and Type Variable Extractors
    , frees
    , catFrees
    , freesIn
    , varFreeIn
    , typeVarsInTerm
    , typeVarsInTerms
       -- ** Term Substitution and Instantiation
    , varSubst
    , Inst
    , inst
    , instFull
    , instConst
    , instConstFull
       -- ** Commonly Used Terms and Functions
    , tmEq
    , pattern TmEq
    , pattern (:=)
    , isEq
    , primMkEq
    , destEq
    , variant
    , variants
      -- * Stateless HOL Term Primitives
       -- ** Constructors for Constant Tags
    , newPrimitiveConst
       -- ** Type Operator Variable Extractors
    , typeOpVarsInTerm
    , typeOpVarsInTerms
      -- * HOL2P Term Primitives
       -- ** Predicates, Constructors, and Destructors for Term-Level Types
    , isTyAbs
    , isTyComb
    , mkTyAbs
    , mkTyComb
    , destTyAbs
    , destTyComb
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel.Prims
import HaskHOL.Core.Kernel.Types

{- 
   HOL Light Term Primitives
   Note that the following primitives are in HaskHOL.Core.State as per 
   Stateless HOL:
   constants, getConstType, newConstant, mkConst, mkEq
-}
-- | Provides an ordering for two terms modulo alpha-equivalence
alphaOrder :: HOLTerm -> HOLTerm -> Ordering
alphaOrder = orda []
  where orda :: [(HOLTerm, HOLTerm)] -> HOLTerm -> HOLTerm -> Ordering
        orda env tm1@VarIn{} tm2@VarIn{} = ordav env tm1 tm2
        orda _ tm1@ConstIn{} tm2@ConstIn{} = tm1 `aorder` tm2
        orda env (CombIn s1 t1) (CombIn s2 t2) =
            case orda env s1 s2 of
              EQ -> orda env t1 t2
              res -> res
        orda env (AbsIn x1@(VarIn _ ty1) t1) 
                 (AbsIn x2@(VarIn _ ty2) t2) =
            case tyAlphaOrder ty1 ty2 of
              EQ -> orda ((x1, x2):env) t1 t2
              res -> res
        orda _ tm1@AbsIn{} tm2@AbsIn{} = compare tm1 tm2
        orda env (TyAbsIn tv1@(TyVarIn True _) tb1) 
                 (TyAbsIn tv2@(TyVarIn True _) tb2) =
            let tb2' = inst [(tv2, tv1)] tb2 in
              orda env tb1 tb2'
        orda _ tm1@TyAbsIn{} tm2@TyAbsIn{} = compare tm1 tm2
        orda env (TyCombIn t1 ty1) (TyCombIn t2 ty2) =
            case orda env t1 t2 of
              EQ -> tyAlphaOrder ty1 ty2
              res -> res
        orda _ ConstIn{} _ = LT
        orda _ _ ConstIn{} = GT
        orda _ VarIn{} _ = LT
        orda _ _ VarIn{} = GT
        orda _ CombIn{} _ = LT
        orda _ _ CombIn{} = GT
        orda _ AbsIn{} _ = LT
        orda _ _ AbsIn{} = GT
        orda _ TyAbsIn{} _ = LT
        orda _ _ TyAbsIn{} = GT

        ordav :: [(HOLTerm, HOLTerm)] -> HOLTerm -> HOLTerm -> Ordering
        ordav [] x1 x2 = x1 `aorder` x2
        ordav ((l, r):oenv) x1 x2
            | x1 == l = if x2 == r then EQ else LT
            | otherwise = if x2 == r then GT else ordav oenv x1 x2

        aorder :: HOLTerm -> HOLTerm -> Ordering
        aorder (VarIn s1 ty1) (VarIn s2 ty2) =
            case compare s1 s2 of
              EQ -> tyAlphaOrder ty1 ty2
              res -> res
        aorder (ConstIn s1 ty1 tag1) (ConstIn s2 ty2 tag2) =
            case compare s1 s2 of
              EQ -> case compare tag1 tag2 of
                      EQ -> tyAlphaOrder ty1 ty2
                      res -> res
              res -> res
        aorder x y = compare x y

-- | Tests if two terms are alpha-equivalent
aConv :: HOLTerm -> HOLTerm -> Bool
aConv tm1 tm2 = alphaOrder tm1 tm2 == EQ

-- | Predicate for term variables.
isVar :: HOLTerm -> Bool
isVar VarIn{} = True
isVar _ = False

-- | Predicate for term constants.
isConst :: HOLTerm -> Bool
isConst ConstIn{} = True
isConst _ = False

-- | Predicate for term abstractions.
isAbs :: HOLTerm -> Bool
isAbs AbsIn{} = True
isAbs _ = False

-- | Predicate for term combinations.
isComb :: HOLTerm -> Bool
isComb CombIn{} = True
isComb _ = False

-- | Constructs a term variable of a given name and type.
mkVar :: Text -> HOLType -> HOLTerm
mkVar = VarIn

{-| 
  Constructs a term abstraction of a given bound term and body term.  Fails with
  'Left' if the bound term is not a variable.
-}
mkAbs :: HOLTerm -> HOLTerm -> Either String HOLTerm
mkAbs bv@VarIn{} bod = Right $ AbsIn bv bod
mkAbs _ _ = Left "mkAbs"

{-|
  Constructs a combination of two given terms.  Fails with 'Left' in the
  following cases:

  * The first term does not have a function type.

  * The types of the two terms does not agree.
-}
mkComb :: HOLTerm -> HOLTerm -> Either String HOLTerm
mkComb f a = 
    case typeOf f of
      (TyAppIn (TyPrimitiveIn "fun" _) (ty:_)) -> 
          if typeOf a `tyAConv` ty
          then Right $ CombIn f a
          else Left "mkComb: argument type mismatch."
      _ -> Left "mkComb: argument not of function type."

{-| 
  Destructs a term variable, returning its name and type.  Fails with 'Nothing'
  if the provided term is not a variable.
-}
destVar :: HOLTerm -> Maybe (Text, HOLType)
destVar (VarIn s ty) = Just (s, ty)
destVar _ = Nothing

{-|
  Destructs a term constant, returning its name and type.  Note that no constant
  tag information is returned.  Fails with 'Nothing' if the provided term is
  not a constant.
-}
destConst :: HOLTerm -> Maybe (Text, HOLType)
destConst (ConstIn s ty _) = Just (s, ty)
destConst _ = Nothing

{-|
  Destructs a term combination, returning its function and argument terms.  
  Fails with 'Nothing' if the provided term is not a combination.
-}
destComb :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destComb (CombIn f x) = Just (f, x)
destComb _ = Nothing

{-|
  Destructs a term abstraction, returning its bound term and body term. Fails
  with 'Nothing' if the provided term is not an abstraction.
-}
destAbs :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destAbs (AbsIn v b) = Just (v, b)
destAbs _ = Nothing

-- | Returns a list of all free, term variables in a term.
frees :: HOLTerm -> [HOLTerm]
frees ConstIn{} = []
frees (AbsIn bv bod) = frees bod \\ [bv]
frees (CombIn s t) = frees s `union` frees t
frees (TyAbsIn _ t) = frees t
frees (TyCombIn t _) = frees t
frees t@VarIn{} = [t]

-- | Returns a list of all free, term variables in a list of terms.
catFrees :: [HOLTerm] -> [HOLTerm]
catFrees = foldr (union . frees) []

-- | Checks a list of term variables to see if they are all free in a give term.
freesIn :: [HOLTerm] -> HOLTerm -> Bool
freesIn _ ConstIn{} = True
freesIn acc (AbsIn bv bod) = freesIn (bv:acc) bod
freesIn acc (CombIn s t) = freesIn acc s && freesIn acc t
freesIn acc (TyAbsIn _ t) = freesIn acc t
freesIn acc (TyCombIn t _) = freesIn acc t
freesIn acc t@VarIn{} = t `elem` acc

-- | Checks if a variable or constant term is free in a given term.
varFreeIn :: HOLTerm -> HOLTerm -> Bool
varFreeIn v (AbsIn bv bod) = v /= bv && varFreeIn v bod
varFreeIn v (CombIn s t) = varFreeIn v s || varFreeIn v t
varFreeIn v (TyAbsIn _ t) = varFreeIn v t
varFreeIn v (TyCombIn t _) = varFreeIn v t
varFreeIn v tm = v == tm

{-| 
  Returns a list of all free, type variables in a term, not including 
  type operator variables.
-}
typeVarsInTerm :: HOLTerm -> [HOLType]
typeVarsInTerm (VarIn _ ty) = tyVars ty
typeVarsInTerm (ConstIn _ ty _) = tyVars ty
typeVarsInTerm (CombIn s t) = 
    typeVarsInTerm s `union` typeVarsInTerm t
typeVarsInTerm (AbsIn bv t) = 
    typeVarsInTerm bv `union` typeVarsInTerm t
typeVarsInTerm (TyAbsIn tv tm) = 
    typeVarsInTerm tm \\ [tv]
typeVarsInTerm (TyCombIn tm ty) = 
    typeVarsInTerm tm `union` tyVars ty

{-|
  Returns a list of all free, type variables in a list of terms, not including
  type operator variables.
-}
typeVarsInTerms :: [HOLTerm] -> [HOLType]
typeVarsInTerms =
    foldr (\ tm tvs -> typeVarsInTerm tm `union` tvs) []

{-| 
  Performs a basic term substitution using a substitution environment containing
  pairs of term variables and terms. 
  Note that the substitution environment is treated as an association list, such
  that:
  
  * The term variable acts as an index in the list, i.e. the substitution pair
    @(A, \\ x.x)@ indicates that the lambda term @\\x.x@ should be substituted 
    for the term variable @A@.

  Substitution fails with 'Nothing' in the case where a bad substitution list is
  presented.
-}
varSubst :: HOLTermEnv -> HOLTerm -> Maybe HOLTerm
varSubst [] term = Just term
varSubst theta term
    | all validPair theta = hush $ varSubstRec theta term
    | otherwise = Nothing
  where validPair :: (HOLTerm, HOLTerm) -> Bool
        validPair (VarIn _ ty, t) = ty `tyAConv` typeOf t
        validPair _ = False

        varSubstRec :: HOLTermEnv -> HOLTerm -> Either String HOLTerm
        varSubstRec _ tm@ConstIn{} = Right tm
        varSubstRec env (CombIn s t) =
              liftM1 mkComb (varSubstRec env s) =<< varSubstRec env t
        varSubstRec env tm@(AbsIn v s) =
            let env' = filter (\ (x, _) -> x /= v) env in
              if null env' then Right tm
              else do s' <- varSubstRec env' s
                      if s' == s 
                         then Right tm
                         else if any (\ (x, t) -> varFreeIn v t && 
                                                  varFreeIn x s) env'
                              then let v' = variant [s'] v in
                                     mkAbs v' =<< varSubstRec ((v, v'):env') s
                              else mkAbs v =<< varSubstRec env' s
        varSubstRec env (TyAbsIn tv t) = 
            mkTyAbs tv =<< varSubstRec env t
        varSubstRec env (TyCombIn t ty) = 
            liftM1 mkTyComb (varSubstRec env t) ty
        varSubstRec env tm@VarIn{} = Right $ lookupd tm env tm

{-|
  The @Inst@ class provides the framework for type instantiation in HaskHOL.
  Note that in the simplest cases, instantiation is simply a type substitution
  for the types of term variables and constants.  Therefore, instantiation is 
  constrained by the 'TypeSubst' class.

  The move to a polymorphic type system further complicates things as types can
  now be bound at the term level, requiring renaming for type instantiation.
  Since we have three different possible substitution environment types, we have
  three different possible instantiation environment types and, therefore, three
  different ways to handle renaming:

  * For @(x::'HOLType', r::'HOLType')@ substitution pairs we rename in the case 
    where a type abstraction binds a type variable present in @r@ and @x@ is
    present in the body of the type abstraction.

  * For @(_::'TypeOp', _::'TypeOp')@ substitution pairs we can safely ignore 
    renaming as our logic does not permit the binding of type operator 
    variables.

  * For @(x::'TypeOp', r::'HOLType')@ substitution pairs we rename in the case 
    where a type abstraction binds a type variable present in @r@ and @x@ is 
    present in the body of the type abstraction.

  Just as we did for the 'TypeSubst' class, we hide the internals of @Inst@ to
  prevent unsound re-definition.  The correct functions to call for
  type instantiation are 'inst' and 'instFull'.
-}
class TypeSubst a b => Inst a b where
    {-| 
      Handles the specific case of instantiating a type abstraction term.  This
      method is not exposed to the user.  Call the 'inst' or 'instFull' function
      instead.
    -}
    instTyAbs :: HOLTermEnv -> [(a, b)] -> HOLTerm -> Either HOLTerm HOLTerm

instance Inst HOLType HOLType where
    instTyAbs env tyenv tm@(TyAbsIn tv t) = 
        let tyenv' = filter (\ (x, _) -> x /= tv) tyenv in
          if null tyenv' then Right tm
          else if any (\ (x, r) -> tv `elem` tyVars r && 
                                   x `elem` typeVarsInTerm t) tyenv'
               -- avoid capture by renaming type variable
               then let tvt = typeVarsInTerm t
                        tvpatts = map fst tyenv'
                        tvrepls = catTyVars . mapMaybe (`lookup` tyenv') $
                                    tvt `intersect` tvpatts
                        tv' = variantTyVar ((tvt \\ tvpatts) `union` tvrepls) 
                                tv in
                      liftM (fromRight . mkTyAbs tv') $
                        instRec env ((tv, tv'):tyenv') t
               else liftM (fromRight . mkTyAbs tv) $ instRec env tyenv' t
    instTyAbs _ _ tm = Right tm

instance Inst TypeOp TypeOp where
    instTyAbs env tyenv (TyAbsIn tv t) = 
        liftM (fromRight . mkTyAbs tv) $ instRec env tyenv t
    instTyAbs _ _ tm = Right tm

instance Inst TypeOp HOLType where
    instTyAbs env tyenv (TyAbsIn tv t) =
        if any (\ (x, ty) -> tv `elem` tyVars ty && 
                             x `elem` typeOpVarsInTerm t) tyenv
        -- avoid capture by renaming type variable
        then let tvbs = typeOpVarsInTerm t
                 tvpatts = map fst tyenv
                 tvrepls = catTyVars . mapMaybe (`lookup` tyenv) $
                             tvbs `intersect` tvpatts
                 tv' = variantTyVar tvrepls tv in
               liftM (fromRight . mkTyAbs tv') . 
                 instRec env tyenv $ inst [(tv, tv')] t
        else liftM (fromRight . mkTyAbs tv) $ instRec env tyenv t
    instTyAbs _ _ tm = Right tm

{-|
  Type instantiation for terms.  Accepts the same types of substitution
  environments as discussed in the documentation for the 'TypeSubst' class, 
  with invalid substitution pairs being pruned internally by 'typeSubst' as 
  necessary.  

  For more information on why the 'Inst' class constraint is necessary and how 
  renaming of bound types is performed, see that classes documentation.
-}
inst :: Inst a b => [(a, b)] -> HOLTerm -> HOLTerm
inst [] tm = tm
inst theta tm = 
    case instRec [] theta tm of
      Right res -> res
      Left _ -> tm

-- Used internally by inst and instTyAbs both.  Not exposed to the user.
instRec :: Inst a b => HOLTermEnv -> [(a, b)] -> HOLTerm -> 
                       Either HOLTerm HOLTerm
instRec env tyenv tm@(VarIn n ty) =
    let tm' = mkVar n $ typeSubst tyenv ty in
      if lookupd tm' env tm == tm then Right tm' 
      else Left tm' -- Clash
instRec _ tyenv (ConstIn s ty tag) =
    let ty' = typeSubst tyenv ty in
      Right $ ConstIn s ty' tag
instRec env tyenv (CombIn f x) =
    do f' <- instRec env tyenv f
       x' <- instRec env tyenv x
       return . fromRight $ mkComb f' x'
instRec env tyenv (AbsIn y@(VarIn _ ty) t) =
    do y'<- instRec [] tyenv y
       case instRec ((y', y):env) tyenv t of
         Right t' -> return . fromRight $ mkAbs y' t'
         e@(Left w') -> 
             if w' /= y' then e
             else do ifrees <- mapM (instRec [] tyenv) $ frees t
                     case variant ifrees y' of
                       (VarIn x _) -> 
                           let z = mkVar x ty in
                             instRec env tyenv . fromRight $ 
                               mkAbs z #<< varSubst [(y, z)] t
                       _ -> e
instRec env tyenv tm@TyAbsIn{} = instTyAbs env tyenv tm
instRec env tyenv (TyCombIn tm ty) =
    do tm' <- instRec env tyenv tm
       let ty' = typeSubst tyenv ty
       return . fromRight $ mkTyComb tm' ty'
instRec _ _ AbsIn{} = error "instRec: bad term construction."

{-| 
  A version of 'inst' that accepts a triplet of type substitution environments.
-}
instFull :: SubstTrip -> HOLTerm -> HOLTerm
instFull (tyenv, tyOps, opOps) = inst opOps . inst tyOps . inst tyenv

{-|
  A simplified version of 'inst' that works only for term constants.  Fails with
  'Nothing' if the provided term is not a constant.  Used internally by 
  'mkConst' to guarantee that only constants are constructed.
-}
instConst :: TypeSubst a b => HOLTerm -> [(a, b)] -> Maybe HOLTerm
instConst (ConstIn s uty tag) tyenv = 
    let ty = typeSubst tyenv uty in
      Just $ ConstIn s ty tag
instConst _ _ = Nothing

{-| 
  A version of 'instConst' that accepts a triplet of type substitition 
  environments.
-}
instConstFull :: HOLTerm -> SubstTrip -> Maybe HOLTerm
instConstFull (ConstIn s uty tag) tyenv = 
    let ty = typeSubstFull tyenv uty in
      Just $ ConstIn s ty tag
instConstFull _ _ = Nothing

-- | Constructs an instance of the HOL equality constant, @=@, for a given type.
tmEq :: HOLType -> HOLTerm
tmEq ty = 
    ConstIn "=" (TyAppIn tyOpFun [ty, TyAppIn tyOpFun [ty, tyBool]]) 
      PrimitiveIn

-- | The pattern synonym equivalent of 'tmEq'.
pattern TmEq ty <- Const "=" (TyFun ty (TyFun _ TyBool))

-- | The infix pattern synonym for term equality.
pattern l := r <- Comb (Comb (Const "=" _) l) r

-- | Predicate for equations, i.e. terms of the form @l = r@.
isEq :: HOLTerm -> Bool
isEq (CombIn (CombIn (ConstIn "=" _ PrimitiveIn) _) _) = True
isEq _ = False

{-| 
  Constructs an equation term given the left and right hand side arguments.  
  Fails with 'Left' if the types of the terms are not alpha-equivalent.
-}
primMkEq :: HOLTerm -> HOLTerm -> Maybe HOLTerm
primMkEq l r
    | typeOf l `tyAConv` typeOf r =
        hush $ liftM1 mkComb (mkComb (tmEq $ typeOf l) l) r
    | otherwise = Nothing

{-|
  Destructs an equation term, returning the left and right hand side arguments.
  Fails with 'Nothing' if the term is not an equation, i.e. of the form @l = r@.
-}
destEq :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destEq (CombIn (CombIn (ConstIn "=" _ PrimitiveIn) l) r) =
    Just (l, r)
destEq _ = Nothing

{-|
  Renames a term variable to avoid sharing a name with any of a given list of
  term variables.  Returns the original term if it's not a term variable.
-}
variant :: [HOLTerm] -> HOLTerm -> HOLTerm
variant avoid v@(VarIn s ty)
    | any (varFreeIn v) avoid = 
          variant avoid $ mkVar (s `snoc` '\'') ty
    | otherwise = v
variant _ tm = tm

{-|
  Renames a list of term variables to avoid sharing a name with any of a given
  list of term variables.  As each term variable is processed it is added to
  the list of avoids such that the resultant list of term variables are all
  uniquely named.
-}
variants :: [HOLTerm] -> [HOLTerm] -> [HOLTerm]
variants _ [] = []
variants avoid (v:vs) = 
    let vh = variant avoid v in
      vh : variants (vh:avoid) vs

{- 
   Stateless HOL Term Primitives
-}

{-|  
  Constructs a primitive constant given a name and type.  Note that primitive
  constants are tagged with a @Primitive@ 'ConstTag' indicating that they have 
  no definition.
-} 
newPrimitiveConst :: Text -> HOLType -> HOLTerm
newPrimitiveConst name ty = ConstIn name ty PrimitiveIn

-- | Returns the list of all type operator variables in a term.
typeOpVarsInTerm :: HOLTerm -> [TypeOp]
typeOpVarsInTerm (VarIn _ ty) = typeOpVars ty
typeOpVarsInTerm (ConstIn _ ty _) = typeOpVars ty
typeOpVarsInTerm (CombIn s t) = 
    typeOpVarsInTerm s `union` typeOpVarsInTerm t
typeOpVarsInTerm (AbsIn bv t) = 
    typeOpVarsInTerm bv `union` typeOpVarsInTerm t
typeOpVarsInTerm (TyAbsIn _ t) = typeOpVarsInTerm t
typeOpVarsInTerm (TyCombIn t ty) = 
    typeOpVarsInTerm t `union` typeOpVars ty

-- | Returns the list of all type operator variables in a list of terms.
typeOpVarsInTerms :: [HOLTerm] -> [TypeOp]
typeOpVarsInTerms =
    foldr (\ tm topvs -> typeOpVarsInTerm tm `union` topvs) []

{- 
   HOL2P Term Primitives
-}
-- | Predicate for type abstraction terms.
isTyAbs :: HOLTerm -> Bool
isTyAbs TyAbsIn{} = True
isTyAbs _ = False

-- | Predicate for type combination terms.
isTyComb :: HOLTerm -> Bool
isTyComb TyCombIn{} = True
isTyComb _ = False

{-|
  Constructs a type abstraction term given a bound type and a body term.  Fails
  with 'Left' in the following cases:

  * The bound type is not a small type variable.
-}
mkTyAbs :: HOLType -> HOLTerm -> Either String HOLTerm
mkTyAbs tv@(TyVarIn True _) bod = Right $ TyAbsIn tv bod
mkTyAbs _ _ = Left "mkTyAbs: first argument not a small type variable."

{-|
  Constructs a type combination term given a body term and a type argument to 
  apply.  Fails with 'Left' in the following cases:

  * The type argument is not a small type.

  * The type of the body term is not a universal type.
-}
mkTyComb :: HOLTerm -> HOLType -> Either String HOLTerm
mkTyComb tm ty
    | isSmall ty =
        case typeOf tm of
          UTypeIn{} -> 
              Right $ TyCombIn tm ty
          _ -> Left "mkTyComb: term must have universal type."
    | otherwise =
        Left "mkTyComb: type argument not small."

{-| 
  Destructs a type abstraction, returning its bound type and body term.  Fails
  with 'Nothing' if the provided term is not a type abstraction.
-}
destTyAbs :: HOLTerm -> Maybe (HOLType, HOLTerm)
destTyAbs (TyAbsIn tv bod) = Just (tv, bod)
destTyAbs _ = Nothing

{-|
  Destructs a type combination, returning its body term and type argument.
  Fails with 'Nothing' if the provided term is not a type combination.
-}
destTyComb :: HOLTerm -> Maybe (HOLTerm, HOLType)
destTyComb (TyCombIn tm ty) = Just (tm, ty)
destTyComb _ = Nothing

-- Documentation copied from HaskHOL.Core.Prims

{-$ViewPatterns
  The primitive data types of HaskHOL are implemented using pattern synonyms in
  order to simulate private data types:

  * Internal constructors are hidden to prevent manual construction of terms.

  * Unidirectional pattern synonyms ('Var', etc.) are exposed to enable pattern 
    matching.
-}

{-$HOLTerms
  The following data types combined provide the definition of HOL terms in 
  HaskHOL.

  Corresponding with the 'HOLType' data type, 'HOLTerm' follows closely from
  the definition of terms in HOL Light.  Again, the appropriate modifications
  have been made to facilitate a semi-stateless and polymorphic term language.

  Most notably this includes:
  (1) The introduction of tags for constants to carry information formerly
      contained in the state.

  2.  Additional constructors have been added to 'HOLTerm' to facilitate
      term-level, type abstractions and applications.
-} 
