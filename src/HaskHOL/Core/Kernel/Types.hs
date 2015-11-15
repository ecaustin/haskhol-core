{-# LANGUAGE MultiParamTypeClasses, PatternSynonyms #-}

{-|
  Module:    HaskHOL.Core.Kernel.Types
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module exports a safe view of HOL types for HaskHOL.  It also defines
  the primitive functions related to types.  For clarity, these functions have
  been seperated based on their influential system: HOL Light, Stateless HOL,
  and HOL2P.

  Note that, per the stateless approach, any stateful, but still primitive,
  functions related to types have been relocated to the "HaskHOL.Core.State"
  module.
-}
module HaskHOL.Core.Kernel.Types
    ( -- * A View of HOL Types
       -- ** A Quick Note on View Patterns
        -- $ViewPatterns
       -- ** A High-Level Overview of HOL Types
        -- $HOLTypes
      HOLType
    , pattern TyVar
    , pattern TyApp
    , pattern UType
       -- ** A Quick Note on Type Variable Distinction
        -- $TypeDistinction
    , TypeOp
    , pattern TyOpVar
    , pattern TyPrimitive
    , pattern TyDefined
    , HOLTypeEnv
    , SubstTrip
      -- * HOL Light Type Primitives
       -- ** Alpha-Equivalence of Types
    , tyAlphaOrder
    , tyAConv
       -- ** Predicates, Constructors, and Destructors for Basic Types
    , isVarType
    , isType
    , mkVarType
    , destVarType
    , destType
       -- ** Type Variable Extractors
    , tyVars
    , catTyVars
       -- ** Type Substitution
    , TypeSubst
    , typeSubst
    , typeSubstFull
       -- ** Commonly Used Types and Functions
    , tyBool
    , pattern TyBool
    , pattern TyFun
    , pattern (:->)
    , tyA
    , pattern TyA
    , tyB
    , pattern TyB
    , destFunTy
    , typeOf
      -- * Stateless HOL Type Primitives
       -- ** Predicates, Constructors, and Destructors for Type Operators
    , isTypeOpVar
    , newPrimitiveTypeOp
    , mkTypeOpVar
    , destTypeOp
       -- ** Commonly Used Type Operators
    , tyOpBool
    , pattern TyOpBool
    , tyOpFun
    , pattern TyOpFun
    , tyApp
       -- ** Type Operator Variable Extractors
    , typeOpVars
    , catTypeOpVars
      -- * HOL2P Type Primitives
       -- ** Predicates, Constructors, and Destructors for Universal Types
    , isUType  
    , isSmall 
    , mkUType
    , mkUTypes 
    , uTypeFromTypeOpVar
    , mkSmall
    , destUType
    , destUTypes
       -- ** Commonly Used Functions
    , containsUType
    , variantTyVar
    , variantTyVars
    , initTypeConstants
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel.Prims

{- 
   HOL Light Type Primitives
   Note that the following primitives are in HaskHOL.Core.State as per 
   Stateless HOL:
   types, getTypeArityCtxt, getTypeArity, newType, mkType, mkFunTy
-}
-- | Provides an ordering for two types modulo alpha-equivalence.
tyAlphaOrder :: HOLType -> HOLType -> Ordering
tyAlphaOrder = tyorda []
  where tyorda :: [(HOLType, HOLType)] -> HOLType -> HOLType -> Ordering
        tyorda env ty1@TyVarIn{} ty2@TyVarIn{} = 
            alphavars env ty1 ty2
        tyorda env (TyAppIn tyop1 args1) (TyAppIn tyop2 args2) =
            case compare tyop1 tyop2 of
              EQ -> tyordas env args1 args2
              res -> res
        tyorda env (UTypeIn v1 t1) (UTypeIn v2 t2) = 
            tyorda ((v1, v2):env) t1 t2
        tyorda _ TyVarIn{} _ = LT
        tyorda _ _ TyVarIn{} = GT
        tyorda _ TyAppIn{} _ = LT
        tyorda _ _ TyAppIn{} = GT

        alphavars :: [(HOLType, HOLType)] -> HOLType -> HOLType -> Ordering
        alphavars [] x y = x `compare` y
        alphavars ((t1, t2):oenv) x y
            | x == t1 = if y == t2 then EQ else LT
            | otherwise = if y == t2 then GT else alphavars oenv x y

        tyordas :: [(HOLType, HOLType)] -> [HOLType] -> [HOLType] -> Ordering
        tyordas _ [] [] = EQ
        tyordas _ [] _ = LT
        tyordas _ _ [] = GT
        tyordas env (x:xs) (y:ys) =
            case tyorda env x y of
              EQ -> tyordas env xs ys
              res -> res

-- | Tests if two types are alpha-equivalent.
tyAConv :: HOLType -> HOLType -> Bool
tyAConv ty1 ty2 = tyAlphaOrder ty1 ty2 == EQ

-- | Predicate for type variables.
isVarType :: HOLType -> Bool
isVarType TyVarIn{} = True
isVarType _ = False

-- | Predicate for type applications
isType :: HOLType -> Bool
isType TyAppIn{} = True
isType _ = False

{-| 
  Constructs a type variable of a given name.  Note that the resultant type 
  variable is unconstrained.
-}
mkVarType :: Text -> HOLType
mkVarType = TyVarIn False

{-| 
  Destructs a type variable, returning its name.  Fails with 'Nothing' if called
  on a non-variable type.
-}
destVarType :: MonadThrow m => HOLType -> m Text
destVarType (TyVarIn _ s) = return s
destVarType ty = throwM $! HOLTypeError ty "destVarType"

{-| 
  Destructs a type application, returning its operator name and its list of type
  arguments.  Fails with 'Nothing' if called on a type that is not an 
  application.
-}
destType :: MonadThrow m => HOLType -> m (TypeOp, [HOLType])
destType (TyAppIn op args) = return (op, args)
destType ty = throwM $! HOLTypeError ty "destType"

{-| 
  Returns the list of all free, type variables in a type, not including type
  operator variables.
-}
tyVars :: HOLType -> [HOLType]
tyVars (UTypeIn tv ty) = 
    tyVars ty \\ [tv]  
tyVars (TyAppIn _ args) = catTyVars args
tyVars tv@TyVarIn{} = [tv] 

{-| 
  Returns the list of all type variables in a list of types, not including type
  operator variables.
-}
catTyVars :: [HOLType] -> [HOLType]
catTyVars = foldr (union . tyVars) []

{-|
  The @TypeSubst@ class provides the framework for type substitution in HaskHOL.
  Note that, with the introduction of universal types and type operator
  variables, we now have three kinds of substitution to handle:

  * Substitution of types for type variables, satisfying type variable 
    constraints.

  * Instantiation of type operators with universal types.

  * Substitution of type operators for type operator variables.

  Rather than have three separate functions exposed to the user, we elected to
  provide a polymorphic type substitution function that will accept any
  well-formed, homogenous substitution environment.

  Note that the internals of @TypeSubst@ are hidden to prevent unsound
  re-definition.  The relevant type substitution function is re-exported as
  'typeSubst'.  We also provide a function, 'typeSubstFull', that
  accepts a triplet of all possible substitution environments that can be
  conveniently used in combination with 'typeMatch'.

  See the ITP2013 paper, "Stateless Higher-Order Logic with Quantified Types,"
  for more details.
-}
class TypeSubst a b where
    -- | Tests if a pair is a valid element in a substitution environment.
    validSubst :: (a, b) -> Bool
    {-| 
      Perfoms a type substitution as described above using the provided 
      environment.
    -}
    typeSubst' :: [(a, b)] -> HOLType -> HOLType

instance TypeSubst HOLType HOLType where
    validSubst (TyVarIn False _, _) = True
    validSubst (TyVarIn{}, ty) = isSmall ty
    validSubst _ = False
    typeSubst' = typeTypeSubst

instance TypeSubst TypeOp TypeOp where
    validSubst (_, TyOpVarIn{}) = False
    validSubst (TyOpVarIn{}, _) = True
    validSubst _ = False
    typeSubst' = typeOpSubst

instance TypeSubst TypeOp HOLType where
    validSubst (TyOpVarIn{}, UTypeIn{}) = True
    validSubst _ = False
    typeSubst' = typeOpInst

{-|
  Re-exports the internal type substitution function of the 'TypeSubst' class
  to prevent unsound re-definition.  Invalid substitution pairs are pruned from
  the environment such that substitution never fails.

  Note that the order of elements in a substitution pair follows the convention
  of most Haskell libraries, rather than the traditional HOL convention:
  
  * The second element is substituted for the first, i.e. the substitution pair
    @(tyA, tyBool)@ indicates that the boolean type should be substituted for
    the type variable @A@.
-}
{-# INLINEABLE typeSubst #-}
typeSubst :: TypeSubst a b => [(a, b)] -> HOLType -> HOLType
typeSubst = typeSubst'

{-| 
  A version of 'typeSubst' that accepts a triplet of type substitution 
  environments.
-}
typeSubstFull :: SubstTrip -> HOLType -> HOLType
typeSubstFull (tyenv, tyOps, opOps) =
    typeOpSubst opOps . typeOpInst tyOps . typeSubst' tyenv

-- Type subst for (HOLType, HOLType) pairs.
typeTypeSubst :: HOLTypeEnv -> HOLType -> HOLType
typeTypeSubst [] t = t
typeTypeSubst tyenv t =
    case runCatch $ typeSubstRec (filter validSubst tyenv) t of
      Right res -> res
      _         -> t
  where typeSubstRec :: HOLTypeEnv -> HOLType -> Catch HOLType
        typeSubstRec tyins (TyAppIn op args) =
            tyApp op =<< mapM (typeSubstRec tyins) args
        typeSubstRec tyins ty@(UTypeIn tv tbody) =
            let tyins' = filter (\ (x, _) -> x /= tv) tyins in
              if null tyins' then return ty
              -- test for name capture, renaming instances of tv if necessary
              else if any (\ (x, t') -> tv `elem` tyVars t' && 
                                        x `elem` tyVars tbody) tyins'
                   then let tvbs = tyVars tbody
                            tvpatts = map fst tyins'
                            tvrepls = catTyVars . mapFilter (`assoc` tyins') $
                                        intersect tvbs tvpatts
                            tv' = variantTyVar ((tvbs \\ tvpatts) `union` 
                                                  tvrepls) tv in
                          mkUType tv' =<< typeSubstRec ((tv, tv'):tyins') tbody
                   else mkUType tv =<< typeSubstRec tyins' tbody
        typeSubstRec tyins ty@TyVarIn{} = return $! assocd ty tyins ty
                                
-- | Alias to the primitive boolean type.
{-# INLINEABLE tyBool #-}
tyBool :: HOLType
tyBool = TyAppIn tyOpBool []

-- | The pattern synonym equivalent of 'tyBool'.
pattern TyBool <- TyApp TyOpBool []

-- | The pattern synonym for easily matching on function types.
pattern TyFun ty1 ty2 <- TyApp TyOpFun [ty1, ty2] 

-- | An infix alias to 'TyFun'.
pattern ty1 :-> ty2 <- TyFun ty1 ty2

-- Used for error cases in type checking only.  Not exported.
{-# INLINEABLE tyBottom #-}
tyBottom :: HOLType
tyBottom = try' $! tyApp tyOpBottom []

-- | Alias to the unconstrained type variable @A@.
{-# INLINEABLE tyA #-}
tyA :: HOLType
tyA = TyVarIn False "A"

-- | The pattern synonym equivalent of 'tyA'.
pattern TyA <- TyVar False "A"

-- | Alias to the unconstrained type variable @B@.
{-# INLINEABLE tyB #-}
tyB :: HOLType
tyB = TyVarIn False "B"

-- | The pattern synonym equivalent of 'tyB'.
pattern TyB <- TyVar False "B"

{-| 
  Specialized version of 'destType' that returns the domain and range of a
  function type.  Fails with 'Nothing' if the type to be destructed isn't a
  primitive function type.
-}
destFunTy :: MonadThrow m => HOLType -> m (HOLType, HOLType)
destFunTy (TyAppIn (TyPrimitiveIn "fun" _) [ty1, ty2]) = 
    return (ty1, ty2)
destFunTy ty = throwM $! HOLTypeError ty "destFunTy"

{-|
  Returns the type of term.  Fails with a special type, @tyBottom@, if the type
  is poorly constructed; this keeps the function total without requiring the
  use of an additional guard type like 'Maybe'.

  In practice, this type will never be seen provided the kernel is not modified
  to expose the internal constructors for terms.
-}
typeOf :: HOLTerm -> HOLType
typeOf (VarIn _ ty) = ty
typeOf (ConstIn _ ty _) = ty
typeOf (CombIn x _) =
    case destType $ typeOf x of
      Just (_, _ : ty : _) -> ty
      _ -> tyBottom
typeOf (AbsIn (VarIn _ ty) t) =
    try' $! tyApp tyOpFun [ty, typeOf t]
typeOf AbsIn{} = tyBottom
typeOf (TyAbsIn tv tb) = 
    try' $! mkUType tv $ typeOf tb
typeOf (TyCombIn t ty) =
    case typeOf t of
      (UTypeIn tv tbody) -> 
          typeSubst [(tv, ty)] tbody
      _ -> tyBottom


{-
   Stateless HOL Type Primitives
-}
-- | Predicate for type operator variables.
isTypeOpVar :: TypeOp -> Bool
isTypeOpVar TyOpVarIn{} = True
isTypeOpVar _ = False

{-| 
  Constructs a primitive type operator of a given name and arity.  Primitive
  type operators are used to represent constant, but undefined, types.
-}
newPrimitiveTypeOp :: Text -> Int -> TypeOp
newPrimitiveTypeOp = TyPrimitiveIn

{-|
  Constructs a type operator variable of a given name.  Note that type
  operator arities are not stored, only inferred from the context where the
  operator is used.

  The parser makes an attempt to guarantee that all instances of a type operator
  in a term have the same arity.  The same protection is not provided for terms
  that are manually constructed.
-}
mkTypeOpVar :: Text -> TypeOp
mkTypeOpVar = TyOpVarIn

{-| 
  Destructs a type operator, returning its name and arity.  Note that we use -1 
  to indicate the arity of a type operator variable since that information is 
  not carried.
-}
destTypeOp :: TypeOp -> (Text, Int)
destTypeOp (TyOpVarIn name) = (name, -1)
destTypeOp (TyPrimitiveIn name arity) = (name, arity)
destTypeOp (TyDefinedIn name arity _) = (name, arity)

-- | Alias to the primitive boolean type operator.
{-# INLINEABLE tyOpBool #-}
tyOpBool :: TypeOp
tyOpBool = TyPrimitiveIn "bool" 0

-- | The pattern synonym equivalent of 'tyOpBool'.
pattern TyOpBool <- TyPrimitive "bool" 0

-- Used for error cases in type checking only.  Not exported.
{-# INLINEABLE tyOpBottom #-}
tyOpBottom :: TypeOp
tyOpBottom = TyPrimitiveIn "_|_" 0

-- | Alias to the primitive function type operator.
{-# INLINEABLE tyOpFun #-}
tyOpFun :: TypeOp
tyOpFun = TyPrimitiveIn "fun" 2

-- | The pattern synonym equivalent of 'tyOpFun'.
pattern TyOpFun <- TyPrimitive "fun" 2

{-|
  Constructs a type application from a provided type operator and list of type
  arguments.  Fails with 'Left' in the following cases:

  * A type operator variable is applied to zero arguments.

  * A type operator's arity disagrees with the length of the argument list.
-}
tyApp :: MonadThrow m => TypeOp -> [HOLType] -> m HOLType
tyApp tyOp@TyOpVarIn{} [] = 
    throwM $ HOLTypeOpError tyOp "tyApp: TyOpVar applied to zero args."
tyApp tyOp@TyOpVarIn{} args = return $! TyAppIn tyOp args
tyApp tyOp args =
    let (_, arity) = destTypeOp tyOp in
      if arity == length args 
      then return $! TyAppIn tyOp args
      else throwM $ HOLTypeOpError tyOp "tyApp: wrong number of arguments."

-- | Returns the list of all type operator variables in a type.
typeOpVars :: HOLType -> [TypeOp]
typeOpVars (TyAppIn op@TyOpVarIn{} args) = 
    foldr (union . typeOpVars) [op] args
typeOpVars (TyAppIn _ args) =
    concatMap typeOpVars args
typeOpVars (UTypeIn _ tbody) = 
    typeOpVars tbody
typeOpVars TyVarIn{} = []

-- | Returns the list of all type operator variables in a list of types.
catTypeOpVars :: [HOLType] -> [TypeOp]
catTypeOpVars = foldr (union . typeOpVars) []

-- substitution of type operator variables for other type operators.
-- Note that replacement with another type operator variable is allowed
typeOpSubst :: [(TypeOp, TypeOp)] -> HOLType -> HOLType
typeOpSubst [] t = t
typeOpSubst tyopenv t =
    case runCatch $ tyOpSubstRec (filter validSubst tyopenv) t of
      Right res -> res
      _         -> t
  where tyOpSubstRec :: [(TypeOp, TypeOp)] -> HOLType -> Catch HOLType
        tyOpSubstRec tyopins (TyAppIn op args) =
            do args' <- mapM (tyOpSubstRec tyopins) args
               case runCatch $ tryFind (\ (tp, tr) ->
                      if tp /= op || snd (destTypeOp tr) /= length args
                      then fail' "tyOpSubstRec"
                      else return tr) tyopins of
                 Left{}    -> tyApp op args'
                 Right op' -> tyApp op' args'
        tyOpSubstRec tyopins (UTypeIn tv tbody) =
            mkUType tv =<< tyOpSubstRec tyopins tbody
        tyOpSubstRec _ ty@TyVarIn{} = return ty

-- instantiation of type operator variables with universal types.
typeOpInst :: [(TypeOp, HOLType)] -> HOLType -> HOLType
typeOpInst [] t = t
typeOpInst tyopenv t = 
    case runCatch $ tyOpInstRec (filter validSubst tyopenv) t of
      Right res -> res
      _         -> t
  where arityOf :: (MonadCatch m, MonadThrow m) => HOLType -> m Int
        arityOf ty = liftM (length . fst) $ destUTypes ty      

        tyOpInstRec :: [(TypeOp, HOLType)] -> HOLType -> Catch HOLType
        tyOpInstRec tyopins ty@(TyAppIn op args) =
            do args' <- mapM (tyOpInstRec tyopins) args
               case runCatch $ tryFind (\ (tp, tr) ->
                      do n <- arityOf tr
                         if tp /= op || n /= length args
                            then fail' "tyOpInstRec"
                            else destUTypes ty) tyopins of
                 Left{} -> tyApp op args'
                 Right (rtvs, rtbody)
                     | isSmall rtbody ->
                         return $ typeSubst (zip rtvs args') rtbody
                     | otherwise -> return ty
        tyOpInstRec tyopins (UTypeIn tv tbody) =
            if any (\ (x, ty) -> tv `elem` tyVars ty && 
                                 x `elem` typeOpVars tbody) tyopins
            -- test for name capture, renaming instances of tv if necessary 
            then let tvbs = typeOpVars tbody
                     tvpatts = map fst tyopins
                     tvrepls = catTyVars . mapFilter (`assoc` tyopins) $
                                 intersect tvbs tvpatts
                     tv' = variantTyVar tvrepls tv
                     tbody' = typeSubst [(tv, tv')] tbody in
                   mkUType tv' =<< tyOpInstRec tyopins tbody'
            else mkUType tv =<< tyOpInstRec tyopins tbody
        tyOpInstRec _ ty@TyVarIn{} = return ty                  
                
{- 
   HOL2P Type Primitives
-}

-- | Predicate for universal types.
isUType :: HOLType -> Bool
isUType UTypeIn{} = True
isUType _ = False

{-|
  Predicate for small types.  Returns 'True' if all type variables in the type
  are constrained to be small and the type contains no universal types; returns 
  'False' otherwise. 
-}
isSmall :: HOLType -> Bool
isSmall (TyVarIn small _) = small
isSmall (TyAppIn _ args) = all isSmall args
isSmall UTypeIn{} = False

{-|
  Constructs a universal type of a given bound type and body type.  Fails with
  'Left' if the bound type is not a small, type variable.
-}
mkUType :: MonadThrow m => HOLType -> HOLType -> m HOLType
mkUType tv@(TyVarIn True _) tybody = 
    return $! UTypeIn tv tybody
mkUType tv _ = throwM $! HOLTypeError tv "mkUType"

{-|
  Constructs a compound universal type given a list of bound types and a body.    Fails with 'Left' if any internal call to 'mkUType' fails.
-}
mkUTypes :: (MonadCatch m, MonadThrow m) => [HOLType] -> HOLType -> m HOLType
mkUTypes vs b = foldrM mkUType b vs <?> "mkUTypes"

{-|
  Constructs a compound universal type from a type operator variable and a given
  number of bound variables, i.e. 
  
  > uTypeFromTypeOpVar _T n === % 'A1 ... 'An. ('A1, ..., 'An)_T  

  Fails with 'Left' in the following cases:

  * @n<=0@ which would result in the application of a type operator to an
    empty list of type arguments.

  * The type operator argument is not a variable. 
-}
uTypeFromTypeOpVar :: (MonadCatch m, MonadThrow m) => TypeOp -> Int -> m HOLType
uTypeFromTypeOpVar s@TyOpVarIn{} n
    | n > 0 = 
        let tvs = map (\ x -> TyVarIn True . pack $ 'A' : show x) 
                    [1 .. n] in
          do ty <- tyApp s tvs
             mkUTypes tvs ty
    | otherwise = throwM $! HOLMiscError n
          "uTypeFromTypeOpVar: must have a positive number of bound types."
uTypeFromTypeOpVar s _ = throwM $! HOLTypeOpError s 
    "uTypeFromTypeOpVar: type operator not a variable."

{-|
  Constructs a small type from a given type by constraining all of the type
  variables in the type to be small.  Fails with 'Left' if the type contains
  any universal types.
-}
mkSmall :: MonadThrow m => HOLType -> m HOLType
mkSmall (TyVarIn _ s) = 
    return $! TyVarIn True s
mkSmall (TyAppIn op tvs) = 
    liftM (TyAppIn op) $! mapM mkSmall tvs
mkSmall ty@UTypeIn{} = throwM $! HOLTypeError ty "mkSmall"

{-| 
  Destructs a universal type, returning its bound type and body type.  Fails
  with 'Nothing' if the provided type is not universally quantified.
-}
destUType :: MonadThrow m => HOLType -> m (HOLType, HOLType)
destUType (UTypeIn tv ty) = return (tv, ty)
destUType ty = throwM $! HOLTypeError ty "destUType"

{-|
  Destructs a compound universal type, returning the list of bound variables
  and the final body type.  Fails if the provided type is not universally
  quantified.
-} 
destUTypes :: MonadThrow m => HOLType -> m ([HOLType], HOLType)
destUTypes (UTypeIn tv tb) = 
    let (tvs, tb') = destUTypesRec ([tv], tb) in
      return (tvs, tb')
  where destUTypesRec :: ([HOLType], HOLType) -> ([HOLType], HOLType)
        destUTypesRec (acc, UTypeIn tv' tb') = 
            destUTypesRec (acc++[tv'], tb')
        destUTypesRec res = res
destUTypes tm = throwM $! HOLTypeError tm "destUTypes"

-- | Predicate to test if a type contains a universal type at any level.
containsUType :: HOLType -> Bool
containsUType TyVarIn{} = False
containsUType (TyAppIn _ args) = any containsUType args
containsUType UTypeIn{} = True

{-|
  Renames a type variable to avoid sharing a name with any of a given list of
  type variables.  Note that this function is both smallness presserving and
  respecting.  Returns the original type if it's not a type variable.
-}
variantTyVar :: [HOLType] -> HOLType -> HOLType
variantTyVar avoid tv@(TyVarIn small name)
    | tv `elem` avoid = 
          variantTyVar avoid (TyVarIn small $ name `snoc` '\'')
    | otherwise = tv
variantTyVar _ ty = ty

{-|
  Renames a list of type variables to avoid sharing a name with any of a given
  list of type variables.  As each type variable is processed it is added to the
  list of avoids such that the resultant list of type variables are all uniquely
  named.
-}
variantTyVars :: [HOLType] -> [HOLType] -> [HOLType]
variantTyVars _ [] = []
variantTyVars avoid (tv:tvs) =
    let tv' = variantTyVar avoid tv in
      tv : variantTyVars (tv':avoid) tvs

-- | The initial type constants.
initTypeConstants :: Map Text TypeOp
initTypeConstants = mapFromList [("bool", tyOpBool), ("fun", tyOpFun)]

-- Documentation copied from HaskHOL.Core.Prims

{-$ViewPatterns
  The primitive data types of HaskHOL are implemented using pattern synonyms in
  order to simulate private data types:

  * Internal constructors are hidden to prevent manual construction of terms.

  * Unidirectional pattern synonyms ('TyVar', etc.) are exposed to enable 
    pattern matching. 
-}

{-$HOLTypes
  The following data types combined provide the definition of HOL types in 
  HaskHOL.

  The primary data type, 'HOLType', follows closely from the 
  simply typed lambda calculus approach used in John Harrison's HOL Light 
  system. 

  There are two principle changes to Harrison's implementation:

  1.  Type operators have been introduced, via the 'TypeOp' data type, to 
      facilitate a semi-stateless logical kernel following from Freek Wiedijk's 
      Stateless HOL system.

  2.  Universal types and type operator variables have been introduced to move
      the logic from simply typed to polymorphic following from Norbert 
      Voelker's HOL2P system.
-}

{-$TypeDistinction
  In order to keep HaskHOL's type system decidable, we follow the same 
  \"smallness\" constraint used by HOL2P: type variables that are constrained 
  to be small cannot be replaced with types that contain either universal types
  or unconstrained type variables.  This constraint, in addition to the
  restriction that universal types can only bind small type variables, prevents
  the system from performing a substitution that would result in a higher rank
  type than the system is capable of dealing with.  This effectively limits the
  type system to 2nd order polymorphism.

  Voelker elected to rely on syntactic distinction to differentiate between the
  many kinds of type variables (small, unconstrained, and operator); depending 
  on how it was to be used, the name of a variable was prepended with a special 
  symbol.  Internal to HaskHOL, we elected to replace these syntactic 
  distinctions with structural ones such that the following hold true:

  * @TyVarIn True \"x\"@ represents the small type variable \"\'x\"

  * @TyVarIn False \"x\"@ represents the unconstrainted type variable \"x\"

  * @TyOpVar \"x\"@ represents the type operator variable \"_x\"

  Note that external to HaskHOL, during I/O of terms, both the parser and
  pretty-printer still rely on the syntactic distinctions introduced by
  Voelker.
-} 
