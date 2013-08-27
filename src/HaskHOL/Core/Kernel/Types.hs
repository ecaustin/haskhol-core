{-# LANGUAGE MultiParamTypeClasses #-}

{-|
  Module:    HaskHOL.Core.Kernel.Types
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
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
    , HOLTypeView(..)
       -- ** A Quick Note on Type Variable Distinction
        -- $TypeDistinction
    , TypeOp
    , HOLTypeEnv
    , SubstTrip
      -- * HOL Light Type Primitives
       -- ** Alpha-Equivalence of Types
    , tyAlphaOrder -- :: HOLType -> HOLType -> Ordering
    , tyAConv      -- :: HOLType -> HOLType -> Bool
       -- ** Predicates, Constructors, and Destructors for Basic Types
    , isVarType   -- :: HOLType -> Bool
    , isType      -- :: HOLType -> Bool
    , mkVarType   -- :: String -> HOLType
    , destVarType -- :: HOLType -> Maybe String
    , destType    -- :: HOLType -> Maybe (String, [HOLType])
       -- ** Type Variable Extractors
    , tyVars    -- :: HOLType -> [HOLType]
    , catTyVars -- :: [HOLType] -> [HOLType]
       -- ** Type Substitution
    , TypeSubst
    , typeSubst     --  :: TypeSubst a b => [(a, b)] -> HOLType -> HOLType
    , typeSubstFull --  :: SubstTrip -> HOLType -> HOLType
       -- ** Commonly Used Types and Functions
    , tyBool    -- :: HOLType
    , tyA       -- :: HOLType
    , tyB       -- :: HOLType
    , destFunTy -- :: HOLType -> Maybe (HOLType, HOLType)
    , typeOf    -- :: HOLTerm -> HOLType
      -- * Stateless HOL Type Primitives
       -- ** Predicates, Constructors, and Destructors for Type Operators
    , isTypeOpVar   -- :: TypeOp -> Bool
    , newPrimTypeOp -- :: String -> Int -> TypeOp
    , mkTypeOpVar   -- :: String -> TypeOp
    , destTypeOp    -- :: TypeOp -> (String, Int)
       -- ** Commonly Used Type Operators
    , tyOpBool -- :: TypeOp
    , tyOpFun  -- :: TypeOp
    , tyApp    -- :: TypeOp -> [HOLType] -> Either String HOLType
       -- ** Type Operator Variable Extractors
    , typeOpVars    -- :: HOLType -> [TypeOp]
    , catTypeOpVars -- :: [HOLType] -> [TypeOp]
      -- * HOL2P Type Primitives
       -- ** Predicates, Constructors, and Destructors for Universal Types
    , isUType            -- :: HOLType -> Bool
    , isSmall            -- :: HOLType -> Bool
    , mkUType            -- :: HOLType -> HOLType -> Either String HOLType
    , mkUTypes           -- :: [HOLType] -> HOLType -> Either String HOLType
    , uTypeFromTypeOpVar -- :: TypeOp -> Int -> Either String HOLType
    , mkSmall            -- :: HOLType -> Either String HOLType
    , destUType          -- :: HOLType -> Maybe (HOLType, HOLType)
    , destUTypes         -- :: HOLType -> Maybe ([HOLType], HOLType)
       -- ** Commonly Used Functions
    , containsUType -- :: HOLType -> Bool
    , variantTyVar  -- :: [HOLType] -> HOLType -> HOLType
    , variantTyVars -- :: [HOLType] -> [HOLType] -> HOLType
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
  where tyorda :: HOLTypeEnv -> HOLType -> HOLType -> Ordering
        tyorda env ty1 ty2
            | ty1 == ty2 && all (uncurry (==)) env = EQ
            | otherwise =
                case (ty1, ty2) of
                  (TyVarIn{}, TyVarIn{}) -> alphavars env ty1 ty2
                  (TyAppIn tyop1 args1, TyAppIn tyop2 args2) ->
                      case compare tyop1 tyop2 of
                        EQ -> tyordas env args1 args2
                        res -> res
                  (UTypeIn v1 t1, UTypeIn v2 t2) -> 
                      tyorda ((v1, v2):env) t1 t2
                  (TyVarIn{}, _) -> LT
                  (_, TyVarIn{}) -> GT
                  (TyAppIn{}, _) -> LT
                  (_, TyAppIn{}) -> GT

        alphavars :: HOLTypeEnv -> HOLType -> HOLType -> Ordering
        alphavars [] ty1 ty2 = compare ty1 ty2
        alphavars ((t1, t2):oenv) ty1 ty2
            | ty1 == t1 = if ty2 == t2 then EQ else LT
            | otherwise = if ty2 == t2 then GT else alphavars oenv ty1 ty2

        tyordas :: HOLTypeEnv -> [HOLType] -> [HOLType] -> Ordering
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
mkVarType :: String -> HOLType
mkVarType = TyVarIn False

{-| 
  Destructs a type variable, returning its name.  Fails with 'Nothing' if called
  on a non-variable type.
-}
destVarType :: HOLType -> Maybe String
destVarType (TyVarIn _ s) = Just s
destVarType _ = Nothing

{-| 
  Destructs a type application, returning its operator name and its list of type
  arguments.  Fails with 'Nothing' if called on a type that is not an 
  application.
-}
destType :: HOLType -> Maybe (TypeOp, [HOLType])
destType (TyAppIn op args) = Just (op, args)
destType _ = Nothing

{-| 
  Returns the list of all free, type variables in a type, not including type
  operator variables.
-}
tyVars :: HOLType -> [HOLType]
tyVars tv@TyVarIn{} = [tv]
tyVars (TyAppIn _ args) = catTyVars args
tyVars (UTypeIn tv ty) = tyVars ty \\ [tv]

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
    validSubst (_, TyOpVar{}) = False
    validSubst (TyOpVar{}, _) = True
    validSubst _ = False
    typeSubst' = typeOpSubst

instance TypeSubst TypeOp HOLType where
    validSubst (TyOpVar{}, UTypeIn{}) = True
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
    typeSubstRec (filter validSubst tyenv) t
  where typeSubstRec :: HOLTypeEnv -> HOLType -> HOLType
        typeSubstRec tyins ty@TyVarIn{} = assocd ty tyins ty
        typeSubstRec tyins (TyAppIn op args) =
            TyAppIn op $ map (typeSubstRec tyins) args
        typeSubstRec tyins ty@(UTypeIn tv tbody) =
            let tyins' = filter (\ (x, _) -> x /= tv) tyins in
              if null tyins' then ty
              -- test for name capture, renaming instances of tv if necessary
              else if any (\ (x, t') -> tv `elem` tyVars t' && 
                                        x `elem` tyVars tbody) tyins'
                   then let tvbs = tyVars tbody
                            tvpatts = map fst tyins'
                            tvrepls = catTyVars . mapMaybe (`lookup` tyins') $
                                        intersect tvbs tvpatts
                            tv' = variantTyVar ((tvbs \\ tvpatts) `union` 
                                                tvrepls) tv in
                          UTypeIn tv' $ typeSubstRec ((tv, tv') : tyins') tbody
                   else UTypeIn tv $ typeSubstRec tyins' tbody              
                                
-- | Alias to the primitive boolean type.
{-# INLINEABLE tyBool #-}
tyBool :: HOLType
tyBool = TyAppIn tyOpBool []

-- Used for error cases in type checking only.  Not exported.
{-# INLINEABLE tyBottom #-}
tyBottom :: HOLType
tyBottom = TyAppIn tyOpBottom []

-- | Alias to the unconstrained type variable @A@.
{-# INLINEABLE tyA #-}
tyA :: HOLType
tyA = TyVarIn False "A"

-- | Alias to the unconstrained type variable @B@.
{-# INLINEABLE tyB #-}
tyB :: HOLType
tyB = TyVarIn False "B"

{-| 
  Specialized version of 'destType' that returns the domain and range of a
  function type.  Fails with 'Nothing' if the type to be destructed isn't a
  primitive function type.
-}
destFunTy :: HOLType -> Maybe (HOLType, HOLType)
destFunTy (TyAppIn (TyPrim "fun" _) [ty1, ty2]) = Just (ty1, ty2)
destFunTy _ = Nothing

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
typeOf (AbsIn (VarIn _ ty) tm) =
    TyAppIn tyOpFun [ty, typeOf tm]
typeOf AbsIn{} = tyBottom
typeOf (TyAbsIn tv tb) = UTypeIn tv $ typeOf tb
typeOf (TyCombIn t ty) =
    case typeOf t of
      (UTypeIn tv tbody) -> typeSubst [(tv, ty)] tbody
      _ -> tyBottom

{-
   Stateless HOL Type Primitives
-}
-- | Predicate for type operator variables.
isTypeOpVar :: TypeOp -> Bool
isTypeOpVar TyOpVar{} = True
isTypeOpVar _ = False

{-| 
  Constructs a primitive type operator of a given name and arity.  Primitive
  type operators are used to represent constant, but undefined, types.
-}
newPrimTypeOp :: String -> Int -> TypeOp
newPrimTypeOp = TyPrim

{-|
  Constructs a type operator variable of a given name.  Note that type
  operator arities are not stored, only inferred from the context where the
  operator is used.

  The parser makes an attempt to guarantee that all instances of a type operator
  in a term have the same arity.  The same protection is not provided for terms
  that are manually constructed.
-}
mkTypeOpVar :: String -> TypeOp
mkTypeOpVar = TyOpVar

{-| 
  Destructs a type operator, returning its name and arity.  Note that we use -1 
  to indicate the arity of a type operator variable since that information is 
  not carried.
-}
destTypeOp :: TypeOp -> (String, Int)
destTypeOp (TyOpVar name) = (name, -1)
destTypeOp (TyPrim name arity) = (name, arity)
destTypeOp (TyDefined name arity _) = (name, arity)

-- | Alias to the primitive boolean type operator.
{-# INLINEABLE tyOpBool #-}
tyOpBool :: TypeOp
tyOpBool = TyPrim "bool" 0
-- Used for error cases in type checking only.  Not exported.
{-# INLINEABLE tyOpBottom #-}
tyOpBottom :: TypeOp
tyOpBottom = TyPrim "_|_" 0
-- | Alias to the primitive function type operator.
{-# INLINEABLE tyOpFun #-}
tyOpFun :: TypeOp
tyOpFun = TyPrim "fun" 2

{-|
  Constructs a type application from a provided type operator and list of type
  arguments.  Fails with 'Left' in the following cases:

  * A type operator variable is applied to zero arguments.

  * A type operator's arity disagrees with the length of the argument list.
-}
tyApp :: TypeOp -> [HOLType] -> Either String HOLType
tyApp tyOp@TyOpVar{} [] = 
    Left $ "tyApp: " ++ show tyOp ++ ": TyOpVar applied to zero args."
tyApp tyOp@TyOpVar{} args = Right $ TyAppIn tyOp args
tyApp tyOp args =
    let (_, arity) = destTypeOp tyOp in
      if arity == length args 
      then Right $ TyAppIn tyOp args
      else Left $ "tyApp: " ++ show tyOp ++ ": wrong number of arguments."

-- | Returns the list of all type operator variables in a type.
typeOpVars :: HOLType -> [TypeOp]
typeOpVars (TyAppIn op@TyOpVar{} args) = foldr (union . typeOpVars) [op] args
typeOpVars (UTypeIn _ tbody) = typeOpVars tbody
typeOpVars _ = []

-- | Returns the list of all type operator variables in a list of types.
catTypeOpVars :: [HOLType] -> [TypeOp]
catTypeOpVars = foldr (union . typeOpVars) []

-- substitution of type operator variables for other type operators.
-- Note that replacement with another type operator variable is allowed
typeOpSubst :: [(TypeOp, TypeOp)] -> HOLType -> HOLType
typeOpSubst [] t = t
typeOpSubst tyopenv t =
    tyOpSubstRec (filter validSubst tyopenv) t
  where tyOpSubstRec :: [(TypeOp, TypeOp)] -> HOLType -> HOLType
        tyOpSubstRec tyopins (TyAppIn op args) =
            let args' = map (tyOpSubstRec tyopins) args in
              case tryFind (\ (tp, tr) ->
                                if tp /= op ||
                                snd (destTypeOp tr) /= length args 
                                then Nothing
                                else Just tr) tyopins of
                Nothing -> TyAppIn op args'
                Just op' -> TyAppIn op' args'
        tyOpSubstRec tyopins (UTypeIn tv tbody) =
            UTypeIn tv $ tyOpSubstRec tyopins tbody
        tyOpSubstRec _ ty = ty

-- instantiation of type operator variables with universal types.
typeOpInst :: [(TypeOp, HOLType)] -> HOLType -> HOLType
typeOpInst [] t = t
typeOpInst tyopenv t = tyOpInstRec (filter validSubst tyopenv) t
  where arityOf :: HOLType -> Maybe Int
        arityOf ty = return (length . fst) <*> destUTypes ty      

        tyOpInstRec :: [(TypeOp, HOLType)] -> HOLType -> HOLType
        tyOpInstRec tyopins ty@(TyAppIn op args) =
            let args' = map (tyOpInstRec tyopins) args in
              case tryFind (\ (tp, tr) ->
                                if tp /= op || 
                                   arityOf tr /= (Just $ length args) 
                                then Nothing
                                else destUTypes tr) tyopins of
                Nothing -> TyAppIn op args'
                Just (rtvs, rtbody)
                    | isSmall rtbody -> typeSubst (zip rtvs args') rtbody
                    | otherwise -> ty
        tyOpInstRec tyopins (UTypeIn tv tbody) =
            if any (\ (x, ty) -> tv `elem` tyVars ty && 
                                 x `elem` typeOpVars tbody) tyopins
            -- test for name capture, renaming instances of tv if necessary 
            then let tvbs = typeOpVars tbody
                     tvpatts = map fst tyopins
                     tvrepls = catTyVars . mapMaybe (`lookup` tyopins) $
                                 intersect tvbs tvpatts
                     tv' = variantTyVar tvrepls tv in
                   UTypeIn tv' . tyOpInstRec tyopins $ 
                     typeSubst [(tv, tv')] tbody
            else UTypeIn tv $ tyOpInstRec tyopins tbody
        tyOpInstRec _ ty = ty
                  
                
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
mkUType :: HOLType -> HOLType -> Either String HOLType
mkUType tv@(TyVarIn True _) tybody = Right $ UTypeIn tv tybody
mkUType _ _ = Left "mkUType"

{-|
  Constructs a compound universal type given a list of bound types and a body.    Fails with 'Left' if any internal call to 'mkUType' fails.
-}
mkUTypes :: [HOLType] -> HOLType -> Either String HOLType
mkUTypes = flip (foldrM mkUType)

{-|
  Constructs a compound universal type from a type operator variable and a given
  number of bound variables, i.e. 
  
  > uTypeFromTypeOpVar _T n === % 'A1 ... 'An. ('A1, ..., 'An)_T  

  Fails with 'Left' in the following cases:

  * @n<=0@ which would result in the application of a type operator to an
    empty list of type arguments.

  * The type operator argument is not a variable. 
-}
uTypeFromTypeOpVar :: TypeOp -> Int -> Either String HOLType
uTypeFromTypeOpVar s@TyOpVar{} n
    | n > 0 = 
        let tvs = map (\ x -> TyVarIn True $ 'A' : show x) [1 .. n] in
          mkUTypes tvs =<< tyApp s tvs
    | otherwise = 
        Left "uTypeFromTypeOpVar: must have a positive number of bound types."
uTypeFromTypeOpVar _ _ = 
    Left "uTypeFromTypeOpVar: type operator not a variable."

{-|
  Constructs a small type from a given type by constraining all of the type
  variables in the type to be small.  Fails with 'Left' if the type contains
  any universal types.
-}
mkSmall :: HOLType -> Either String HOLType
mkSmall (TyVarIn _ s) = Right $ TyVarIn True s
mkSmall (TyAppIn s tvs) = liftM (TyAppIn s) $ mapM mkSmall tvs
mkSmall UTypeIn{} = Left "mkSmall"

{-| 
  Destructs a universal type, returning its bound type and body type.  Fails
  with 'Nothing' if the provided type is not universally quantified.
-}
destUType :: HOLType -> Maybe (HOLType, HOLType)
destUType (UTypeIn tv ty) = Just (tv, ty)
destUType _ = Nothing

{-|
  Destructs a compound universal type, returning the list of bound variables
  and the final body type.  Fails if the provided type is not universally
  quantified.
-} 
destUTypes :: HOLType -> Maybe ([HOLType], HOLType)
destUTypes (UTypeIn tv tb) = Just $ destUTypesRec ([tv], tb)
  where destUTypesRec :: ([HOLType], HOLType) -> ([HOLType], HOLType)
        destUTypesRec (acc, UTypeIn tv' tb') = destUTypesRec (acc++[tv'], tb')
        destUTypesRec res = res
destUTypes _ = Nothing

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
    | tv `elem` avoid = variantTyVar avoid . TyVarIn small $ name ++ "'"
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


-- Documentation copied from HaskHOL.Core.Prims

{-$ViewPatterns
  The primitive data types of HaskHOL are implemented using view patterns in
  order to simulate private data types:

  * Internal constructors are hidden to prevent manual construction of terms.

  * View constructors (those of 'HOLTypeView', 'HOLTermView', 'HOLThmView') are
    exposed to enable pattern matching. 

  * View patterns, as defined by instances of the 'view' function from the 
    @Viewable@ class, provide a conversion between the two sets of constructors.
-}

{-$HOLTypes
  The following data types combined provide the definition of HOL types in 
  HaskHOL.

  The primary data type, 'HOLType', follows closely from the 
  simply typed lambda calculus approach used in John Harrison's HOL Light 
  system. 

  There are two principle changes to Harrison's implementation:

  1.  Type operators have been introduced, via the 'TypeOp' data type, to 
      facilitate a stateless logical kernel following from Freek Wiedijk's 
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
