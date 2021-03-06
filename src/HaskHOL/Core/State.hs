{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, 
             ImplicitParams, MultiParamTypeClasses, TypeFamilies #-}
{-|
  Module:    HaskHOL.Core.State
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module exports the stateful layer of HaskHOL.  It consists of:

  * Stateful type primitives not found in "HaskHOL.Core.Types".

  * Stateful term primitives not found in "HaskHOL.Core.Terms".

  * Stateful theory extension primitives not found in "HaskHOL.Core.Kernel".

  * A very primitive debugging system.
-}
module HaskHOL.Core.State
    ( -- * Stateful Type Primitives
      types
    , tyDefinitions
    , getTypeArity
    , newType
    , mkType
    , mkFunTy
    -- * Stateful Term Primitives
    , constants
    , getConstType
    , newConstant
    , mkConst
    , mkConst_FULL
    , mkConst_NIL
    , genVarWithName
    , genVar
    , genSmallTyVar
    -- * Stateful Theory Extension Primitives
    , axioms
    , newAxiom
    , getAxiom
    , definitions
    , newBasicDefinition
    , getBasicDefinition
    , newBasicTypeDefinition
    , getBasicTypeDefinition
    -- * Primitive Debugging System
    , FlagDebug(..)
    , warn
    , printDebugLn
    , printDebug
      -- * Stateful Re-Exports and Overloadings
    , module HaskHOL.Core.State.Monad
    , module HaskHOL.Core.Overloadings
    , mkMConst
    , listMkIComb
    , mkBinary
    , mkBinder
    , mkTyBinder
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
    , mkGAbs
    , listMkGAbs
    , mkLet
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel hiding (destEq, typeOf, mkVar)
import HaskHOL.Core.State.Monad hiding
  (typeAbbrevs, infixes, prefixes, tyBinders, binders)
import HaskHOL.Core.Parser.Rep

import HaskHOL.Core.Overloadings hiding
  (getConstType, mkConst_NIL, mkConst_FULL, mkConst, mkFunTy, mkType,
   mkMConst, listMkIComb, mkBinary, mkBinder, mkTyBinder, mkIff, mkConj, mkImp, 
   mkForall, mkExists, mkDisj, mkNeg, mkUExists, mkTyAll, mkTyEx, listMkConj, 
   listMkDisj, listMkForall, listMkExists, mkGAbs, listMkGAbs, mkLet)
import qualified HaskHOL.Core.Overloadings as O

-- New flags and extensions
-- | Flag states whether or not to print debug statements.
newFlag "FlagDebug" True


data TypeConstants = TypeConstants !(Map Text TypeOp) deriving Typeable

deriveSafeCopy 0 'base ''TypeConstants

insertTypeConstant :: Text -> TypeOp -> Update TypeConstants ()
insertTypeConstant ty op =
    do TypeConstants m <- get
       put (TypeConstants (mapInsert ty op m))

getTypeConstants ::  Query TypeConstants (Map Text TypeOp)
getTypeConstants =
    do TypeConstants m <- ask
       return m

makeAcidic ''TypeConstants 
    ['insertTypeConstant, 'getTypeConstants]


data TypeDefinitions = TypeDefinitions !(Map Text (HOLThm, HOLThm)) 
    deriving Typeable

deriveSafeCopy 0 'base ''TypeDefinitions

insertTypeDefinition :: Text -> (HOLThm, HOLThm) -> Update TypeDefinitions ()
insertTypeDefinition ty defs =
    do TypeDefinitions m <- get
       put (TypeDefinitions (mapInsert ty defs m))

getTypeDefinitions :: Query TypeDefinitions (Map Text (HOLThm, HOLThm))
getTypeDefinitions  =
    do TypeDefinitions m <- ask
       return m

getTypeDefinition :: Text -> Query TypeDefinitions (Maybe (HOLThm, HOLThm))
getTypeDefinition lbl =
    do (TypeDefinitions m) <- ask
       return $! mapAssoc lbl m 

makeAcidic ''TypeDefinitions 
    ['insertTypeDefinition, 'getTypeDefinitions, 'getTypeDefinition]


data TermConstants = TermConstants !(Map Text HOLTerm) deriving Typeable

deriveSafeCopy 0 'base ''TermConstants

insertTermConstant :: Text -> HOLTerm -> Update TermConstants ()
insertTermConstant tm op =
    do TermConstants m <- get
       put (TermConstants (mapInsert tm op m))

getTermConstants :: Query TermConstants (Map Text HOLTerm)
getTermConstants =
    do TermConstants m <- ask
       return m

makeAcidic ''TermConstants 
    ['insertTermConstant, 'getTermConstants]

data TheAxioms = TheAxioms !(Map Text HOLThm) deriving Typeable

deriveSafeCopy 0 'base ''TheAxioms

insertAxiom :: Text -> HOLThm -> Update TheAxioms ()
insertAxiom lbl thm =
    do TheAxioms m <- get
       put (TheAxioms (mapInsert lbl thm m))

getAxioms :: Query TheAxioms (Map Text HOLThm)
getAxioms =
    do TheAxioms m <- ask
       return m

getAxiom' :: Text -> Query TheAxioms (Maybe HOLThm)
getAxiom' lbl =
    do TheAxioms m <- ask
       return $! mapAssoc lbl m

makeAcidic ''TheAxioms ['insertAxiom, 'getAxioms, 'getAxiom']


data TheCoreDefinitions = 
    TheCoreDefinitions !(Map Text HOLThm) deriving Typeable

deriveSafeCopy 0 'base ''TheCoreDefinitions

insertCoreDefinition :: Text -> HOLThm -> Update TheCoreDefinitions ()
insertCoreDefinition lbl thm =
    do TheCoreDefinitions defs <- get
       put (TheCoreDefinitions (mapInsert lbl thm defs))

getCoreDefinitions :: Query TheCoreDefinitions [HOLThm]
getCoreDefinitions =
    do TheCoreDefinitions defs <- ask
       return $! mapElems defs

getCoreDefinition :: Text -> Query TheCoreDefinitions (Maybe HOLThm)
getCoreDefinition name =
    do (TheCoreDefinitions defs) <- ask
       return $! name `mapAssoc` defs

makeAcidic ''TheCoreDefinitions 
    ['insertCoreDefinition, 'getCoreDefinitions, 'getCoreDefinition]


-- Term and Type Generation
{-|  
  Generates a new term variable consisting of a given prefix and the next value
  in the fresh term counter.
-}
genVarWithName :: HOLTypeRep ty cls thry => Text -> ty -> HOL cls thry HOLTerm
genVarWithName n ty =
    do count <- tickTermCounter
       mkVar (n `append` textShow count) ty

-- | A version of 'genVarWithName' that defaults to the prefix \"_\".
genVar :: HOLTypeRep ty cls thry => ty -> HOL cls thry HOLTerm
genVar = genVarWithName "_"

{-|
  Generates a new small, type variable with a name built using the fresh type
  counter.
-}
genSmallTyVar :: HOL cls thry HOLType
genSmallTyVar =
    do count <- tickTypeCounter
       (mkSmall . mkVarType $ '_' `cons` textShow count) <?> "genSmallTyVar"

-- Stateful HOL Light Type Primitives
{-|
  Retrieves the 'Map' of type constants from the current working theory.  The
  mapping pairs strings recognized by the parser with the associated
  type operator value, i.e. 

  > ("bool", tyOpBool)
-}
types :: HOL cls thry (Map Text TypeOp)
types =
    do acid <- openLocalStateHOL (TypeConstants initTypeConstants)
       m <- queryHOL acid GetTypeConstants
       closeAcidStateHOL acid
       return m

-- | Retrieves the 'Map' of type definitions from the current working theory.
tyDefinitions :: HOL cls thry (Map Text (HOLThm, HOLThm))
tyDefinitions =
    do acid <- openLocalStateHOL (TypeDefinitions mapEmpty)
       m <- queryHOL acid GetTypeDefinitions
       closeAcidStateHOL acid
       return m

{-|
  Returns the arity associated with a type constant.
  Throws a 'HOLException' if the provided type constant
  name is not defined.
-}
getTypeArity :: Text -> HOL cls thry Int
getTypeArity name =
    do tys <- types
       ((snd . destTypeOp) `fmap` mapAssoc name tys) <?>
         "getTypeArity: name has not been defined."

{- 
  Primitive type constant construction function.  Used by newType and 
  newBasicTypeDefinition.  Not exposed to the user.
-}
newType' :: Text -> TypeOp -> HOL Theory thry ()
newType' name tyop =
    do failWhen (can getTypeArity name) $
         "newType: type " ++ show name ++ " has already been declared."
       acid <- openLocalStateHOL (TypeConstants initTypeConstants)
       updateHOL acid (InsertTypeConstant name tyop)
       closeAcidStateHOL acid
       overParseContext typeConstants (mapInsert name tyop)

{-| 
  Constructs a new primitve type constant of a given name and arity.  Also adds
  this new type to the current working theory.  Throws a 'HOLException' when a 
  type of the same name has already been declared.
-}
newType :: Text -> Int -> HOL Theory thry ()
newType name arity = 
    newType' name $ newPrimitiveTypeOp name arity

{-|
  Constructs a type application given an operator name and a list of argument
  types.  If the provided name is not a currently defined type constant then
  this function defaults it to a type operator variable.  Throws a 
  'HOLException' in the following cases:

  * A type operator's arity disagrees with the length of the argument list.

  * A type operator is applied to zero arguments.
-}
mkType :: HOLTypeRep ty cls thry => Text -> [ty] -> HOL cls thry HOLType
mkType op = let ?typesFun = types in overload1 (O.mkType op)

{-|
  Constructs a function type safely using 'mkType'.  Should never fail provided
  that the initial value for type constants has not been modified.
-}
mkFunTy :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
        => ty1 -> ty2 -> HOL cls thry HOLType
mkFunTy = let ?typesFun = types in overload2 O.mkFunTy

-- State for Constants
{-|
  Retrieves the 'Map' of term constants from the current working theory.  The
  mapping pairs strings recognized by the parser and the associated
  term constant value, i.e. 

  > ("=", tmEq tyA)
-}
constants :: HOL cls thry (Map Text HOLTerm)
constants =
    do acid <- openLocalStateHOL (TermConstants initTermConstants)
       m <- queryHOL acid GetTermConstants
       closeAcidStateHOL acid
       return m

{-|
  Retrieves the type of a given term constant.  Throws a 'HOLException' if the
  provided term constant name is not defined.
-}
getConstType :: Text -> HOL cls thry HOLType
getConstType = let ?constsFun = constants in O.getConstType

{-
  Primitive term constant construction function.  Used by newConstant,
  newBasicDefinition, and newBasicTypeDefinition.
-}
newConstant' :: Text -> HOLTerm -> HOL Theory thry ()
newConstant' name c =
    do failWhen (can getConstType name) $
         "newConstant: constant " ++ show name ++ " has already been declared."
       acid <- openLocalStateHOL (TermConstants initTermConstants)
       updateHOL acid (InsertTermConstant name c)
       closeAcidStateHOL acid
       overParseContext termConstants (mapInsert name c)

{-|
  Constructs a new primitive term constant of a given name and type.  Also adds
  this new term to the current working theory.  Throws a 'HOLException' when a
  term of the same name has already been declared.
-}
newConstant :: HOLTypeRep ty Theory thry => (Text, ty) -> HOL Theory thry ()
newConstant (name, pty) =
    do cond <- can getConstType name
       if cond
          then printDebugLn ("newConstant: ignoring redefintion of " ++ 
                             show name) $ return ()
          else do ty <- toHTy pty
                  newConstant' name $ newPrimitiveConst name ty

class TypeSubstHOL a b cls thry where
    mkConstHOL :: (?constsFun :: HOL cls thry (Map Text HOLTerm)) 
               => Text -> [(a, b)] -> HOL cls thry HOLTerm

instance (HOLTypeRep l cls thry, HOLTypeRep r cls thry) => 
         TypeSubstHOL l r cls thry where
    mkConstHOL op = O.mkConst op <=< mapM (toHTy `ffCombM` toHTy)

instance HOLTypeRep r cls thry => TypeSubstHOL TypeOp r cls thry where
    mkConstHOL op = O.mkConst op <=< mapM (return `ffCombM` toHTy)

instance TypeSubstHOL TypeOp TypeOp cls thry where
    mkConstHOL = O.mkConst

{-|
  Constructs a specific instance of a term constant when provided with its name
  and a type substition environment.  Throws a 'HOLException' in the 
  following cases:

  * The instantiation as performed by 'instConst' fails.

  * The provided name is not a currently defined constant.
-}
mkConst :: TypeSubstHOL l r cls thry 
        => Text -> [(l, r)] -> HOL cls thry HOLTerm
mkConst = let ?constsFun = constants in mkConstHOL

{-| 
  A version of 'mkConst' that accepts a triplet of type substitition 
  environments.  Frequently used with the 'typeMatch' function.
-}
mkConst_FULL :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry,
                 HOLTypeRep ty3 cls thry)
             => Text -> ([(ty1, ty2)], [(TypeOp, ty3)], [(TypeOp, TypeOp)]) 
             -> HOL cls thry HOLTerm
mkConst_FULL op = let ?constsFun = constants in overload1 (O.mkConst_FULL op)
         
mkConst_NIL ::Text -> HOL cls thry HOLTerm
mkConst_NIL = let ?constsFun = constants in O.mkConst_NIL

-- State for Axioms     

{-|
  Retrieves the list of axioms from the current working theory.  The list
  contains pairs of string names and the axioms.  This names exists such that
  compile time operations have a tag with which they can use to extract axioms 
  from saved theories.  See 'extractAxiom' for more details.
-}
axioms :: HOL cls thry (Map Text HOLThm)
axioms =        
    do acid <- openLocalStateHOL (TheAxioms mapEmpty)
       m <- queryHOL acid GetAxioms
       closeAcidStateHOL acid
       return m

{-| 
  Constructs a new axiom of a given name and conclusion term.  Also adds this
  new axiom to the current working theory.  Throws a 'HOLException' in the 
  following cases:

  * The provided term is not a proposition.

  * An axiom with the provided name has already been declared.
-}
newAxiom :: HOLTermRep tm Theory thry => (Text, tm) -> HOL Theory thry HOLThm
newAxiom (name, ptm) =
    do acid <- openLocalStateHOL (TheAxioms mapEmpty)
       qth <- queryHOL acid (GetAxiom' name)
       closeAcidStateHOL acid
       case qth of
         Just th -> 
             return th
         Nothing ->
             do tm <- toHTm ptm
                ty <- typeOf tm
                failWhen (return $! ty /= tyBool) "newAxiom: Not a proposition."
                let th = axiomThm tm
                acid' <- openLocalStateHOL (TheAxioms mapEmpty)
                updateHOL acid' (InsertAxiom name th)
                closeAcidStateHOL acid'
                return th
                   
-- | Retrieves an axiom by label from the theory context.
getAxiom :: Text -> HOL cls thry HOLThm
getAxiom lbl =
    do acid <- openLocalStateHOL (TheAxioms mapEmpty)
       qth <- queryHOL acid (GetAxiom' lbl)
       closeAcidStateHOL acid
       case qth of
         Just res -> return res
         _ -> fail $ "getAxiom: axiom " ++ show lbl ++ " not found."

-- State for Definitions
{-|
  Retrieves the list of definitions from the current working theory.  See
  'newBasicDefinition' for more details.
-}
definitions :: HOL cls thry [HOLThm]
definitions =
    do acid <- openLocalStateHOL (TheCoreDefinitions mapEmpty)
       m <- queryHOL acid GetCoreDefinitions
       closeAcidStateHOL acid
       return m

{-|
  Introduces a definition of the form @c = t@ into the current working theory.
  Throws a 'HOLException' when the definitional term is ill-formed.  See
  'newDefinedConst' for more details.
-}
newBasicDefinition :: HOLTermRep tm Theory thry 
                   => (Text, tm) -> HOL Theory thry HOLThm
newBasicDefinition (lbl, ptm) =
    getBasicDefinition lbl
    <|> do tm <- toHTm ptm
           pat <- destEq tm
           case pat of
             (Const _ _, _) -> 
                 fail "newBasicDefinition: constant already defined."
             (Var name _, _)
                 | name /= lbl ->
                     fail $ "newBasicDefinition: provided label does not " ++
                            "match provided term."
                 | otherwise ->
                     do (c@(Const x _), dth) <- newDefinedConst tm
                        newConstant' x c
                        acid <- openLocalStateHOL (TheCoreDefinitions mapEmpty)
                        updateHOL acid (InsertCoreDefinition lbl dth)
                        closeAcidStateHOL acid
                        return dth
             _ -> fail "newBasicDefinition: provided term not an equation."
                    
-- | Retrieves a basic term definition by label from the theory context.
getBasicDefinition :: Text -> HOL cls thry HOLThm
getBasicDefinition lbl =
    do acid <- openLocalStateHOL (TheCoreDefinitions mapEmpty)
       qth <- queryHOL acid (GetCoreDefinition lbl)
       closeAcidStateHOL acid
       case qth of
         Just res -> return res
         _ -> fail $ "getBasicDefinition: definition for " ++ show lbl ++
                     " not found."

{-|
  Introduces a new type constant, and two associated term constants, into the 
  current working theory that is defined as an inhabited subset of an existing 
  type constant.  Takes the following arguments:
  
  *  The name of the new type constant.

  *  The name of the new term constant that will be used to construct the type.

  *  The name of the new term constant that will be used to desctruct the type.

  *  A theorem that proves that the defining predicate has at least one
     satisfying value.

  Throws a 'HOLException' in the following cases:

  *  A term constant of either of the provided names has already been defined.

  *  A type constant of the provided name has already been defined.

  See 'newDefinedTypeOp' for more details.
-}
newBasicTypeDefinition :: HOLThmRep thm Theory thry => Text -> Text -> Text 
                       -> thm -> HOL Theory thry (HOLThm, HOLThm)
newBasicTypeDefinition tyname absname repname pth =
  do failWhen (return or <*> mapM (can getConstType) [absname, repname]) $
       "newBasicTypeDefinition: Constant(s) " ++ show absname ++ ", " ++ 
       show repname ++ " already in use."
     dth <- toHThm pth
     (atyop, a, r, dth1, dth2) <- newDefinedTypeOp tyname absname repname dth
     failWhen (canNot (newType' tyname) atyop) $
       "newBasicTypeDefinition: Type " ++ show tyname ++ " already defined."
     newConstant' absname a
     newConstant' repname r
     acid <- openLocalStateHOL (TypeDefinitions mapEmpty)
     updateHOL acid (InsertTypeDefinition tyname (dth1, dth2))
     closeAcidStateHOL acid
     return (dth1, dth2)

-- | Retrieves a basic type definition by label from the theory context.
getBasicTypeDefinition :: Text -> HOL cls thry (HOLThm, HOLThm)
getBasicTypeDefinition lbl =
    do acid <- openLocalStateHOL (TypeDefinitions mapEmpty)
       qth <- queryHOL acid (GetTypeDefinition lbl)
       closeAcidStateHOL acid
       case qth of
         Just res -> return res
         _ -> fail $ "getBasicTypeDefinition: definition for " ++ 
                     show lbl ++ " not found."

-- Primitive Debugging Functions
{-| 
  Prints the provided string, with a new line, when the given boolean value is
  true.
-}
warn :: Bool -> String -> HOL cls thry ()
warn flag str = when flag $ putStrLnHOL str

{-|
  Prints the provided string, with a new line, when debugging is turned on, then
  returns the given 'HOL' computation.  A version of 'trace' for the 'HOL' monad
  that is referentially transparent.
-}
printDebugLn :: String -> HOL cls thry a -> HOL cls thry a
printDebugLn = printDebugBase putStrLnHOL

-- | A version of printDebug that does not print a new line.
printDebug :: String -> HOL cls thry a -> HOL cls thry a
printDebug = printDebugBase putStrHOL

-- Abstracted out for future flexibility.  Not exported.
printDebugBase :: (String -> HOL cls thry ()) -> String -> HOL cls thry a -> 
                  HOL cls thry a
printDebugBase fn str x =
    do debug <- getBenignFlag FlagDebug
       if debug
          then fn str >> x
          else x

-- Overloadings
mkMConst :: HOLTypeRep ty cls thry => Text -> ty -> HOL cls thry HOLTerm
mkMConst op = let ?constsFun = constants in overload1 (O.mkMConst op)

listMkIComb :: HOLTermRep tm cls thry => Text -> [tm] -> HOL cls thry HOLTerm
listMkIComb op = let ?constsFun = constants in overload1 (O.listMkIComb op)

mkBinary :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
         => Text -> tm1 -> tm2 -> HOL cls thry HOLTerm
mkBinary op = let ?constsFun = constants in overload2 (O.mkBinary op)

mkBinder :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
         => Text -> tm1 -> tm2 -> HOL cls thry HOLTerm
mkBinder op = let ?constsFun = constants in overload2 (O.mkBinder op)

mkTyBinder :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
           => Text -> ty -> tm -> HOL cls thry HOLTerm
mkTyBinder op = let ?constsFun = constants in overload2 (O.mkTyBinder op)

mkIff :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
      => tm1 -> tm2 -> HOL cls thry HOLTerm
mkIff = let ?constsFun = constants in overload2 O.mkIff

mkConj :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
       => tm1 -> tm2 -> HOL cls thry HOLTerm
mkConj = let ?constsFun = constants in overload2 O.mkConj

mkImp :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
      => tm1 -> tm2 -> HOL cls thry HOLTerm
mkImp = let ?constsFun = constants in overload2 O.mkImp

mkForall :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
         => tm1 -> tm2 -> HOL cls thry HOLTerm
mkForall = let ?constsFun = constants in overload2 O.mkForall

mkExists :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
         => tm1 -> tm2 -> HOL cls thry HOLTerm
mkExists = let ?constsFun = constants in overload2 O.mkExists

mkDisj :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
       => tm1 -> tm2 -> HOL cls thry HOLTerm
mkDisj = let ?constsFun = constants in overload2 O.mkDisj

mkNeg :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
mkNeg = let ?constsFun = constants in overload1 O.mkNeg

mkUExists :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
          => tm1 -> tm2 -> HOL cls thry HOLTerm
mkUExists = let ?constsFun = constants in overload2 O.mkUExists

mkTyAll :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
        => ty -> tm -> HOL cls thry HOLTerm
mkTyAll = let ?constsFun = constants in overload2 O.mkTyAll

mkTyEx :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
       => ty -> tm -> HOL cls thry HOLTerm
mkTyEx = let ?constsFun = constants in overload2 O.mkTyEx

listMkConj :: HOLTermRep tm cls thry => [tm] -> HOL cls thry HOLTerm
listMkConj = let ?constsFun = constants in overload1 O.listMkConj

listMkDisj :: HOLTermRep tm cls thry => [tm] -> HOL cls thry HOLTerm
listMkDisj = let ?constsFun = constants in overload1 O.listMkDisj

listMkForall :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
          => [tm1] -> tm2 -> HOL cls thry HOLTerm
listMkForall = let ?constsFun = constants in overload2 O.listMkForall

listMkExists :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
          => [tm1] -> tm2 -> HOL cls thry HOLTerm
listMkExists = let ?constsFun = constants in overload2 O.listMkExists

mkGAbs :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
       => tm1 -> tm2 -> HOL cls thry HOLTerm
mkGAbs = let ?typesFun = types
             ?constsFun = constants in overload2 O.mkGAbs

listMkGAbs :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
           => [tm1] -> tm2 -> HOL cls thry HOLTerm
listMkGAbs = let ?typesFun = types
                 ?constsFun = constants in overload2 O.listMkGAbs

mkLet :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry,
          HOLTermRep tm3 cls thry)
      => [(tm1, tm2)] -> tm3 -> HOL cls thry HOLTerm
mkLet = let ?typesFun = types
            ?constsFun = constants in overload2 O.mkLet
