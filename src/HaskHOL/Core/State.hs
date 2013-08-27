{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, ViewPatterns #-}

{-|
  Module:    HaskHOL.Core.State
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
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
      types            -- :: HOL cls thry [(String, TypeOp)]
    , getTypeArityCtxt -- :: HOLContext thry -> String -> Maybe Int
    , getTypeArity     -- :: String -> HOL cls thry Int
    , newType          -- :: String -> Int -> HOL Theory thry ()
    , mkType           -- :: String -> [HOLType] -> HOL cls thry HOLType
    , mkFunTy          -- :: HOLType -> HOLType -> HOL cls thry HOLType
    -- * Stateful Term Primitives
    , constants    -- :: HOL cls thry [(String, HOLTerm)]
    , getConstType -- :: String -> HOL cls thry HOLType
    , newConstant  -- :: String -> HOLType -> HOL Theory thry ()
    , mkConst      -- :: TypeSubst l r => 
                   --    String -> [(l, r)] -> HOL cls thry HOLTerm
    , mkConstFull  -- :: String -> SubstTrip -> HOL cls thry HOLTerm
    , mkEq         -- :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
    -- * Stateful Theory Extension Primitives
    , axioms                 -- :: HOL cls thry [(String, HOLThm)]
    , getAxiom               -- :: String -> HOL cls thry HOLThm
    , newAxiom               -- :: String -> HOLTerm -> HOL Theory thry HOLThm
    , definitions            -- :: HOL cls thry [HOLThm]
    , newBasicDefinition     -- :: HOLTerm -> HOL Theory thry HOLThm
    , newBasicTypeDefinition -- :: String -> String -> String -> HOLThm -> 
                             --    HOL Theory thry (HOLThm, HOLThm)
    -- * Primitive Debugging System
    , FlagDebug(..)
    , warn         -- :: Bool -> String -> HOL cls thry ()
    , printDebugLn -- :: String -> HOL cls thry a -> HOL cls thry a
    , printDebug   -- :: String -> HOL cls thry a -> HOL cls thry a
      -- * Monad Re-Export
    , module HaskHOL.Core.State.Monad
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

-- New flags and extensions
-- | Flag states whether or not to print debug statements.
newFlag "FlagDebug" True

newExtension "TypeConstants" 
  [| [("bool", tyOpBool), ("fun", tyOpFun)] :: [(String, TypeOp)] |]

newExtension "TermConstants" [| [("=", tmEq tyA)] :: [(String, HOLTerm)] |]

newExtension "TheAxioms" [| [] :: [(String, HOLThm)] |]

{- 
  Extensible state type for term definitions introduced via 
  newBasicDefinition.
-}
newExtension "TheCoreDefinitions" [| [] :: [HOLThm] |]

-- Stateful HOL Light Type Primitives
{-|
  Retrieves the list of type constants from the current working theory.  The
  list contains pairs of strings recognized by the parser and the associated
  type operator value, i.e. 

  > ("bool", tyOpBool)
-}
types :: HOL cls thry [(String, TypeOp)]
types =
    do (TypeConstants tys) <- getExt
       return tys

-- needed for parser
{-| 
  Retrieves the arity of a given type constant.  Fails with 'Nothing' if the
  provided type constant name is not defined in the provided context.

  Note that this function takes a 'HOLContext' argument such that it can be
  used outside of 'HOL' computations; for example, in the parser.
-}
getTypeArityCtxt :: HOLContext thry -> String -> Maybe Int
getTypeArityCtxt ctx name =
    let (TypeConstants tys) = getExtCtxt ctx in
      do tyOp <- lookup name tys
         return . snd $ destTypeOp tyOp

{-|
  A version of 'getTypeArityCtxt' that operates over the current working theory
  of a 'HOL' computation.  Throws a 'HOLException' if the provided type constant
  name is not defined.
-}
getTypeArity :: String -> HOL cls thry Int
getTypeArity name =
    do ctxt <- get
       liftMaybe ("getTypeArity: type " ++ name ++ " has not been defined.") $
         getTypeArityCtxt ctxt name

{- 
  Primitive type constant construction function.  Used by newType and 
  newBasicTypeDefinition.  Not exposed to the user.
-}
newType' :: String -> TypeOp -> HOL Theory thry ()
newType' name tyop =
    do failWhen (can getTypeArity name) $
         "newType: type " ++ name ++ " has already been declared."
       modifyExt $ \ (TypeConstants consts) -> 
                       TypeConstants $ (name, tyop) : consts

{-| 
  Constructs a new primitve type constant of a given name and arity.  Also adds
  this new type to the current working theory.  Throws a 'HOLException' when a 
  type of the same name has already been declared.
-}
newType :: String -> Int -> HOL Theory thry ()
newType name arity = 
    newType' name $ newPrimTypeOp name arity

{-|
  Constructs a type application given an operator name and a list of argument
  types.  If the provided name is not a currently defined type constant then
  this function defaults it to a type operator variable.  Throws a 
  'HOLException' in the following cases:

  * A type operator's arity disagrees with the length of the argument list.

  * A type operator is applied to zero arguments.
-}
mkType :: String -> [HOLType] -> HOL cls thry HOLType
mkType name args =
    do (TypeConstants consts) <- getExt
       case lookup name consts of
         Just tyOp -> liftEither "mkType: type constructor application failed" $
                        tyApp tyOp args
         Nothing -> 
           {- This seemed to be the easiest way to supress superfluous warnings
              when parsing type operators. -}
           do name' <- case name of
                         '_':x -> return x
                         _ -> printDebugLn 
                                ("warning - mkType: type " ++ name ++ " has " ++
                                 "not been defined.  Defaulting to type " ++ 
                                 "operator variable.") $ 
                                return name
              failWhen (return $ null args)
                "mkType: type operator applied to zero args."
              liftEither "mkType: type operator variable application failed" $ 
                tyApp (mkTypeOpVar name') args

{-|
  Constructs a function type safely using 'mkType'.  Should never fail provided
  that the initial value for type constants has not been modified.
-}
mkFunTy :: HOLType -> HOLType -> HOL cls thry HOLType
mkFunTy ty1 ty2 = mkType "fun" [ty1, ty2]

-- State for Constants
{-|
  Retrieves the list of term constants from the current working theory.  The
  list contains pairs of strings recognized by the parser and the associated
  term constant value, i.e. 

  > ("=", tmEq tyA)
-}
constants :: HOL cls thry [(String, HOLTerm)]
constants =
    do (TermConstants consts) <- getExt
       return consts

{-|
  Retrieves the type of a given term constant.  Throws a 'HOLException' if the
  provided term constant name is not defined.
-}
getConstType :: String -> HOL cls thry HOLType
getConstType name =
    do (TermConstants consts) <- getExt
       tm <- liftMaybe "getConstType: not a constant name" $
               lookup name consts
       return $! typeOf tm

{-
  Primitive term constant construction function.  Used by newConstant,
  newBasicDefinition, and newBasicTypeDefinition.
-}
newConstant' :: String -> HOLTerm -> HOL Theory thry ()
newConstant' name c =
    do failWhen (can getConstType name) $
         "newConstant: constant " ++ name ++ " has already been declared."
       modifyExt $ \ (TermConstants consts) -> 
                       TermConstants $ (name, c) : consts

{-|
  Constructs a new primitive term constant of a given name and type.  Also adds
  this new term to the current working theory.  Throws a 'HOLException' when a
  term of the same name has already been declared.
-}
newConstant :: String -> HOLType -> HOL Theory thry ()
newConstant name ty =
    newConstant' name $ newPrimConst name ty

{-|
  Constructs a specific instance of a term constant when provided with its name
  and a type substition environment.  Throws a 'HOLException' in the 
  following cases:

  * The instantiation as performed by 'instConst' fails.

  * The provided name is not a currently defined constant.
-}
mkConst :: TypeSubst l r => String -> [(l, r)] -> HOL cls thry HOLTerm
mkConst name tyenv =
    do (TermConstants consts) <- getExt
       tm <- liftMaybe "mkConst: not a constant name" $ 
               lookup name consts
       liftMaybe "mkConst: instantiation failed" $ 
         instConst tm tyenv

{-| 
  A version of 'mkConst' that accepts a triplet of type substitition 
  environments.  Frequently used with the 'typeMatch' function.
-}
mkConstFull :: String -> SubstTrip -> HOL cls thry HOLTerm
mkConstFull name pat =
    do (TermConstants consts) <- getExt
       tm <- liftMaybe "mkConstFull: not a constant name" $
               lookup name consts
       liftMaybe "mkConstFull: instantiation failed" $ 
         instConstFull tm pat
                                    
{-| 
  Safely creates an equality between two terms using 'mkConst' using the type of
  the left hand side argument to perform the required instantiation.  Throws a
  'HOLException' in the case when the types of the two terms do not agree.
-}
mkEq :: HOLTerm -> HOLTerm -> HOL cls thry HOLTerm
mkEq l r =
    let ty = typeOf l in
      do eq <- mkConst "=" [(tyA, ty)]
         liftEither "mkEq" $
           liftM1 mkComb (mkComb eq l) r

-- State for Axioms	

{-|
  Retrieves the list of axioms from the current working theory.  The list
  contains pairs of string names and the axioms.  This names exists such that
  compile time operations have a tag with which they can use to extract axioms 
  from saved theories.  See 'extractAxiom' for more details.
-}
axioms :: HOL cls thry [(String, HOLThm)]
axioms =	
    do (TheAxioms thms) <- getExt
       return thms

{-| 
  Retrieves a specific axiom by name.  Throws a 'HOLException' if there is no
  axiom with the provided name in the current working theory.
-}
getAxiom :: String -> HOL cls thry HOLThm
getAxiom lbl =
    do (TheAxioms thms) <- getExt
       liftMaybe "getAxiom: axiom name not found" $
         lookup lbl thms

{-| 
  Constructs a new axiom of a given name and conclusion term.  Also adds this
  new axiom to the current working theory.  Throws a 'HOLException' in the 
  following cases:

  * The provided term is not a proposition.

  * An axiom with the provided name has already been declared.
-}
newAxiom :: String -> HOLTerm -> HOL Theory thry HOLThm
newAxiom name tm
    | typeOf tm /= tyBool = fail "newAxiom: Not a proposition."
    | otherwise =
        do failWhen (can getAxiom name) $ "newAxiom: axiom with name " ++ 
             name ++ " has already been declared."
           let th = axiomThm tm 
           modifyExt $ \ (TheAxioms axs) -> TheAxioms $ (name, th) : axs
           return th

-- State for Definitions
{-|
  Retrieves the list of definitions from the current working theory.  See
  'newBasicDefinition' for more details.
-}
definitions :: HOL cls thry [HOLThm]
definitions =
    do (TheCoreDefinitions defs) <- getExt
       return defs

{-|
  Introduces a definition of the form @c = t@ into the current working theory.
  Throws a 'HOLException' when the definitional term is ill-formed.  See
  'newDefinedConst' for more details.
-}
newBasicDefinition :: HOLTerm -> HOL Theory thry HOLThm
newBasicDefinition tm =
    do (c@(view -> Const name _ _), dth) <- liftEither "newBasicDefinition" $
                                              newDefinedConst tm
       newConstant' name c
       modifyExt $ \ (TheCoreDefinitions defs) -> 
                       TheCoreDefinitions $ dth : defs
       return dth

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
newBasicTypeDefinition :: String -> String -> String -> HOLThm -> 
                          HOL Theory thry (HOLThm, HOLThm)
newBasicTypeDefinition tyname absname repname dth =
  do failWhen (return or <*> mapM (can getConstType) [absname, repname]) $
       "newBasicTypeDefinition: Constant(s) " ++ absname ++ ", " ++ repname ++
         " already in use."
     (atyop, a, r, dth1, dth2) <- liftEither "newBasicTypeDefinition" $
                                    newDefinedTypeOp tyname absname repname dth
     failWhen (canNot (newType' tyname) atyop) $
       "newBasicTypeDefinition: Type " ++ tyname ++ " already defined."
     newConstant' absname a
     newConstant' repname r
     return (dth1, dth2)


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

deriveLiftMany [ ''TypeConstants, ''TermConstants
               , ''TheAxioms, ''TheCoreDefinitions ]
