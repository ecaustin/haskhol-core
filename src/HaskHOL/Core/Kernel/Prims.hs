{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GADTs, PatternSynonyms, 
             TemplateHaskell #-}

{-|
  Module:    HaskHOL.Core.Kernel.Prims
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines the primitive data types for HaskHOL: 
  'HOLType', 'HOLTerm', and 'HOLThm'.

   Note:  This module is intended to be hidden by cabal to prevent manual, and 
   possibly unsound, construction of the primitive data types.  

   To include the contents of this module with the appropriate restrictions in 
   place, along with the entirey of the core system, import the "HaskHOL.Core"
   module.  Alternatively, the following modules also export individual
   primitive types with their associated restrictions:
   * "HaskHOL.Core.Types"  - Exports types
   * "HaskHOL.Core.Terms"  - Exports terms
   * "HaskHOL.Core.Kernel" - Exports theorems
-}

module HaskHOL.Core.Kernel.Prims
    ( -- * HOL types
      HOLType(..)
    , pattern TyVar
    , pattern TyApp
    , pattern UType
    , TypeOp(..)
    , pattern TyOpVar
    , pattern TyPrimitive
    , pattern TyDefined
    , HOLTypeEnv
    , SubstTrip
      -- * HOL terms
    , HOLTerm(..)
    , pattern Var
    , pattern Const
    , pattern Comb
    , pattern Abs
    , pattern TyComb
    , pattern TyAbs
    , ConstTag(..)
    , pattern Primitive
    , pattern Defined
    , pattern MkAbstract
    , pattern DestAbstract
    , HOLTermEnv
      -- * HOL theorems
    , HOLThm(..)
    , pattern Thm
      -- Primitive Error Types
    , HOLPrimError(..)
    ) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad.Catch (Exception)

import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)
import Data.Hashable
import Data.Text.Lazy (Text)

import GHC.Generics

import Language.Haskell.TH.Lift
import Instances.TH.Lift()

{-
  A quick note on how the primitive data types of HaskHOL are implemented -- 
  unidirectional pattern synonyms are used to simulate private data types for 
  HOL types, terms, and theorems.
-}

{-
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
{-|
  The 'HOLType' data type defines the internal constructors for HOL types in
  HaskHOL.
-}
data HOLType 
    = TyVarIn !Bool    !Text
    | TyAppIn !TypeOp  ![HOLType]
    | UTypeIn !HOLType !HOLType
    deriving (Eq, Ord, Typeable, Generic)

instance Hashable HOLType

-- | A type variable consisting of a constraint flag and name.
pattern TyVar b s <- TyVarIn b s
    
{-| 
  A type application consisting of a type operator and a list of type
  arguments.  See 'TypeOp' for more details.
-}
pattern TyApp op tys <- TyAppIn op tys
{-| 
  A universal type consisting of a bound type and a body type.  Note that 
  the bound type must be a small, type variable.
-}
pattern UType tv bod <- UTypeIn tv bod

{-|
  The data type for type operators, 'TypeOp', is a mashing together of the
  representation of type operators from both both HOL2P and Stateless HOL.
  For more information regarding construction of the different operators, see
  the documentation of the following functions: 'mkTypeOpVar', 
  'newPrimitiveTypeOp', and 'newDefinedTypeOp'
-}
data TypeOp 
    = TyOpVarIn     !Text
    | TyPrimitiveIn !Text !Int
    | TyDefinedIn   !Text !Int !Int -- Hash of concl of thm
    deriving (Eq, Ord, Typeable, Generic)

instance Hashable TypeOp

-- | A type operator variable consisting of a name.
pattern TyOpVar s <- TyOpVarIn s

-- | A type operator primitive consisting of a name and arity.
pattern TyPrimitive s n <- TyPrimitiveIn s n

-- | A defined type operator consisting of a name and arity.
pattern TyDefined s n <- TyDefinedIn s n _

{-
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

  * @TyVarIn True \"x\"@ represents the small type variable @\'x@
 
  * @TyVarIn False \"x\"@ represents the unconstrainted type variable @x@
 
  * @TyOpVar "x"@ represents the type operator variable @_x@

  Note that external to HaskHOL, during I/O of terms, both the parser and
  pretty-printer still rely on the syntactic distinctions introduced by
  Voelker.
-} 

-- | Type synonym for the commonly used, list-based, type environment.
type HOLTypeEnv = [(HOLType, HOLType)]

{-| 
  Type synonym for the commonly used triplet of substitution environments.
  See 'TypeSubst' for more information.
-}
type SubstTrip = (HOLTypeEnv, [(TypeOp, HOLType)], [(TypeOp, TypeOp)])

instance Show TypeOp where
    show (TyOpVarIn s) = '_' : show s
    show (TyPrimitiveIn s _) = show s
    show (TyDefinedIn s _ _) = show s

{-
  The following data types combined provide the definition of HOL terms in 
  HaskHOL.

  Corresponding with the 'HOLType' data type, 'HOLTerm' follows closely from
  the definition of terms in HOL Light.  Again, the appropriate modifications
  have been made to facilitate a stateless and polymorphic term language.

  Most notably this includes:
  (1) The introduction of tags for constants to carry information formerly
      contained in the state.

  2.  Additional constructors have been added to 'HOLTerm' to facilitate
      term-level, type abstractions and applications.
-}

{-|
  The 'HOLTerm' data type defines the internal constructors for HOL terms in
  HaskHOL.  For more details, see the documentation for its view pattern data
  type, 'HOLTermView'.
-}
data HOLTerm
    = VarIn    !Text     !HOLType
    | ConstIn  !Text     !HOLType !ConstTag 
    | CombIn   !HOLTerm  !HOLTerm
    | AbsIn    !HOLTerm  !HOLTerm
    | TyCombIn !HOLTerm  !HOLType
    | TyAbsIn  !HOLType  !HOLTerm
    deriving (Eq, Ord, Typeable, Generic)

instance Hashable HOLTerm

-- | A term variable consisting of a name and type.
pattern Var s ty <- VarIn s ty 

{-| 
  A term constant consisting of a name, type, and tag.  See 'ConstTag' for 
  more information.
-}
pattern Const s ty <- ConstIn s ty _

-- | A term application consisting of a function term and argument term.
pattern Comb l r <- CombIn l r
    
{-| 
  A term abstraction consisting of a bound term and a body term.  Note that
  the bound term must be a type variable.
-}
pattern Abs bv bod <- AbsIn bv bod

{-| 
  A term-level, type application consisting of a body term and an argument 
  type. Note that the body term must have a universal type.
-}
pattern TyComb tm ty <- TyCombIn tm ty
    
{-| 
  A term-level, type abstraction consisting of a bound type and a body term.
  Note that the bound type must be a small, type variable.
-}
pattern TyAbs ty tm <- TyAbsIn ty tm
   
{-| 
  The data type for constant tags, 'ConstTag', follows identically from the
  implementation in Stateless HOL.  For more information regarding construction
  of the different tags, see the documentation of the following functions:
  'newPrimitiveConst', 'newDefinedConst', and 'newDefinedTypeOp'.
-}
data ConstTag
    = PrimitiveIn
    | DefinedIn      !Int            -- hash
    | MkAbstractIn   !Text !Int !Int -- name, arity, hash
    | DestAbstractIn !Text !Int !Int -- name, arity, hash
    deriving (Eq, Ord, Typeable, Generic)

instance Hashable ConstTag

-- | A primitive constant tag.
pattern Primitive <- PrimitiveIn

-- | A defined constant tag.
pattern Defined <- DefinedIn _

{-| A defined constant tag for type construction consisting of a name and 
    arity.
-}
pattern MkAbstract s n <- MkAbstractIn s n _

{-| A defined constant tag for type destruction consisting of a name and 
    arity.
-}
pattern DestAbstract s n <- DestAbstractIn s n _

instance Show ConstTag where
    show PrimitiveIn = "Prim"
    show (DefinedIn _) = "Defined"
    show (MkAbstractIn s _ _) = "Mk__" ++ show s
    show (DestAbstractIn s _ _) = "Dest__" ++ show s

-- | Type synonym for the commonly used, list-based, term environment.
type HOLTermEnv = [(HOLTerm, HOLTerm)]

{-| 
  The 'HOLThm' data type defines HOL Theorems in HaskHOL.  A theorem is defined
  simply as a list of assumption terms and a conclusion term.

  Note that this representation, in combination with a stateless 
  approach, means that the introduction of axioms is not tracked in the kernel.
  Axioms can be tracked once the stateful layer of the prover is introduced,
  though.  For more details see the documentation for `newAxiom`.
-}
data HOLThm = ThmIn ![HOLTerm] !HOLTerm deriving (Eq, Ord, Typeable, Generic)
        
instance Hashable HOLThm

-- | The pattern synonym for HOL theorems.
pattern Thm as c <- ThmIn as c

-- Error types
data HOLPrimError where
    HOLTypeOpError :: TypeOp -> String -> HOLPrimError
    HOLTypeError   :: HOLType -> String -> HOLPrimError
    HOLTermError   :: HOLTerm -> String -> HOLPrimError 
    HOLThmError    :: HOLThm -> String -> HOLPrimError
    HOLMiscError   :: Show a => a -> String -> HOLPrimError
    HOLErrorMsg    :: String -> HOLPrimError
    HOLExhaustiveWarning :: String -> HOLPrimError
    HOLMZero       :: HOLPrimError

instance Show HOLPrimError where
    show (HOLTypeOpError _ str) = str
    show (HOLTypeError _ str) = str
    show (HOLTermError _ str) = str
    show (HOLThmError  _ str) = str
    show (HOLMiscError x str) = str ++ "<* witness - " ++ show x ++ " *>"
    show (HOLErrorMsg  str) = str
    show (HOLExhaustiveWarning str) = 
        "<* exhaustive case warning - " ++ str ++ " *>"
    show HOLMZero = "<* mzero *>"

instance Exception HOLPrimError

{- 
  Deepseq instances for the primitive data types.  These are included as they 
  are commonly used by a number of benchmarking libraries.
-}
instance NFData HOLType where
    rnf (TyVarIn b s) = rnf b `seq` rnf s
    rnf (TyAppIn s tys) = rnf s `seq` rnf tys
    rnf (UTypeIn tv tb) = rnf tv `seq` rnf tb

instance NFData TypeOp where
    rnf (TyOpVarIn s) = rnf s
    rnf (TyPrimitiveIn s n) = rnf s `seq` rnf n
    rnf (TyDefinedIn s n h) = rnf s `seq` rnf n `seq` rnf h

instance NFData HOLTerm where
    rnf (VarIn s ty) = rnf s `seq` rnf ty
    rnf (ConstIn s ty tag) = rnf s `seq` rnf ty `seq` rnf tag
    rnf (CombIn l r) = rnf l `seq` rnf r
    rnf (AbsIn bv bod) = rnf bv `seq` rnf bod
    rnf (TyAbsIn bty bod) = rnf bty `seq` rnf bod
    rnf (TyCombIn tm ty) = rnf tm `seq` rnf ty

instance NFData ConstTag where
    rnf PrimitiveIn = ()
    rnf (DefinedIn h) = rnf h
    rnf (MkAbstractIn s i h) = rnf s `seq` rnf i `seq` rnf h
    rnf (DestAbstractIn s i h) = rnf s `seq` rnf i `seq` rnf h

instance NFData HOLThm where
    rnf (ThmIn asl c) = rnf asl `seq` rnf c

deriveSafeCopy 0 'base ''TypeOp
deriveSafeCopy 0 'base ''HOLType
deriveSafeCopy 0 'base ''ConstTag
deriveSafeCopy 0 'base ''HOLTerm
deriveSafeCopy 0 'base ''HOLThm

deriveLiftMany [''TypeOp, ''HOLType, ''ConstTag, ''HOLTerm, ''HOLThm]
