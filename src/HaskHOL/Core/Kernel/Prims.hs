{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, 
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
    , HOLTypeView(..)
    , TypeOp(..)
    , HOLTypeEnv
    , SubstTrip
      -- * HOL terms
    , HOLTerm(..)
    , HOLTermView(..)
    , ConstTag(..)
    , HOLTermEnv
      -- * HOL theorems
    , HOLThm(..)
    , HOLThmView(..)
      -- * The View pattern class
    , Viewable(..)
    ) where

import HaskHOL.Core.Lib

{-
  A quick note on how the primitive data types of HaskHOL are implemented -- 
  view patterns are used to simulate private data types for HOL types and 
  terms:
  * Internal constructors are hidden to prevent manual construction of terms.
 
  * View constructors (those of 'HOLTypeView', 'HOLTermView', and 'HOLThmView')
    are exposed to enable pattern matching. 
 
  * View patterns, as defined by instances of the 'view' function from the 
    @Viewable@ class, provide a conversion between the two sets of constructors.
-}

{-
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

{-|
  The 'HOLType' data type defines the internal constructors for HOL types in
  HaskHOL.  For more details, see the documentation for its view pattern data
  type, 'HOLTypeView'.
-}
data HOLType
    = TyVarIn !Bool !String
    | TyAppIn !TypeOp ![HOLType]
    | UTypeIn !HOLType !HOLType
    deriving (Eq, Ord, Typeable) 

-- | The view pattern data type for HOL types.
data HOLTypeView
    -- | A type variable consisting of a constraint flag and name.
    = TyVar Bool String
    {-| 
      A type application consisting of a type operator and a list of type
      arguments.  See 'TypeOp' for more details.
    -}
    | TyApp TypeOp [HOLType]
    {-| 
      A universal type consisting of a bound type and a body type.  Note that 
      the bound type must be a small, type variable.
    -}
    | UType HOLType HOLType

{-|
  The data type for type operators, 'TypeOp', is a mashing together of the
  representation of type operators from both both HOL2P and Stateless HOL.
  For more information regarding construction of the different operators, see
  the documentation of the following functions: 'mkTypeOpVar', 'newPrimTypeOp',
  'newDefinedTypeOp'
-}
data TypeOp 
    = TyOpVar !String
    | TyPrim !String !Int
    | TyDefined !String !Int !HOLThm
    deriving (Eq, Ord, Typeable)

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

-- Viewable and Show instances for HOLType
instance Viewable HOLType HOLTypeView where
    view (TyVarIn b s) = TyVar b s
    view (TyAppIn tyop tys) = TyApp tyop tys
    view (UTypeIn v b) = UType v b

instance Show TypeOp where
    show (TyOpVar s) = '_' : s
    show (TyPrim s _) = s
    show (TyDefined s _ _) = s

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
    = VarIn !String !HOLType
    | ConstIn !String !HOLType !ConstTag
    | CombIn !HOLTerm !HOLTerm
    | AbsIn !HOLTerm !HOLTerm
    | TyCombIn !HOLTerm !HOLType
    | TyAbsIn !HOLType !HOLTerm
    deriving (Eq, Ord, Typeable)

-- | The view pattern data type for HOL terms.
data HOLTermView
    -- | A term variable consisting of a name and type.
    = Var String HOLType
    {-| 
      A term constant consisting of a name, type, and tag.  See 'ConstTag' for 
      more information.
    -}
    | Const String HOLType ConstTag
    -- | A term application consisting of a function term and argument term.
    | Comb HOLTerm HOLTerm
    {-| 
      A term abstraction consisting of a bound term and a body term.  Note that
      the bound term must be a type variable.
    -}
    | Abs HOLTerm HOLTerm
    {-| 
      A term-level, type application consisting of a body term and an argument 
      type. Note that the body term must have a universal type.
    -}
    | TyComb HOLTerm HOLType
    {-| 
      A term-level, type abstraction consisting of a bound type and a body term.
      Note that the bound type must be a small, type variable.
    -}
    | TyAbs HOLType HOLTerm
   
{-| 
  The data type for constant tags, 'ConstTag', follows identically from the
  implementation in Stateless HOL.  For more information regarding construction
  of the different tags, see the documentation of the following functions:
  'newPrimConst', 'newDefinedConst', and 'newDefinedTypeOp'.
-}
data ConstTag
    = Prim
    | Defined !HOLTerm
    | MkAbstract !String !Int !HOLThm
    | DestAbstract !String !Int !HOLThm
    deriving (Eq, Ord, Typeable)

-- | Type synonym for the commonly used, list-based, term environment.
type HOLTermEnv = [(HOLTerm, HOLTerm)]

{- 
  The Viewable instance for terms.  
  Note that the Show instance for terms is more complicated than for types and, 
  as such, is included separately in the HaskHOL.Core.Printer module.
-}
instance Viewable HOLTerm HOLTermView where
    view (VarIn s ty) = Var s ty
    view (ConstIn s ty tag) = Const s ty tag
    view (CombIn l r) = Comb l r
    view (AbsIn v bod) = Abs v bod
    view (TyAbsIn tv tb) = TyAbs tv tb
    view (TyCombIn tm ty) = TyComb tm ty

{-| 
  The 'HOLThm' data type defines HOL Theorems in HaskHOL.  A theorem is defined
  simply as a list of assumption terms and a conclusion term.

  Note that this representation, in combination with a stateless 
  approach, means that the introduction of axioms is not tracked in the kernel.
  Axioms can be tracked once the stateful layer of the prover is introduced,
  though.  For more details see the documentation for `newAxiom`.
-}
data HOLThm = ThmIn ![HOLTerm] !HOLTerm
  deriving (Eq, Ord, Typeable)

-- | The view pattern data type for HOL theorems.
data HOLThmView = Thm [HOLTerm] HOLTerm

instance Viewable HOLThm HOLThmView where
  view (ThmIn asl c) = Thm asl c

{-| 
  The @Viewable@ class is used to provide a polymorphic view pattern for
  HaskHOL's primitive data types.
-}
class Viewable a b where
    {-| 
      The view pattern function for HaskHOL's primitive data types:
      
      * For types - Converts from 'HOLType' to 'HOLTypeView'.
      
      * For terms - Converts from 'HOLTerm' to 'HOLTermView'.

      * For theorems - Converts from 'HOLThm' to 'HOLThmView'.
    -}
    view :: a -> b

{- 
  Deepseq instances for the primitive data types.  These are included as they 
  are commonly used by a number of benchmarking libraries.
-}
instance NFData HOLType where
    rnf (TyVarIn b s) = rnf b `seq` rnf s
    rnf (TyAppIn s tys) = rnf s `seq` rnf tys
    rnf (UTypeIn tv tb) = rnf tv `seq` rnf tb

instance NFData TypeOp where
    rnf (TyOpVar s) = rnf s
    rnf (TyPrim s n) = rnf s `seq` rnf n
    rnf (TyDefined s n thm) = rnf s `seq` rnf n `seq` rnf thm

instance NFData HOLTerm where
    rnf (VarIn s ty) = rnf s `seq` rnf ty
    rnf (ConstIn s ty d) = rnf s `seq` rnf ty `seq` rnf d
    rnf (CombIn l r) = rnf l `seq` rnf r
    rnf (AbsIn bv bod) = rnf bv `seq` rnf bod
    rnf (TyAbsIn bty bod) = rnf bty `seq` rnf bod
    rnf (TyCombIn tm ty) = rnf tm `seq` rnf ty

instance NFData ConstTag where
    rnf Prim = ()
    rnf (Defined tm) = rnf tm
    rnf (MkAbstract s i thm) = rnf s `seq` rnf i `seq` rnf thm
    rnf (DestAbstract s i thm) = rnf s `seq` rnf i `seq` rnf thm

instance NFData HOLThm where
    rnf (ThmIn asl c) = rnf asl `seq` rnf c

{- 
  These are the automatically derived Lift instances for the primitive data
  types.  These instances are used by the compile-time operations found in
  the HaskHOL.Core.Protected module.
-}
$(deriveLiftMany [ ''TypeOp, ''HOLType
                 , ''ConstTag, ''HOLTerm
                 , ''HOLThm])
