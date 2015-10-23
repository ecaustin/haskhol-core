{-# LANGUAGE FlexibleContexts #-}
{-|
  Module:    HaskHOL.Core
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module is the one to import for users looking to include the entirety of
  the core of the HaskHOL proof system.  It re-exports all of the core 
  sub-modules in addition to a number of overloaded functions that work with
  'HOLTermRep' and 'HOLTypeRep' representations for convenience reasons.
-}
module HaskHOL.Core
    ( -- * 'HOLTermRep' and 'HOLTypeRep' Overloads
      newConstant
    , newAxiom
    , newBasicDefinition
    , makeOverloadable
    , reduceInterface
    , overrideInterface
    , overloadInterface
    , prioritizeOverload
    , newTypeAbbrev
    , primREFL
    , primTRANS
    , primMK_COMB
    , primABS
    , primBETA
    , primASSUME
    , primEQ_MP
    , primDEDUCT_ANTISYM
    , primINST_TYPE
    , primINST_TYPE_FULL
    , primINST
    , primTYABS
    , primTYAPP2
    , primTYAPP
    , primTYBETA
      -- * Library and Utility Functions
    , module HaskHOL.Core.Lib
      -- * Logical Kernel
    , module HaskHOL.Core.Kernel
      -- * Stateful Primitives
    , module HaskHOL.Core.State
      -- * Basic Derived Type and Term Functions
    , module HaskHOL.Core.Basics
      -- * HaskHOL Parsers
    , module HaskHOL.Core.Parser
      -- * HaskHOL Pretty Printers
    , module HaskHOL.Core.Printer
      -- * HaskHOL Core Extensions
    , module HaskHOL.Core.Ext
    , Constraint -- | A re-export of 'Constraint' from @GHC.Prim@.
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel hiding ( axiomThm, newDefinedConst, newDefinedTypeOp
                                  , primREFL, primTRANS, primMK_COMB
                                  , primABS, primBETA, primASSUME
                                  , primEQ_MP, primDEDUCT_ANTISYM
                                  , primINST_TYPE, primINST_TYPE_FULL
                                  , primINST, primTYABS, primTYAPP2
                                  , primTYAPP, primTYBETA )
import HaskHOL.Core.State hiding ( newConstant, newAxiom, newBasicDefinition )
import HaskHOL.Core.Basics
import HaskHOL.Core.Parser hiding ( makeOverloadable, reduceInterface 
                                  , overrideInterface, overloadInterface
                                  , prioritizeOverload, newTypeAbbrev )
import HaskHOL.Core.Printer
import HaskHOL.Core.Ext

import qualified HaskHOL.Core.Kernel as K
import qualified HaskHOL.Core.State as S
import qualified HaskHOL.Core.Parser as P

-- This re-export has to exist at the top-most module for some reason?
import GHC.Prim (Constraint)

-- from state
{-| 
  A redefinition of 'S.newConstant' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newConstant :: HOLTypeRep ty Theory thry => Text -> ty -> HOL Theory thry ()
newConstant s = S.newConstant s <=< toHTy

{-| 
  A redefinition of 'S.newAxiom' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newAxiom :: HOLTermRep tm Theory thry => (Text, tm) -> HOL Theory thry HOLThm
newAxiom (s, t) = S.newAxiom s =<< toHTm t

{-| 
  A redefinition of 'S.newBasicDefinition' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newBasicDefinition :: HOLTermRep tm Theory thry
                   => (Text, tm) -> HOL Theory thry HOLThm
newBasicDefinition (lbl, t) = S.newBasicDefinition lbl =<< toHTm t


-- from parser
{-|
  A redefinition of 'P.makeOverloadable' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
makeOverloadable :: HOLTypeRep ty Theory thry 
                 => Text -> ty -> HOL Theory thry ()
makeOverloadable s = P.makeOverloadable s <=< toHTy

{-|
  A redefinition of 'P.reduceInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
reduceInterface :: HOLTermRep tm Theory thry
                => Text -> tm -> HOL Theory thry ()
reduceInterface s = P.reduceInterface s <=< toHTm

{-|
  A redefinition of 'P.overrideInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overrideInterface :: HOLTermRep tm Theory thry 
                  => Text -> tm -> HOL Theory thry ()
overrideInterface s = P.overrideInterface s <=< toHTm

{-|
  A redefinition of 'P.overloadInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overloadInterface :: HOLTermRep tm Theory thry 
                  => Text -> tm -> HOL Theory thry ()
overloadInterface s = P.overloadInterface s <=< toHTm

{-|
  A redefinition of 'P.prioritizeOverload' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
prioritizeOverload :: HOLTypeRep ty Theory thry => ty -> HOL Theory thry ()
prioritizeOverload = P.prioritizeOverload <=< toHTy

{-|
  A redefinition of 'P.newTypeAbbrev' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
newTypeAbbrev :: HOLTypeRep ty Theory thry => Text -> ty -> HOL Theory thry ()
newTypeAbbrev s = P.newTypeAbbrev s <=< toHTy

-- from kernel
primREFL :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primREFL = liftM (K.primREFL) . toHTm

primTRANS :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
          => thm1 -> thm2 -> HOL cls thry HOLThm
primTRANS pthm1 pthm2 =
    do thm1 <- toHThm pthm1
       thm2 <- toHThm pthm2
       K.primTRANS thm1 thm2

primMK_COMB :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry)
            => thm1 -> thm2 -> HOL cls thry HOLThm
primMK_COMB pthm1 pthm2 =
    do thm1 <- toHThm pthm1
       thm2 <- toHThm pthm2
       K.primMK_COMB thm1 thm2

primABS :: (HOLTermRep tm cls thry, HOLThmRep thm cls thry) 
        => tm -> thm -> HOL cls thry HOLThm  
primABS ptm pthm =
    do tm <- toHTm ptm
       thm <- toHThm pthm
       K.primABS tm thm

primBETA :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primBETA = K.primBETA <=< toHTm

primASSUME :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primASSUME = K.primASSUME <=< toHTm

primEQ_MP :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
          => thm1 -> thm2 -> HOL cls thry HOLThm
primEQ_MP pthm1 pthm2 =
    do thm1 <- toHThm pthm1
       thm2 <- toHThm pthm2
       K.primEQ_MP thm1 thm2

primDEDUCT_ANTISYM :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
                   => thm1 -> thm2 -> HOL cls thry HOLThm
primDEDUCT_ANTISYM pthm1 pthm2 =
    pure K.primDEDUCT_ANTISYM <*> toHThm pthm1 <*> toHThm pthm2

primINST_TYPE :: (HOLThmRep thm cls thry, Inst a b) 
              => [(a, b)] -> thm -> HOL cls thry HOLThm
primINST_TYPE tyenv = liftM (K.primINST_TYPE tyenv) . toHThm

primINST_TYPE_FULL :: HOLThmRep thm cls thry 
                   => SubstTrip -> thm -> HOL cls thry HOLThm
primINST_TYPE_FULL tyenv = liftM (K.primINST_TYPE_FULL tyenv) . toHThm

primINST :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
             HOLThmRep thm cls thry)
         => [(tm1, tm2)] -> thm -> HOL cls thry HOLThm
primINST ptmenv pthm = 
    do tmenv <- mapM (toHTm `ffCombM` toHTm) ptmenv
       K.primINST tmenv =<< toHThm pthm

primTYABS :: (HOLTypeRep ty cls thry, HOLThmRep thm cls thry) 
          => ty -> thm -> HOL cls thry HOLThm
primTYABS pty pthm =
    do ty <- toHTy pty
       thm <- toHThm pthm
       K.primTYABS ty thm

primTYAPP2 :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry, 
               HOLThmRep thm cls thry) 
           => ty1 -> ty2 -> thm -> HOL cls thry HOLThm
primTYAPP2 pty1 pty2 pthm =
    do ty1 <- toHTy pty1
       ty2 <- toHTy pty2
       thm <- toHThm pthm
       K.primTYAPP2 ty1 ty2 thm

primTYAPP :: (HOLTypeRep ty cls thry, HOLThmRep thm cls thry) 
          => ty -> thm -> HOL cls thry HOLThm
primTYAPP pty pthm =
    do ty <- toHTy pty
       thm <- toHThm pthm
       K.primTYAPP ty thm

primTYBETA :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primTYBETA = K.primTYBETA <=< toHTm
