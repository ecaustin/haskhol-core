{-# LANGUAGE FlexibleContexts #-}
{-|
  Module:    HaskHOL.Core
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module is the one to import for users looking to include the entirety of
  the core of the HaskHOL proof system.  It re-exports all of the core 
  sub-modules in addition to a number of overloaded functions that work with
  'HOLTermRep' and 'HOLTypeRep' representations for convenience reasons.
-}
module HaskHOL.Core
    ( -- * Library and Utility Functions
      module HaskHOL.Core.Lib
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
      -- * HaskHOL Core Overloadings
    , module HaskHOL.Core.Overloadings
    , Constraint -- | A re-export of 'Constraint' from @GHC.Prim@.
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel hiding 
-- hidden for overloadings
    ( mkUType, mkUTypes, typeMatch
    , mkVar, mkAbs, mkComb, mkTyAbs, mkTyComb
    , primREFL, primTRANS, primMK_COMB, primABS, primBETA, primASSUME, primEQ_MP
    , primDEDUCT_ANTISYM, primINST_TYPE, primINST_TYPE_FULL, primINST, primTYABS
    , primTYAPP2, primTYAPP, primTYBETA
-- stateless primitives hidden to encourage stateful alternatives
    , axiomThm, newDefinedConst, newDefinedTypeOp
    )
import HaskHOL.Core.State hiding 
-- hidden for overloadings
    ( mkType, mkFunTy, newConstant, newAxiom, newBasicDefinition 
    )
import HaskHOL.Core.Basics hiding 
-- hidden for overloadings
    ( tysubst, mkEq, subst, listMkComb, listMkTyComb, listMkAbs, listMkTyAbs
    , rator, rand, bndvar, body, bndvarTyabs, bodyTyabs
    , mkBinop, listMkBinop, mkBinder, mkTyBinder
    )
import HaskHOL.Core.Parser hiding 
 -- hidden for overloadings
    ( makeOverloadable, reduceInterface 
    , overrideInterface, overloadInterface
    , prioritizeOverload, newTypeAbbrev 
    )
import HaskHOL.Core.Printer
import HaskHOL.Core.Ext
import HaskHOL.Core.Overloadings

-- This re-export has to exist at the top-most module for some reason?
import GHC.Prim (Constraint)


