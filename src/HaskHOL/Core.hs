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
      newConstant        -- :: HOLTypeRep ty thry => 
                         --    String -> ty -> HOL Theory thry ()
    , newAxiom           -- :: HOLTermRep tm thry => 
                         --    String -> tm -> HOL Theory thry HOLThm
    , newBasicDefinition -- :: HOLTermRep tm thry => 
                         --    tm -> HOL Theory thry HOLThm
    , makeOverloadable   -- :: HOLTypeRep ty thry => 
                         --    String -> ty -> HOL Theory thry ()
    , reduceInterface    -- :: HOLTermRep tm thry => 
                         --    String -> tm -> HOL Theory thry ()
    , overrideInterface  -- :: HOLTermRep tm thry => 
                         --    String -> tm -> HOL Theory thry ()
    , overloadInterface  -- :: HOLTermRep tm thry => 
                         --    String -> tm -> HOL Theory thry ()
    , prioritizeOverload -- :: HOLTypeRep ty thry => ty -> HOL Theory thry ()
    , newTypeAbbrev      -- :: HOLTypeRep ty thry => 
                         --    String -> ty -> HOL Theory thry ()
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
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State hiding ( newConstant, newAxiom, newBasicDefinition )
import HaskHOL.Core.Basics
import HaskHOL.Core.Parser hiding ( makeOverloadable, reduceInterface 
                                  , overrideInterface, overloadInterface
                                  , prioritizeOverload, newTypeAbbrev )
import HaskHOL.Core.Printer
import HaskHOL.Core.Ext

import qualified HaskHOL.Core.State as S ( newConstant, newAxiom
                                         , newBasicDefinition )
import qualified HaskHOL.Core.Parser as P ( makeOverloadable, reduceInterface 
                                          , overrideInterface, overloadInterface
                                          , prioritizeOverload, newTypeAbbrev )
-- from state
{-| 
  A redefinition of 'S.newConstant' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newConstant :: HOLTypeRep ty thry => String -> ty -> HOL Theory thry ()
newConstant s = S.newConstant s <=< toHTy

{-| 
  A redefinition of 'S.newAxiom' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newAxiom :: HOLTermRep tm thry => String -> tm -> HOL Theory thry HOLThm
newAxiom s = S.newAxiom s <=< toHTm

{-| 
  A redefinition of 'S.newBasicDefinition' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newBasicDefinition :: HOLTermRep tm thry => tm -> HOL Theory thry HOLThm
newBasicDefinition = S.newBasicDefinition <=< toHTm


-- from parser
{-|
  A redefinition of 'P.makeOverloadable' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
makeOverloadable :: HOLTypeRep ty thry => String -> ty -> HOL Theory thry ()
makeOverloadable s = P.makeOverloadable s <=< toHTy

{-|
  A redefinition of 'P.reduceInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
reduceInterface :: HOLTermRep tm thry => String -> tm -> HOL Theory thry ()
reduceInterface s = P.reduceInterface s <=< toHTm

{-|
  A redefinition of 'P.overrideInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overrideInterface :: HOLTermRep tm thry => 
                     String -> tm -> HOL Theory thry ()
overrideInterface s = P.overrideInterface s <=< toHTm

{-|
  A redefinition of 'P.overloadInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overloadInterface :: HOLTermRep tm thry => String -> tm -> HOL Theory thry ()
overloadInterface s = P.overloadInterface s <=< toHTm

{-|
  A redefinition of 'P.prioritizeOverload' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
prioritizeOverload :: HOLTypeRep ty thry => ty -> HOL Theory thry ()
prioritizeOverload = P.prioritizeOverload <=< toHTy

{-|
  A redefinition of 'P.newTypeAbbrev' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
newTypeAbbrev :: HOLTypeRep ty thry => String -> ty -> HOL Theory thry ()
newTypeAbbrev s = P.newTypeAbbrev s <=< toHTy
