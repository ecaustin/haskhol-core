{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, 
             TypeSynonymInstances #-}
{-|
  Module:    HaskHOL.Core.Parser.Rep
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module defines conversions for alternative type and term representations
  via the 'HOLTermRep' and 'HOLTypeRep' classes.

  The most commonly used alternative representations are 'Text' and protected
  terms/types as produced by the "HaskHOL.Core.Ext" module.
-}
module HaskHOL.Core.Parser.Rep where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

import HaskHOL.Core.Ext.Protected

import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.Elab
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.Parser.TermParser

-- Types
{-|
  The 'HOLTypeRep' class provides a conversion from an alternative 
  representation of types to 'HOLType' within the 'HOL' monad.

  The first parameter is the type of the alternative representation.
 
  The second parameter is the classification of the monad computation.  This
  is used to assert type equality when converting between monadic 
  representations.

  The third parameter is the tag for the last checkpoint of the 
  current working theory.  This enables us to safely have conversions between 
  representations that are theory dependent.
-}
class HOLTypeRep a cls thry where
    -- | Conversion from alternative type @a@ to 'HOLType'.
    toHTy :: a -> HOL cls thry HOLType

instance HOLTypeRep Text cls thry where
    toHTy x = do ctxt <- parseContext 
                 pty <- holTypeParser ctxt x
                 tyElab ctxt pty

instance thry1 ~ thry2 => HOLTypeRep (PType thry1) cls thry2 where
    toHTy = serve

instance HOLTypeRep PreType cls thry where
    toHTy x = do ctxt <- parseContext
                 tyElab ctxt x

instance HOLTypeRep HOLType cls thry where
    toHTy = return

instance HOLTypeRep (Catch HOLType) cls thry where
    toHTy = either (fail . show) return . runCatch

instance (cls1 ~ cls2, thry1 ~ thry2) => 
         HOLTypeRep (HOL cls1 thry1 HOLType) cls2 thry2 where
    toHTy = id

-- Terms
{-|
  The 'HOLTermRep' class provides a conversion from an alternative 
  representation of terms to 'HOLTerm' within the 'HOL' monad.

  The second parameter is the classification of the monad computation.  This
  is used to assert type equality when converting between monadic 
  representations.

  The third parameter is the tag for the last checkpoint of the 
  current working theory.  This enables us to safely have conversions between 
  representations that are theory dependent.
-}
class HOLTermRep a cls thry where
    -- | Conversion from alternative type @a@ to 'HOLTerm'.
    toHTm :: a -> HOL cls thry HOLTerm

instance HOLTermRep Text cls thry where
    toHTm x = do ctxt <- parseContext
                 ptm <- holTermParser ctxt x
                 elab ctxt ptm

instance thry1 ~ thry2 => HOLTermRep (PTerm thry1) cls thry2 where
    toHTm = serve
                
instance HOLTermRep PreTerm cls thry where
    toHTm tm = do ctxt <- parseContext
                  elab ctxt tm

instance  HOLTermRep HOLTerm cls thry where
    toHTm = return

instance HOLTermRep (Catch HOLTerm) cls thry where
    toHTm = either (fail . show) return . runCatch

instance (cls1 ~ cls2, thry1 ~ thry2) => 
         HOLTermRep (HOL cls1 thry1 HOLTerm) cls2 thry2 where
    toHTm = id

-- Theorems
{-|
  The 'HOLThmRep' class provides a conversion from an alternative 
  representation of theorems to 'HOLThm' within the 'HOL' monad.

  The second parameter is the classification of the monad computation.  This
  is used to assert type equality when converting between monadic 
  representations.

  The third parameter is the tag for the last checkpoint of the 
  current working theory.  This enables us to safely have conversions between 
  representations that are theory dependent.
-}
class HOLThmRep a cls thry where
    -- | Conversion from alternative type @a@ to 'HOLThm'.
    toHThm :: a -> HOL cls thry HOLThm

instance HOLThmRep HOLThm cls thry where
    toHThm = return

instance HOLThmRep (Catch HOLThm) cls thry where
    toHThm = either (fail . show) return . runCatch

instance (cls1 ~ cls2, thry1 ~ thry2) => 
         HOLThmRep (HOL cls1 thry1 HOLThm) cls2 thry2 where
    toHThm = id
