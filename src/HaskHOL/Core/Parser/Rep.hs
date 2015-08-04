{-# LANGUAGE FlexibleInstances, IncoherentInstances, MultiParamTypeClasses, 
             TypeSynonymInstances, TypeFamilies #-}

{-|
  Module:    HaskHOL.Core.Parser.Rep
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
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
import HaskHOL.Core.State

import HaskHOL.Core.Ext.Protected

import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.Elab
import HaskHOL.Core.Parser.TypeParser
import HaskHOL.Core.Parser.TermParser

import Data.String

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

instance (IsString a, a ~ Text) => HOLTypeRep a cls thry where
    toHTy x = do ctxt <- parseContext 
                 pty <- liftEither "holTypeParser" $ holTypeParser ctxt x
                 liftEither "tyElab" $ tyElab ctxt pty

instance thry1 ~ thry2 => HOLTypeRep (PType thry1) cls thry2 where
    toHTy = serve

instance HOLTypeRep PreType cls thry where
    toHTy x = do ctxt <- parseContext
                 liftEither "tyElab" $ tyElab ctxt x

instance HOLTypeRep HOLType cls thry where
    toHTy = return

instance HOLTypeRep (Maybe HOLType) cls thry where
    toHTy Nothing = fail "toHTy: Nothing"
    toHTy (Just ty) = return ty

instance Show a => HOLTypeRep (Either a HOLType) cls thry where
    toHTy (Left err) = fail $ show err
    toHTy (Right ty) = return ty

instance (cls ~ cls1, thry ~ thry1) => 
         HOLTypeRep (HOL cls1 thry1 HOLType) cls thry where
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

instance thry1 ~ thry2 => HOLTermRep (PTerm thry1) cls thry2 where
    toHTm = serve

instance (IsString a, a~Text) => HOLTermRep a cls thry where
    toHTm x = do ctxt <- parseContext
                 ptm <- liftEither "holTermParser" $ holTermParser ctxt x
                 liftEither "elab" $ elab ctxt ptm
                
instance HOLTermRep PreTerm cls thry where
    toHTm tm = do ctxt <- parseContext
                  liftEither "elab" $ elab ctxt tm

instance HOLTermRep HOLTerm cls thry where
    toHTm = return

instance HOLTermRep (Maybe HOLTerm) cls thry where
    toHTm Nothing = fail "toHTm: Nothing"
    toHTm (Just tm) = return tm

instance Show a => HOLTermRep (Either a HOLTerm) cls thry where
    toHTm (Left err) = fail $ show err
    toHTm (Right tm) = return tm

instance (cls ~ cls1, thry ~ thry1) => 
         HOLTermRep (HOL cls1 thry1 HOLTerm) cls thry where
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

instance thry1 ~ thry2 => HOLThmRep (PThm thry1) cls thry2 where
    toHThm = serve

instance HOLThmRep HOLThm cls thry where
    toHThm = return

instance Show a => HOLThmRep (Either a HOLThm) cls thry where
    toHThm (Left err) = fail $ show err
    toHThm (Right thm) = return thm

instance (cls ~ cls1, thry ~ thry1) => 
         HOLThmRep (HOL cls1 thry1 HOLThm) cls thry where
    toHThm = id
