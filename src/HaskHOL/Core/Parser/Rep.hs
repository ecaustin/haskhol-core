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
class HOLTypeRep a m where
    -- | Conversion from alternative type @a@ to 'HOLType'.
    toHTy :: a -> m HOLType

instance HOLTypeRep (PType thry) (HOL cls thry) where
    toHTy = serve

instance (IsString a, a ~ Text) => HOLTypeRep a (HOL cls thry) where
    toHTy x = do ctxt <- parseContext 
                 pty <- holTypeParser ctxt x
                 tyElab ctxt pty

instance HOLTypeRep PreType (HOL cls thry) where
    toHTy x = do ctxt <- parseContext
                 tyElab ctxt x

instance Monad m => HOLTypeRep HOLType m where
    toHTy = return

instance HOLTypeRep (m HOLType) m where
    toHTy = id

instance (MonadThrow m1, m1 ~ Catch, MonadThrow m2) => 
         HOLTypeRep (m1 HOLType) m2 where
    toHTy m = case runCatch m of
                Right res -> return res
                Left err  -> throwM err

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
class HOLTermRep a m where
    -- | Conversion from alternative type @a@ to 'HOLTerm'.
    toHTm :: a -> m HOLTerm

instance HOLTermRep (PTerm thry) (HOL cls thry) where
    toHTm = serve

instance (IsString a, a ~ Text) => HOLTermRep a (HOL cls thry) where
    toHTm x = do ctxt <- parseContext
                 ptm <- holTermParser ctxt x
                 elab ctxt ptm
                
instance HOLTermRep PreTerm (HOL cls thry) where
    toHTm tm = do ctxt <- parseContext
                  elab ctxt tm

instance Monad m => HOLTermRep HOLTerm m where
    toHTm = return

instance HOLTermRep (m HOLTerm) m where
    toHTm = id

instance (MonadThrow m1, m1 ~ Catch, MonadThrow m2) => 
         HOLTermRep (m1 HOLTerm) m2 where
    toHTm m = case runCatch m of
                Right res -> return res
                Left err  -> throwM err

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
class HOLThmRep a m where
    -- | Conversion from alternative type @a@ to 'HOLThm'.
    toHThm :: a -> m HOLThm

instance HOLThmRep (PThm thry) (HOL cls thry) where
    toHThm = serve

instance Monad m => HOLThmRep HOLThm m where
    toHThm = return

instance HOLThmRep (m HOLThm) m where
    toHThm = id

instance (MonadThrow m1, m1 ~ Catch, MonadThrow m2) => 
         HOLThmRep (m1 HOLThm) m2 where
    toHThm m = case runCatch m of
                 Right res -> return res
                 Left err  -> throwM err
