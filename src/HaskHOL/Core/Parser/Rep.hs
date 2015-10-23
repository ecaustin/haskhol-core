{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, 
             TypeSynonymInstances #-}
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

instance {-# OVERLAPPABLE #-} (MonadThrow m, m ~ Catch) => 
         HOLTypeRep (m HOLType) cls thry where
    toHTy m = case runCatch m of
                Right res -> return res
                Left err  -> throwM err

instance HOLTypeRep (PType thry) cls thry where
    toHTy = serve

instance HOLTypeRep PreType cls thry where
    toHTy x = do ctxt <- parseContext
                 tyElab ctxt x

instance HOLTypeRep HOLType cls thry where
    toHTy = return

instance {-# OVERLAPPING #-} (cls1 ~ cls2, thry1 ~ thry2) => 
         HOLTypeRep (HOL cls thry HOLType) cls thry where
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

instance {-# OVERLAPPABLE #-} (MonadThrow m, m ~ Catch) => 
         HOLTermRep (m HOLTerm) cls thry where
    toHTm m = case runCatch m of
                Right res -> return res
                Left err  -> throwM err

instance HOLTermRep (PTerm thry) cls thry where
    toHTm = serve
                
instance HOLTermRep PreTerm cls thry where
    toHTm tm = do ctxt <- parseContext
                  elab ctxt tm

instance  HOLTermRep HOLTerm cls thry where
    toHTm = return

instance {-# OVERLAPPING #-} (cls1 ~ cls2, thry1 ~ thry2) => 
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

instance {-# OVERLAPPABLE #-} (MonadThrow m, m ~ Catch) => 
         HOLThmRep (m HOLThm) cls thry where
    toHThm m = case runCatch m of
                 Right res -> return res
                 Left err  -> throwM err

instance HOLThmRep (PThm thry) cls thry where
    toHThm = serve

instance HOLThmRep HOLThm cls thry where
    toHThm = return

instance {-# OVERLAPPING #-} (cls1 ~ cls2, thry1 ~ thry2) => 
         HOLThmRep (HOL cls1 thry1 HOLThm) cls2 thry2 where
    toHThm = id
