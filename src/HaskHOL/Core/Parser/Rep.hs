{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, 
             TypeSynonymInstances, UndecidableInstances #-}

{-|
  Module:    HaskHOL.Core.Parser.Rep
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines conversions for alternative type and term representations
  via the 'HOLTermRep' and 'HOLTypeRep' classes.

  The most commonly used alternative representations are strings and protected
  terms/types as produced by the "HaskHOL.Core.Ext" module.
-}
module HaskHOL.Core.Parser.Rep
    ( HOLTypeRep(..)
    , HOLTermRep(..)
    ) where

import HaskHOL.Core.Kernel
import HaskHOL.Core.State

import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.Elab
import {-# SOURCE #-} HaskHOL.Core.Parser (holTermParser, holTypeParser)

{-|
  The 'HOLTypeRep' class provides a conversion from an alternative 
  representation of types to 'HOLType' within the 'HOL' monad.

  The first parameter is the type of the alternative representation.
 
  The second parameter is the tag for the last checkpoint of the 
  current working theory.  This enables us to have a conversion from 
  representations that are theory dependent without running into type 
  matchability issues.
-}
class HOLTypeRep a thry | a -> thry where
    -- | Conversion from alternative type @a@ to 'HOLType'.
    toHTy :: a -> HOL cls thry HOLType

instance HOLTypeRep String a where
    toHTy x = 
        do ctxt <- get
           tyElab =<< liftEither "toHTy" (holTypeParser x ctxt)

instance HOLTypeRep PreType a where
    toHTy = tyElab

instance HOLTypeRep HOLType a where
    toHTy = return

{-|
  The 'HOLTermRep' class provides a conversion from an alternative 
  representation of terms to 'HOLTerm' within the 'HOL' monad.

  The first parameter is the type of the alternative representation.
 
  The second parameter is the tag for the last checkpoint of the 
  current working theory.  This enables us to have a conversion from 
  representations that are theory dependent, i.e. 'PTerm', without running into
  type matchability issues.
-}
class HOLTermRep a thry | a -> thry where
    -- | Conversion from alternative type @a@ to 'HOLTerm'.
    toHTm :: a -> HOL cls thry HOLTerm

instance HOLTermRep String a where
    toHTm x = 
        do ctxt <- get
           elab =<< liftEither "toHTm" (holTermParser x ctxt)
                
instance HOLTermRep PreTerm a where
    toHTm = elab

instance HOLTermRep HOLTerm a where
    toHTm = return
