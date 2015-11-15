{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
{-|
  Module:    HaskHOL.Core.Lib.Families
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module defines type families for basic, type-level boolean computation.
  It is implemented separately given GHC's limitations on exporting type
  families with operators for names.
-}
module HaskHOL.Core.Lib.Families where

-- | Type family equality between types of the same kind.
type family (a :: k) == (b :: k) :: Bool

-- | Type family disjunction.
type family ((a :: Bool) || (b :: Bool)) :: Bool
type instance 'True  || a = 'True
type instance a || 'True  = 'True
type instance 'False || a = a
type instance a || 'False = a

-- | Type family conjunction.
type family ((a :: Bool) && (b :: Bool)) :: Bool
type instance 'True  && a = a
type instance 'False && a = 'False
