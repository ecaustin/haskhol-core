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
      -- * Stateful Primitives, including complete overloadings for Kernel, Basic, and Parser libraries.
    , module HaskHOL.Core.State
      -- * HaskHOL Pretty Printers
    , module HaskHOL.Core.Printer
      -- * HaskHOL Core Extensions
    , module HaskHOL.Core.Ext
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.State
import HaskHOL.Core.Printer
import HaskHOL.Core.Ext
