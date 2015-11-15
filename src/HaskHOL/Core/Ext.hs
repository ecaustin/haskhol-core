{-# LANGUAGE ScopedTypeVariables #-}
{-|
  Module:    HaskHOL.Core.Ext
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module exports HaskHOL's non-trivial extensions to the underlying HOL
  system, i.e. the compile time operations.  These operations are split into
  three categories:

  * Methods related to the Protect and Serve Mechanism for sealing and unsealing
    data against a provided theory context.

  * Methods related to quasi-quoting of 'HOLTerm's.  

  * Methods related to compile time extension and caching of theory contexts.
-}
module HaskHOL.Core.Ext
    ( -- * Protected Data Methods
       -- $Protect
      module HaskHOL.Core.Ext.Protected
      -- * Quasi-Quoter Methods
       -- $QQ
    , module HaskHOL.Core.Ext.QQ
    , module Language.Haskell.TH {-|
        Re-exports 'Q', 'Dec', and 'Exp' for the purpose of writing type
        signatures external to this module.
      -}
    , module Language.Haskell.TH.Quote {-|
        Re-exports 'QuasiQuoter' for the purpose of writing type signatures
        external to this module.
      -}
    ) where

import HaskHOL.Core.Ext.Protected
import HaskHOL.Core.Ext.QQ

import Language.Haskell.TH (Q, Dec, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax  

import Prelude hiding (FilePath)

-- Documentation copied from sub-modules

{-$Protect
  The basic goal behind the Protect and Serve mechanism is to recapture some of
  the efficiency lost as a result of moving from an impure, interpretted host 
  language to a pure, compiled one.  We do this by forcing the evaluation of 
  large computations, usually proofs, such that they are only run once. To
  maintain soundness of our proof system, we must track what information
  was used to force the computation and guarantee that information is present
  in all cases where this new value is to be used.  This is the purpose of the
  @Protected@ class and the 'liftProtectedExp' and 'liftProtected' methods.
-}

{-$QQ
  Quasi-quoting provides a way to parse 'HOLTerm's at compile time safely.
  Just as with proofs, we seal these terms against the theory context used to
  parse them with 'protect' and 'serve' to preserve soundness.  See the
  documentation for 'base' for a brief discussion on when quasi-quoting should
  be used vs. 'toHTm'.
-}
