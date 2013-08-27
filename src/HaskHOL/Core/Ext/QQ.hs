{-|
  Module:    HaskHOL.Core.Ext.QQ
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines a mechanism for compile time quasi-quoting of 'HOLTerm's.
  The 'baseQuoter' method constructs a theory specific quasi-quoter that parses
  'HOLTerm's at the expression level using 'toHTm'.  An example, 'base' is 
  provided to demonstrate how this process works.

  Additionally, a specialized quasi-quoter for 'String's is provided that
  escapes special characters and trims white-space.  This can be helpful when
  expressing 'HOLTerm's as 'String's, i.e. @\"\\ x . x\"@.
-}
module HaskHOL.Core.Ext.QQ
    ( baseQuoter  -- :: Typeable thry => HOLContext thry -> QuasiQuoter
    , base        -- :: QuasiQuoter
    , str         -- :: QuasiQuoter
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.State
import HaskHOL.Core.Parser
import HaskHOL.Core.Ext.Protected

{-
  We require some Template Haskell primitives that shouldn't be exposed outside
  of this module, i.e. runIO
-}
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift(..))

-- Only used here, not really necessary to include in Core.Lib
import Data.Char (isSpace)

{-|
  This is the base quasi-quoter for the HaskHOL system.  When provided with a
  theory context value, it constucts a theory specific quasi-quoter that parses
  a 'String' as a term, protecting and lifting the result.

  Note that, at this point in time, we only allowing quoting at the expression
  level.
-}
baseQuoter :: Typeable thry => HOLContext thry -> QuasiQuoter
baseQuoter ctxt = QuasiQuoter quoteBaseExps nothing nothing nothing
    where quoteBaseExps x =
              liftProtectedExp =<< 
                (runIO . evalHOLCtxt (liftM (protect ctxt) $ toHTm x) $ ctxt)
          nothing _ = fail "quoting here not supported"

{-| 
  An instance of 'baseQuoter' for the core theory context, 'ctxtBase'.
  Example:

  > [base| x = y |]

  will parse the provided string and construct the 'HOLTerm' @x = y@ at compile
  time.  Note that this term is protected, such that it has to be accessed via
  'serve'.  This is advantageous in computations that may be run many times, 
  for example:

  > do tm <- serve [base| x = y |]
  >    ...

  will parse the term exactly once, only checking the @thry@ tag of the
  computation for each evaluation.  Conversely,

  > do tm <- toHTm "x = y"
  >    ...

  will parse the term for every evaluation of that computation.  Generally, the
  use of 'toHTm' is reserved for run time parsing and in larger computations
  that themselves are evaluated at copmile time to minimize the amount of work
  Template Haskell has to do.
-}
base :: QuasiQuoter
base = baseQuoter ctxtBase

{-|
  This is a specialized quasi-quoter for 'String's.  It can be used to strip
  white space and automatically escape special characters.  It is typically used
  in conjunction with 'toHTm' directly or indirectly.
-}
str :: QuasiQuoter
str = QuasiQuoter quoteStrExp nothing nothing nothing
    where quoteStrExp x = lift $ trim x
          trim = dropWhile isSpace . dropWhileEnd isSpace
          nothing _ = fail "quoting here not supported"
