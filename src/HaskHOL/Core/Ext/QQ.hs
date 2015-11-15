{-# LANGUAGE ScopedTypeVariables #-}
{-|
  Module:    HaskHOL.Core.Ext.QQ
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
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
    ( baseQuoter
    , baseQQ
    , txt
    , liftParseContext
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.State.Monad
import HaskHOL.Core.Parser
import HaskHOL.Core.Ext.Protected

{-
  We require some Template Haskell primitives that shouldn't be exposed outside
  of this module, i.e. runIO
-}
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Quote


{-|
  This is the base quasi-quoter for the HaskHOL system.  When provided with a
  theory context value, it constucts a theory specific quasi-quoter that parses
  a 'String' as a term, protecting and lifting the result.

  Note that, at this point in time, we only allowing quoting at the expression
  level.
-}
baseQuoter :: CtxtName thry => TheoryPath thry -> ParseContext -> QuasiQuoter
baseQuoter thry ctxt = QuasiQuoter quoteBaseExps nothing nothing nothing
    where quoteBaseExps :: String -> Q Exp 
          quoteBaseExps x =
              let x' = textStrip $ pack x in
                if textHead x' == ':'
                then body tyElab holTypeParser $ textTail x'
                else body elab holTermParser x'
          
          nothing _ = fail "quoting here not supported"
          
          body :: Protected b 
               => (ParseContext -> a -> Catch b) 
               -> (ParseContext -> Text -> Catch a)
               -> Text -> Q Exp
          body efun pfun x' =
              case runCatch $ efun ctxt =<< pfun ctxt x' of
                Right res -> liftProtectedExp $ protect thry res
                Left  err -> fail $ show err

{-| 
  An instance of 'baseQuoter' for the core theory context, 'ctxtBase'.
  Example:

  > [baseQQ| x = y |]

  will parse the provided string and construct the 'HOLTerm' @x = y@ at compile
  time.  Note that this term is protected, such that it has to be accessed via
  'serve'.  This is advantageous in computations that may be run many times, 
  for example:

  > do tm <- serve [baseQQ| x = y |]
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
baseQQ :: QuasiQuoter
baseQQ = baseQuoter ctxtBase initParseContext

{-|
  This is a specialized quasi-quoter for 'Text's.  It can be used to strip
  white space and automatically escape special characters.  It is typically used
  in conjunction with 'toHTm' directly or indirectly.
-}
txt :: QuasiQuoter
txt = QuasiQuoter quoteStrExp nothing nothing nothing
    where quoteStrExp x = [| textStrip $ pack $(litE $ StringL x) |]
          nothing _ = fail "quoting here not supported"

liftParseContext :: TheoryPath thry -> Q Exp
liftParseContext ctxt =
    lift =<< runIO (runHOLProof False parseContext ctxt)
