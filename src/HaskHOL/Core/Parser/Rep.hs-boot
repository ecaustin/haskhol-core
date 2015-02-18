{-# LANGUAGE MultiParamTypeClasses #-}
module HaskHOL.Core.Parser.Rep where

import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

class HOLTypeRep a cls thry where
    toHTy :: a -> HOL cls thry HOLType