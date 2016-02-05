{-# LANGUAGE TypeFamilies #-}
{-|
  Module:    HaskHOL.Core.Printer.Prims
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module implements the primitive data types and methods for HaskHOL's
  pretty printer.
-}
module HaskHOL.Core.Printer.Prims where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad
import HaskHOL.Core.Parser.Prims (initBinderOps, initTyBinderOps, initInfixOps)

import Control.Lens

import Language.Haskell.TH.Lift

data PrintContext = PrintContext
    { _interface :: ![(Text, (Text, HOLType))]
    , _binders   :: ![Text]
    , _tyBinders :: ![Text]
    , _prefixes  :: ![Text]
    , _lefts     :: ![(Text, Int)]
    , _rights    :: ![(Text, Int)]
    , _unspaced  :: ![Text]
    , _prebroken :: ![Text]
    } deriving Typeable

grabInfix :: Text -> [(Text, (Int, Text))] -> [(Text, Int)]
grabInfix a = mapFilter $ \ (x, (n, a')) -> 
    if a == a' then return (x, n) else fail' "grabInfix"

deriveLift ''PrintContext

makeLenses ''PrintContext

deriveSafeCopy 0 'base ''PrintContext

putPrintContext :: PrintContext -> Update PrintContext ()
putPrintContext = put

getPrintContext :: Query PrintContext PrintContext
getPrintContext = ask

makeAcidic ''PrintContext ['putPrintContext, 'getPrintContext]

initUnspaced :: [Text]
initUnspaced = [",", "..", "$"]

initPrebroken :: [Text]
initPrebroken = ["==>"]

-- | The initial pretty-printer context.
initPrintContext :: PrintContext
initPrintContext = PrintContext [] initBinderOps initTyBinderOps []
    (grabInfix "left" initInfixOps) (grabInfix "right" initInfixOps)
    initUnspaced initPrebroken

-- | Retrieves the current pretty-printer context.
printContext :: HOL cls thry PrintContext
printContext =
    do acid <- openLocalStateHOL initPrintContext
       ctxt <- queryHOL acid GetPrintContext
       closeAcidStateHOL acid
       return ctxt

viewPrintContext :: Getting a PrintContext a -> HOL cls thry a
viewPrintContext f = view f `fmap` printContext

overPrintContext :: Setting (->) PrintContext PrintContext a a -> (a -> a) 
                 -> HOL Theory thry ()
overPrintContext f p =
    do acid <- openLocalStateHOL initPrintContext
       ctxt <- queryHOL acid GetPrintContext
       updateHOL acid (PutPrintContext $ over f p ctxt)
       closeAcidStateHOL acid

testPrintContext :: Optical (->) (->) (Const Bool) PrintContext PrintContext a a
                 -> (a -> Bool) -> HOL cls thry Bool
testPrintContext f p = views f p `fmap` printContext
