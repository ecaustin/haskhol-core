{-# LANGUAGE TypeFamilies #-}
module HaskHOL.Core.Printer.Prims where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

import Control.Lens

import Language.Haskell.TH.Lift

data PrintContext = PrintContext
    { _interface :: ![(Text, (Text, HOLType))]
    } deriving Typeable

deriveLift ''PrintContext

makeLenses ''PrintContext

deriveSafeCopy 0 'base ''PrintContext

putPrintContext :: PrintContext -> Update PrintContext ()
putPrintContext = put

getPrintContext :: Query PrintContext PrintContext
getPrintContext = ask

makeAcidic ''PrintContext ['putPrintContext, 'getPrintContext]

initPrintContext :: PrintContext
initPrintContext = PrintContext []

printContext :: HOL cls thry PrintContext
printContext =
    do acid <- openLocalStateHOL initPrintContext
       ctxt <- queryHOL acid GetPrintContext
       closeAcidStateHOL acid
       return ctxt

viewPrintContext :: Getting a PrintContext a -> HOL cls thry a
viewPrintContext f = liftM (view f) printContext

overPrintContext :: Setting (->) PrintContext PrintContext a a -> (a -> a) 
                 -> HOL Theory thry ()
overPrintContext f p =
    do acid <- openLocalStateHOL initPrintContext
       ctxt <- queryHOL acid GetPrintContext
       updateHOL acid (PutPrintContext $ over f p ctxt)
       closeAcidStateHOL acid
