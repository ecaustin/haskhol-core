{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,     
             TypeFamilies, TypeSynonymInstances #-}
{-|
  Module:    HaskHOL.Core.Ext.Protected
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module defines a mechanism for sealing and unsealing values against a
  given context.  Additionally, a number of compile time operations are
  provided that leverage this technique as an example of how it can be used.

  The basic goal behind the content of this module is to recapture some of the
  efficiency lost as a result of moving from an impure, interpretted host 
  language to a pure, compiled one.  We do this by forcing the evaluation of 
  large computations, usually proofs, such that they are only run once. To
  maintain soundness of our proof system, we must track what information
  was used to force the computation and guarantee that information is present
  in all cases where this new value is to be used.  This is the purpose of the
  @Protected@ class and the 'liftProtectedExp' and 'liftProtected' methods.
-}
module HaskHOL.Core.Ext.Protected
    ( Protected
    , PData
    , protect
    , serve
    , PType
    , PTerm
    , liftProtectedExp
    , liftProtected
    ) where

import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))

-- protected values
{-|
  The Protected class is the associated type class that facilitates our
  protect/serve protection mechanism.

  It defines:

  * A data wrapper for our protected type.

  * Conversions to/from this new type, protect and serve.

  * Some boilerplate code to enable template haskell lifting.
-}
class Lift a => Protected a where
    -- | The associated type for the 'Protected' class.
    data PData a thry
    -- | Protects a value by sealing it against a provided context.
    protect :: TheoryPath thry -> a -> PData a thry
    {-| 
      Unseals a protected value, returning it in a monadic computation whose
      current working theory satisfies the context that the value was originally
      sealed with.
    -}
    serve :: PData a thry -> HOL cls thry a
    -- | Unseals a protected value, returning a pure, unprotected value.
    liftTy :: a -> Type
    protLift :: PData a thry -> Q Exp

instance Protected HOLTerm where
    data PData HOLTerm thry = PTm HOLTerm
    protect _ = PTm
    serve (PTm tm) = return tm
    liftTy _ = ConT ''HOLTerm
    protLift (PTm tm) = conE 'PTm `appE` lift tm
-- | Type synonym for protected 'HOLTerm's.
type PTerm thry = PData HOLTerm thry

instance Protected HOLType where
    data PData HOLType thry = PTy HOLType
    protect _ = PTy
    serve (PTy ty) = return ty
    liftTy _ = ConT ''HOLType
    protLift (PTy ty) = conE 'PTy `appE` lift ty
-- | Type synonym for protected 'HOLType's.
type PType thry = PData HOLType thry

{-
  Builds the theory contrainst for a lifted, protected value.  
  For example:
  
  > buildThryType (x::PData a BoolType)

  builds the context

  > forall thry. BoolCtxt thry => PData a thry
-}
buildThryType :: forall a thry. (Protected a, CtxtName thry) => 
                                PData a thry -> Q Type
buildThryType _ =
    do tyname <- newName "thry"
       let cls = ConT (mkName $ ctxtName (undefined::thry)) `AppT` VarT tyname
       return . ForallT [PlainTV tyname] [cls] . 
         AppT (AppT (ConT ''PData) $ liftTy (undefined :: a)) $ 
           VarT tyname
         

{-| 
  Lifts a protected data value as an expression using an ascribed type.
  For example:

  > liftProtectedExp (x::PData a Bool)

  produces the following spliceable expression

  > [| x :: forall thry. BoolCtxt thry => PData a Bool |]
-}
liftProtectedExp :: (Protected a, CtxtName thry) => PData a thry -> Q Exp
liftProtectedExp pdata =
  do pdata' <- protLift pdata
     ty <- buildThryType pdata
     return $! SigE pdata' ty

{-| 
  Lifts a protected data value as a declaration of a given name with an ascribed
  type signature.
  For example:

  > liftProtected "protX" (x::PData a Bool)

  produces the following list of spliceable declarations

  > [ [d| protX :: forall thry. BoolCtxt thry => PData a Bool |]
  > , [d| protX = x |] ]

  See 'extractAxiom' for a basic example of how this function may be used.
-}
liftProtected :: (Protected a, CtxtName thry) => 
                 String -> PData a thry -> Q [Dec]
liftProtected lbl pdata =
  do pdata' <- protLift pdata
     ty <- buildThryType pdata
     let name = mkName lbl
         tysig = SigD name ty
         dec = ValD (VarP name) (NormalB pdata') []
     return [tysig, dec]
