{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,     
             TypeFamilies, TypeSynonymInstances #-}
{-|
  Module:    HaskHOL.Core.Ext.Protected
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
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
    , protectM
    , serve
    , unsafeServe
    , unsafeProtect
    , PType
    , PTerm
    , PThm
    , liftProtectedExp
    , liftProtected
    ) where

import HaskHOL.Core.Kernel
import HaskHOL.Core.State

import HaskHOL.Core.Parser.Prims

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
    -- | Protects a value by sealing it against the current context.
    protectM :: forall cls thry. a -> HOL cls thry (PData a thry)
    protectM x = return $! protect (undefined :: TheoryPath thry) x
    {-| 
      Unseals a protected value, returning it in a monadic computation whose
      current working theory satisfies the context that the value was originally
      sealed with.
    -}
    serve :: PData a thry -> HOL cls thry a
    -- | Unseals a protected value, returning a pure, unprotected value.
    unsafeServe :: PData a thry -> a
    {- | An alias to the internal 'PData' constructor to create a fully 
         polymorphic \"protected\" value. -}
    unsafeProtect :: a -> PData a thry
    liftTy :: a -> Type
    protLift :: PData a thry -> Q Exp

instance Protected ParseContext where
    data PData ParseContext thry = PPC ParseContext
    protect _ = PPC
    serve (PPC pc) = return pc
    unsafeServe (PPC pc) = pc
    unsafeProtect = PPC
    liftTy _ = ConT ''ParseContext
    protLift (PPC pc) = conE 'ParseContext `appE` lift pc

instance Protected HOLThm where
    data PData HOLThm thry = PThm HOLThm
    protect _ = PThm
    serve (PThm thm) = return thm
    unsafeServe (PThm thm) = thm
    unsafeProtect = PThm
    liftTy _ = ConT ''HOLThm
    protLift (PThm thm) = conE 'PThm `appE` lift thm
-- | Type synonym for protected 'HOLThm's.
type PThm thry = PData HOLThm thry

instance Protected HOLTerm where
    data PData HOLTerm thry = PTm HOLTerm
    protect _ = PTm
    serve (PTm tm) = return tm
    unsafeServe (PTm tm) = tm
    unsafeProtect = PTm
    liftTy _ = ConT ''HOLTerm
    protLift (PTm tm) = conE 'PTm `appE` lift tm
-- | Type synonym for protected 'HOLTerm's.
type PTerm thry = PData HOLTerm thry

instance Protected HOLType where
    data PData HOLType thry = PTy HOLType
    protect _ = PTy
    serve (PTy ty) = return ty
    unsafeServe (PTy ty) = ty
    unsafeProtect = PTy
    liftTy _ = ConT ''HOLType
    protLift (PTy ty) = conE 'PTy `appE` lift ty
-- | Type synonym for protected 'HOLType's.
type PType thry = PData HOLType thry

instance Protected Int where
    data PData Int thry = PInt Int
    protect _ = PInt
    serve (PInt n) = return n
    unsafeServe (PInt n) = n
    unsafeProtect = PInt
    liftTy _ = ConT ''Int
    protLift (PInt n) = conE 'PInt `appE` lift n

instance Protected a => Protected [a] where
    data PData [a] thry = PList [PData a thry]
    protect c as = PList $ map (protect c) as
    serve (PList as) = mapM serve as
    unsafeServe (PList as) = map unsafeServe as
    unsafeProtect as = PList $ map unsafeProtect as
    liftTy _ = AppT ListT $ liftTy (undefined::a)
    protLift (PList as) = conE 'PList `appE` listE (map protLift as)

instance (Protected a, Protected b) => Protected (a, b) where
    data PData (a, b) thry = PPair (PData a thry) (PData b thry)
    protect c (a, b) = PPair (protect c a) (protect c b)
    serve (PPair a b) = do a' <- serve a
                           b' <- serve b
                           return (a', b')
    unsafeServe (PPair a b) = (unsafeServe a, unsafeServe b)
    unsafeProtect (a, b) = PPair (unsafeProtect a) (unsafeProtect b)
    liftTy _ = AppT (AppT (TupleT 2) (liftTy (undefined::a))) 
                 (liftTy (undefined::b))
    protLift (PPair a b) = conE 'PPair `appE` protLift a `appE` protLift b

instance (Protected a, Protected b, Protected c) => Protected (a, b, c) where
    data PData (a, b, c) thry = 
        PTrip (PData a thry) (PData b thry) (PData c thry)
    protect ctx (a, b, c) = 
        PTrip (protect ctx a) (protect ctx b) (protect ctx c)
    serve (PTrip a b c) = do a' <- serve a
                             b' <- serve b
                             c' <- serve c
                             return (a', b', c')
    unsafeServe (PTrip a b c) = (unsafeServe a, unsafeServe b, unsafeServe c)
    unsafeProtect (a, b, c) = 
        PTrip (unsafeProtect a) (unsafeProtect b) (unsafeProtect c)
    liftTy _ = AppT (AppT (AppT (TupleT 3) (liftTy (undefined::a)))
                          (liftTy (undefined::b)))
                 (liftTy (undefined::c))
    protLift (PTrip a b c) = 
        conE 'PTrip `appE` protLift a `appE` protLift b `appE` protLift c

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
