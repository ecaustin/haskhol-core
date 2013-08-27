{-# LANGUAGE ExistentialQuantification, FlexibleInstances, 
             FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeSynonymInstances, 
             UndecidableInstances, ViewPatterns #-}

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
    ( Protected(protect, serve)
    , PData
    , PType
    , PTerm
    , PThm
    , liftProtectedExp       -- :: (Protected a, Typeable thry) => 
                             --    PData a thry -> Q Exp
    , liftProtected          -- :: (Protected a, Typeable thry) => 
                             --    String -> PData a thry -> Q [Dec]
    , proveCompileTime       -- :: Typeable thry => HOLContext thry -> String ->
                             --    HOL Proof thry HOLThm -> Q [Dec]
    , proveCompileTimeMany   -- :: Typeable thry => HOLContext thry -> [String] 
                             --    -> HOL Proof thry [HOLThm] -> Q [Dec]
    , extractBasicDefinition -- :: Typeable thry => HOLContext thry -> 
                             --    String -> String -> Q [Dec]
    , extractAxiom           -- :: Typeable thry => 
                             --    HOLContext thry -> String -> Q [Dec]
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel hiding (typeOf)
import HaskHOL.Core.State
import HaskHOL.Core.Parser

import Data.Typeable (typeOf, typeRepArgs)
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
    data PData a thry
    -- | Protects a value by sealing it against a provided context.
    protect :: HOLContext thry -> a -> PData a thry
    {-| 
      Unseals a protected value, returning it in a monadic computation whose
      current working theory satisfies the context that the value was originally
      sealed with.
    -}
    serve :: PData a thry -> HOL cls thry a
    liftTy :: a -> Name
    protLift :: PData a thry -> Q Exp

instance Protected HOLThm where
    data PData HOLThm thry = PThm HOLThm
    protect _ = PThm
    serve (PThm thm) = return thm
    liftTy _ = ''HOLThm
    protLift (PThm thm) = conE 'PThm `appE` lift thm
-- | Type synonym for protected 'HOLThm's.
type PThm thry = PData HOLThm thry

instance Protected HOLTerm where
    data PData HOLTerm thry = PTm HOLTerm
    protect _ = PTm
    serve (PTm tm) = return tm
    liftTy _ = ''HOLTerm
    protLift (PTm tm) = conE 'PTm `appE` lift tm
-- | Type synonym for protected 'HOLTerm's.
type PTerm thry = PData HOLTerm thry

instance Protected HOLType where
    data PData HOLType thry = PTy HOLType
    protect _ = PTy
    serve (PTy ty) = return ty
    liftTy _ = ''HOLType
    protLift (PTy ty) = conE 'PTy `appE` lift ty
-- | Type synonym for protected 'HOLType's.
type PType thry = PData HOLType thry

instance HOLTermRep (PTerm thry) thry where
    toHTm = serve

instance HOLTypeRep (PType thry) thry where
    toHTy = serve


{-
  Builds the theory contrainst for a lifted, protected value.  
  For example:
  
  > buildThryType (x::PData a Bool)

  builds the context

  > forall thry. BoolCtxt thry => PData a thry
-}
buildThryType :: forall a thry. (Protected a, Typeable thry) => 
                                PData a thry -> Type
buildThryType _ =
    let tyname = mkName "thry"
        ctxtName = mkName $ topTheory ++ "Ctxt"
        cls = ClassP ctxtName [VarT tyname] in
      ForallT [PlainTV tyname] [cls] . 
        AppT (AppT (ConT ''PData) . ConT $ liftTy (undefined :: a)) $ 
             VarT tyname
  where topTheory :: String
        topTheory = 
            let base = show . head . typeRepArgs $ typeOf (undefined :: thry) in
              take (length base - 4) base

{-| 
  Lifts a protected data value as an expression using an ascribed type.
  For example:

  > liftProtectedExp (x::PData a Bool)

  produces the following spliceable expression

  > [| x :: forall thry. BoolCtxt thry => PData a Bool |]
-}
liftProtectedExp :: (Protected a, Typeable thry) => 
                    PData a thry -> Q Exp
liftProtectedExp pdata =
  do pdata' <- protLift pdata
     let ty = buildThryType pdata
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
liftProtected :: (Protected a, Typeable thry) => 
                 String -> PData a thry -> Q [Dec]
liftProtected lbl pdata =
  do pdata' <- protLift pdata
     let ty = buildThryType pdata
         name = mkName lbl
         tysig = SigD name ty
         dec = ValD (VarP name) (NormalB pdata') []
     return [tysig, dec]


-- Compile Time Proof
--EvNote: long-term idea: change debuging printing to be based on cabal flag
{-|
  Evaluates a proof compilation, protects it with the theory used to evaluate
  it, and then lifts it as a declaration of a given name with an ascribed type
  signature.

  Relies internally on 'protect' and 'liftProtected' to guarantee that the
  resultant theorem is sealed with the right type.
-}
proveCompileTime :: Typeable thry => HOLContext thry -> String -> 
                                     HOL Proof thry HOLThm -> Q [Dec]
proveCompileTime ctx lbl th =
  do thm <- runIO $ 
              do putStr $ "proving: " ++ lbl ++ "..."
                 thm <- evalHOLCtxt (setBenignFlag FlagDebug >> th) ctx
                 putStrLn "...proved."
                 return thm
     liftProtected lbl $ protect ctx thm

{-|
  A version of 'proveCompileTime' that works for a proof computation returning
  multiple theorems.

  Note that each resultant theorem must have a unique, provided name.
-}
proveCompileTimeMany :: Typeable thry => HOLContext thry -> [String] -> 
                                         HOL Proof thry [HOLThm] -> Q [Dec]
proveCompileTimeMany ctx lbls ths =
    let n = length lbls in
      do thms <- runIO $ 
                   do putStrLn $ "proving " ++ show n ++ " theorems"
                      thms <- evalHOLCtxt (setBenignFlag FlagDebug >> ths) ctx
                      if length thms /= n
                         then fail $ "proveCompileTimeMany: number of " ++ 
                                     "theorems and labels does not agree."
                         else do putStrLn $ unwords lbls ++ " proved."
                                 return thms
         liftM concat . mapM (\ (lbl, thm) -> liftProtected lbl $ 
                                                protect ctx thm) $ zip lbls thms

-- Extraction functions for Core State values
{-|
  Extracts a basic term definition from a provided context, protecting and 
  lifting it with 'liftProtected'.  The extraction is performed by looking for 
  a definition whose left hand side matches a provided constant name.
  For example:

  > extractBasicDefinition ctxtBool "defT" "T"

  will return the spliceable list of declarations for the following theorem

  @ |- T = (\ p:bool . p) = (\ p:bool . p) @
-}
extractBasicDefinition :: Typeable thry => 
                          HOLContext thry -> String -> String -> Q [Dec]
extractBasicDefinition ctx lbl name =
    do defns <- runIO $ evalHOLCtxt definitions ctx
       let mb = find (\ x -> case destEq $ concl x of
                               Just (view -> Const l _ _, _) -> l == name
                               _ -> False) defns
       case mb of
         Nothing -> fail "extractBasicDefinition: definition not found"
         Just th -> liftProtected lbl $ protect ctx th

{-|
  Extracts an axiom from a provided context, protecting and lifting it with
  'liftProtected'.  The extraction is performed by looking for an axioms of
  a given name, as specified when the axiom was created with 'newAxiom'.
-}
extractAxiom :: Typeable thry => HOLContext thry -> String -> Q [Dec]
extractAxiom ctx lbl =
    do ax <- runIO $ evalHOLCtxt 
                       (getAxiom lbl <?> "extractAxiom: axiom not found") ctx
       liftProtected lbl $ protect ctx ax
