{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MagicHash, OverlappingInstances,
             TemplateHaskell, TypeSynonymInstances #-}

{-|
  Module:    HaskHOL.Core.Lib.Lift
  Copyright: (c) Ian Lynagh 2006
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
  Stability:   unstable
  Portability: unknown

  This module is a re-export of the th-lift library originally written by Ian
  Lynagh and maintained by Mathieu Boespflug.  A very minor change was made by
  Evan Austin in order to facilitate derivation of lift instances for quantified
  type constructors.

  The decision to include this source as part of the HaskHOL system, rather than
  import the original library, was made to facilitate the above change and to
  sever HaskHOL's only dependence on a non-Haskell Platform library.
-}

{-
  The original copyright is included in its entirety below, as required by BSD3:

  Copyright (c) Ian Lynagh.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. The names of the author may not be used to endorse or promote
     products derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
-}

module HaskHOL.Core.Lib.Lift 
    ( deriveLift'    -- :: Info -> Q [Dec]
    , deriveLift     -- :: Name -> Q [Dec]
    , deriveLiftMany -- :: [Name] -> Q [Dec]
    , module TH {-|
        Re-exports 'Lift' for the purpose of writing type signatures external to
        this module.
      -}
    ) where

import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Control.Monad ((<=<))


-- | Derive Lift instances for the given datatype.
deriveLift :: Name -> Q [Dec]
deriveLift = deriveLift' <=< reify

-- | Derive Lift instances for many datatypes.
deriveLiftMany :: [Name] -> Q [Dec]
deriveLiftMany = deriveLiftMany' <=< mapM reify

-- | Obtain Info values through a custom reification function. This is useful
-- when generating instances for datatypes that have not yet been declared.
deriveLift' :: Info -> Q [Dec]
deriveLift' = fmap (:[]) . deriveLiftOne

deriveLiftMany' :: [Info] -> Q [Dec]
deriveLiftMany' = mapM deriveLiftOne

deriveLiftOne :: Info -> Q Dec
deriveLiftOne i =
  case i of
    TyConI (DataD dcx n vsk cons _) ->
      liftInstance dcx n (map unTyVarBndr vsk) (map doCons cons)
    TyConI (NewtypeD dcx n vsk con _) ->
      liftInstance dcx n (map unTyVarBndr vsk) [doCons con]
    _ -> fail ("deriveLift: unhandled: " ++ pprint i)
  where liftInstance dcx n vs cases =
          instanceD (ctxt dcx vs) (conT ''Lift `appT` typ n vs) [funD 'lift cases]
        typ n = foldl appT (conT n) . map varT
        ctxt dcx = fmap (dcx ++) . cxt . map liftPred
        unTyVarBndr (PlainTV v) = v
        unTyVarBndr (KindedTV v _) = v
        liftPred n = classP ''Lift [varT n]

doCons :: Con -> Q Clause
doCons (NormalC c sts) = do
  let ns = zipWith (\_ i -> 'x' : show i) sts [(0::Integer)..]
      con = [| conE c |]
      args = [ [| lift $(varE (mkName n)) |] | n <- ns ]
      e = foldl (\e1 e2 -> [| appE $e1 $e2 |]) con args
  clause [conP c (map (varP . mkName) ns)] (normalB e) []
doCons (RecC c sts) = doCons $ NormalC c [(s, t) | (_, s, t) <- sts]
doCons (InfixC sty1 c sty2) = do
  let con = [| conE c |]
      left = [| lift $(varE (mkName "x0")) |]
      right = [| lift $(varE (mkName "x1")) |]
      e = [| infixApp $left $con $right |]
  clause [infixP (varP (mkName "x0")) c (varP (mkName "x1"))] (normalB e) []
-- ECA
doCons (ForallC _ _ con) = doCons con

instance Lift Name where
    lift (Name occName nameFlavour) = [| Name occName nameFlavour |]

instance Lift OccName where
  lift n = [| mkOccName $(lift $ occString n) |]

instance Lift PkgName where
  lift n = [| mkPkgName $(lift $ pkgString n) |]

instance Lift ModName where
  lift n = [| mkModName $(lift $ modString n) |]

instance Lift NameFlavour where
    lift NameS = [| NameS |]
    lift (NameQ moduleName) = [| NameQ moduleName |]
    lift (NameU i) = [| case $( lift (I# i) ) of
                            I# i' -> NameU i' |]
    lift (NameL i) = [| case $( lift (I# i) ) of
                            I# i' -> NameL i' |]
    lift (NameG nameSpace pkgName moduleName)
     = [| NameG nameSpace pkgName moduleName |]

instance Lift NameSpace where
    lift VarName = [| VarName |]
    lift DataName = [| DataName |]
    lift TcClsName = [| TcClsName |]

-- These instances should really go in the template-haskell package.

instance Lift () where
  lift _ = [| () |]

instance Lift Rational where
  lift x = return (LitE (RationalL x))

--ECA
instance Lift String where
  lift = liftString
