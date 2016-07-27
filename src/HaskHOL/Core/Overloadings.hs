{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, 
             FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, 
             TypeFamilies #-}
module HaskHOL.Core.Overloadings
    ( module HaskHOL.Core.Kernel
    , module HaskHOL.Core.Basics
    , module HaskHOL.Core.Parser
    , module HaskHOL.Core.Overloadings
    )
 where

import HaskHOL.Core.Lib

import HaskHOL.Core.Kernel hiding
  (tyApp, destFunTy, primTYBETA, primTYAPP, primTYAPP2, primTYABS, 
   primINST, primINST_TYPE_FULL, primINST_TYPE, primDEDUCT_ANTISYM, 
   primEQ_MP, primASSUME, primBETA, primABS, primMK_COMB, primTRANS, 
   primREFL, varSubst, destEq, destTyComb, destTyAbs, destComb, destAbs, 
   destVar, mkTyComb, mkTyAbs, mkComb, mkAbs, mkVar, inst, typeMatch,
   mkUTypes, mkUType, typeOf)
import qualified HaskHOL.Core.Kernel as K

import HaskHOL.Core.Basics hiding
  (destNumeral, destLet, destList, destCons, destTyEx, destTyAll, destUExists,
   destNeg, destDisj, destExists, destForall, destImp, destConj, destIff,
   destTyBinder, destBinder, destGAbs, listMkBinop, mkBinop, destBinop,
   destBinary, mkIComb, bodyTyabs, bndvarTyabs, body, bndvar, rand, rator,
   listMkTyAbs, listMkAbs, listMkTyComb, listMkComb, alphaTyabs, alpha,
   subst, mkEq, alphaUtype, tysubst)
import qualified HaskHOL.Core.Basics as B

import HaskHOL.Core.Parser hiding
  (newTypeAbbrev, prioritizeOverload, overloadInterface, overrideInterface,
   reduceInterface, makeOverloadable)
import qualified HaskHOL.Core.Parser as P

import HaskHOL.Core.State.Monad (HOL, Theory, Constraint)


-- Overloading Skeletons
class Overload a b where
  type family OverloadTy a b cls thry :: Constraint
  overload :: OverloadTy a b cls thry => b -> HOL cls thry a

instance Overload HOLType ty where
  type OverloadTy HOLType ty cls thry = HOLTypeRep ty cls thry
  overload = toHTy

instance Overload HOLTerm tm where
  type OverloadTy HOLTerm tm cls thry = HOLTermRep tm cls thry
  overload = toHTm

instance Overload HOLThm thm where
  type OverloadTy HOLThm thm cls thry = HOLThmRep thm cls thry
  overload = toHThm

instance (Overload a1 b1, Overload a2 b2) => Overload (a1, a2) (b1, b2) where
  type OverloadTy (a1, a2) (b1, b2) cls thry =
      (OverloadTy a1 b1 cls thry, OverloadTy a2 b2 cls thry)
  overload = overload `ffCombM` overload

instance (Overload a1 b1, Overload a2 b2, Overload a3 b3) => 
         Overload (a1, a2, a3) (b1, b2, b3) where
  type OverloadTy (a1, a2, a3) (b1, b2, b3) cls thry =
      (OverloadTy a1 b1 cls thry, OverloadTy a2 b2 cls thry, 
       OverloadTy a3 b3 cls thry)
  overload (x, y, z) = 
    do {x' <- overload x; y' <- overload y; z' <- overload z; return (x',y',z')}

-- Has the potential for a space leak for large argument lists due to mapM.
instance Overload a b => Overload [a] [b] where
  type OverloadTy [a] [b] cls thry = OverloadTy a b cls thry
  overload = mapM overload

-- One off to clean up overloadings related to type substitution
instance Overload K.TypeOp K.TypeOp where
  type OverloadTy K.TypeOp K.TypeOp cls thry = ()
  overload = return

overload1 :: (Overload a b, OverloadTy a b cls thry)
          => (a -> HOL cls thry c) -> b -> HOL cls thry c
overload1 f x = join (f <$!> overload x)

overload2 :: (Overload a1 b1, OverloadTy a1 b1 cls thry,
              Overload a2 b2, OverloadTy a2 b2 cls thry)
          => (a1 -> a2 -> HOL cls thry c) -> b1 -> b2 -> HOL cls thry c
overload2 f x y = join (f <$!> overload x <*> overload y)

overload3 :: (Overload a1 b1, OverloadTy a1 b1 cls thry,
              Overload a2 b2, OverloadTy a2 b2 cls thry,
              Overload a3 b3, OverloadTy a3 b3 cls thry)
          => (a1 -> a2 -> a3 -> HOL cls thry c) 
          -> b1 -> b2 -> b3 -> HOL cls thry c
overload3 f x y z = join (f <$!> overload x <*> overload y <*> overload z)

-- Kernel Type Functions
destFunTy :: HOLTypeRep ty cls thry => ty -> HOL cls thry (HOLType, HOLType)
destFunTy = overload1 K.destFunTy

tyApp :: HOLTypeRep ty cls thry => TypeOp -> [ty] -> HOL cls thry HOLType
tyApp = overload2 K.tyApp

mkUType :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
        => ty1 -> ty2 -> HOL cls thry HOLType
mkUType = overload2 K.mkUType

mkUTypes :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
         => [ty1] -> ty2 -> HOL cls thry HOLType
mkUTypes = overload2 K.mkUTypes

typeMatch :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry,
              HOLTypeRep ty3 cls thry, HOLTypeRep ty4 cls thry,
              HOLTypeRep ty5 cls thry)
          => ty1 -> ty2
          -> ([(ty3, ty4)], [(K.TypeOp, ty5)], [(K.TypeOp, K.TypeOp)]) 
          -> HOL cls thry SubstTrip
typeMatch = overload3 K.typeMatch

{-# INLINEABLE typeMatch_NIL #-}
typeMatch_NIL :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
              => ty1 -> ty2 -> HOL cls thry SubstTrip
typeMatch_NIL x y = 
  HaskHOL.Core.Overloadings.typeMatch x y (([], [], [])::SubstTrip)

-- Kernel Term Functions
typeOf :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLType
typeOf = overload1 (return . K.typeOf)

mkVar :: HOLTypeRep ty cls thry => Text -> ty -> HOL cls thry HOLTerm
mkVar x = overload1 (return . K.mkVar x)

mkAbs :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
      => tm1 -> tm2 -> HOL cls thry HOLTerm
mkAbs = overload2 K.mkAbs

mkComb :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
       => tm1 -> tm2 -> HOL cls thry HOLTerm
mkComb = overload2 K.mkComb

mkTyAbs :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
        => ty -> tm -> HOL cls thry HOLTerm
mkTyAbs = overload2 K.mkTyAbs

mkTyComb :: (HOLTermRep tm cls thry, HOLTypeRep ty cls thry)
         => tm -> ty -> HOL cls thry HOLTerm
mkTyComb = overload2 K.mkTyComb

destVar :: HOLTermRep tm cls thry => tm -> HOL cls thry (Text, HOLType)
destVar = overload1 K.destVar

destAbs :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destAbs = overload1 K.destAbs

destComb :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destComb = overload1 K.destComb

destTyAbs :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLType, HOLTerm)
destTyAbs = overload1 K.destTyAbs

destTyComb :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLType)
destTyComb = overload1 K.destTyComb

destEq :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destEq = overload1 K.destEq

varSubst :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
             HOLTermRep tm3 cls thry)
         => [(tm1, tm2)] -> tm3 -> HOL cls thry HOLTerm
varSubst = overload2 K.varSubst


-- Is there a cleaner way to do this?
type InstHOL a b ty1 ty2 cls thry =
  (Inst ty1 ty2, Overload ty1 a, Overload ty2 b, 
   OverloadTy ty1 a cls thry, OverloadTy ty2 b cls thry)

inst :: forall a b ty1 ty2 tm cls thry. 
        (InstHOL a b ty1 ty2 cls thry, HOLTermRep tm cls thry) 
     => [(a, b)] -> tm -> HOL cls thry HOLTerm
inst = 
  let fun :: [(ty1, ty2)] -> HOLTerm -> HOL cls thry HOLTerm
      fun x = return . K.inst x in
    overload2 fun

-- Kernel Theorem Functions

{-| 
  A redefinition of 'K.primREFL' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primREFL :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primREFL = overload1 (return . K.primREFL)

{-| 
  A redefinition of 'K.primTRANS' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primTRANS :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
          => thm1 -> thm2 -> HOL cls thry HOLThm
primTRANS = overload2 K.primTRANS

{-| 
  A redefinition of 'K.primMK_COMB' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primMK_COMB :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry)
            => thm1 -> thm2 -> HOL cls thry HOLThm
primMK_COMB = overload2 K.primMK_COMB

{-| 
  A redefinition of 'K.primABS' to overload it for all valid term and theorem
  representations as defined by 'HOLTermRep' and 'HOLThmRep'.
-}
primABS :: (HOLTermRep tm cls thry, HOLThmRep thm cls thry) 
        => tm -> thm -> HOL cls thry HOLThm  
primABS = overload2 K.primABS

{-| 
  A redefinition of 'K.primBETA' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primBETA :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primBETA = overload1 K.primBETA

{-| 
  A redefinition of 'K.primASSUME' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primASSUME :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primASSUME = overload1 K.primASSUME

{-| 
  A redefinition of 'K.primEQ_MP' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primEQ_MP :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
          => thm1 -> thm2 -> HOL cls thry HOLThm
primEQ_MP = overload2 K.primEQ_MP

{-| 
  A redefinition of 'K.primDEDUCT_ANTISYM' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primDEDUCT_ANTISYM :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
                   => thm1 -> thm2 -> HOL cls thry HOLThm
primDEDUCT_ANTISYM = overload2 (\ x -> return . K.primDEDUCT_ANTISYM x)

{-| 
  A redefinition of 'K.primINST_TYPE' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primINST_TYPE :: forall a b ty1 ty2 thm cls thry.
                 (InstHOL a b ty1 ty2 cls thry, HOLThmRep thm cls thry) 
              => [(a, b)] -> thm -> HOL cls thry HOLThm
primINST_TYPE = 
  let fun :: [(ty1, ty2)] -> HOLThm -> HOL cls thry HOLThm
      fun x = return . K.primINST_TYPE x in
    overload2 fun

{-| 
  A redefinition of 'K.primINST_TYPE_FULL' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primINST_TYPE_FULL :: HOLThmRep thm cls thry 
                   => SubstTrip -> thm -> HOL cls thry HOLThm
primINST_TYPE_FULL tyenv = overload1 (return . K.primINST_TYPE_FULL tyenv)

{-| 
  A redefinition of 'K.primINST' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primINST :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
             HOLThmRep thm cls thry)
         => [(tm1, tm2)] -> thm -> HOL cls thry HOLThm
primINST = overload2 K.primINST

{-| 
  A redefinition of 'K.primTYABS' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primTYABS :: (HOLTypeRep ty cls thry, HOLThmRep thm cls thry) 
          => ty -> thm -> HOL cls thry HOLThm
primTYABS = overload2 K.primTYABS

{-| 
  A redefinition of 'K.primTYAPP2' to overload it for all valid type and theorem
  representations as defined by 'HOLTypeRep' and 'HOLThmRep'.
-}
primTYAPP2 :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry, 
               HOLThmRep thm cls thry) 
           => ty1 -> ty2 -> thm -> HOL cls thry HOLThm
primTYAPP2 = overload3 K.primTYAPP2

{-| 
  A redefinition of 'K.primTYAPP' to overload it for all valid type and theorem
  representations as defined by 'HOLTypeRep' and 'HOLThmRep'.
-}
primTYAPP :: (HOLTypeRep ty cls thry, HOLThmRep thm cls thry) 
          => ty -> thm -> HOL cls thry HOLThm
primTYAPP = overload2 K.primTYAPP

{-| 
  A redefinition of 'K.primTYBETA' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primTYBETA :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primTYBETA = overload1 K.primTYBETA

-- Core "Basic" Functions
tysubst :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry, 
            HOLTypeRep ty3 cls thry) 
        => [(ty1, ty2)] -> ty3 -> HOL cls thry HOLType
tysubst = overload2 B.tysubst

alphaUtype :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
           => ty1 -> ty2 -> HOL cls thry HOLType
alphaUtype = overload2 B.alphaUtype

mkEq :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
     => tm1 -> tm2 -> HOL cls thry HOLTerm
mkEq = overload2 B.mkEq

subst :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
          HOLTermRep tm3 cls thry) 
      => [(tm1, tm2)] -> tm3 -> HOL cls thry HOLTerm
subst = overload2 B.subst

alpha :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
      => tm1 -> tm2 -> HOL cls thry HOLTerm
alpha = overload2 B.alpha

alphaTyabs :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
           => ty -> tm -> HOL cls thry HOLTerm
alphaTyabs = overload2 B.alphaTyabs

listMkComb :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
           => tm1 -> [tm2] -> HOL cls thry HOLTerm
listMkComb = overload2 B.listMkComb

listMkTyComb :: (HOLTermRep tm cls thry, HOLTypeRep ty cls thry)
             => tm -> [ty] -> HOL cls thry HOLTerm
listMkTyComb = overload2 B.listMkTyComb

listMkAbs :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
          => [tm1] -> tm2 -> HOL cls thry HOLTerm
listMkAbs = overload2 B.listMkAbs

listMkTyAbs :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
            => [ty] -> tm -> HOL cls thry HOLTerm
listMkTyAbs = overload2 B.listMkTyAbs

rator :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
rator = overload1 B.rator

rand :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
rand = overload1 B.rand

bndvar :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
bndvar = overload1 B.bndvar

body :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
body = overload1 B.body

bndvarTyabs :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLType
bndvarTyabs = overload1 B.bndvarTyabs

bodyTyabs :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
bodyTyabs = overload1 B.bodyTyabs

mkIComb :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
        => tm1 -> tm2 -> HOL cls thry HOLTerm
mkIComb = overload2 B.mkIComb

destBinary :: HOLTermRep tm cls thry 
           => Text -> tm -> HOL cls thry (HOLTerm, HOLTerm)
destBinary s = overload1 (B.destBinary s)

destBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
          => tm1 -> tm2 -> HOL cls thry (HOLTerm, HOLTerm)
destBinop = overload2 B.destBinop

mkBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
            HOLTermRep tm3 cls thry) 
        => tm1 -> tm2 -> tm3 -> HOL cls thry HOLTerm
mkBinop = overload3 B.mkBinop

listMkBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
            => tm1 -> [tm2] -> HOL cls thry HOLTerm
listMkBinop = overload2 B.listMkBinop

destGAbs :: HOLTermRep tm cls thry 
           => tm -> HOL cls thry (HOLTerm, HOLTerm)
destGAbs = overload1 B.destGAbs

destBinder :: HOLTermRep tm cls thry 
           => Text -> tm -> HOL cls thry (HOLTerm, HOLTerm)
destBinder op = overload1 (B.destBinder op)

destTyBinder :: HOLTermRep tm cls thry 
             => Text -> tm -> HOL cls thry (HOLType, HOLTerm)
destTyBinder op = overload1 (B.destTyBinder op)

destIff :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destIff = overload1 B.destIff

destConj :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destConj = overload1 B.destConj

destImp :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destImp = overload1 B.destImp

destForall :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destForall = overload1 B.destForall

destExists :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destExists = overload1 B.destExists

destNeg :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
destNeg = overload1 B.destNeg

destDisj :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destDisj = overload1 B.destDisj

destUExists :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destUExists = overload1 B.destUExists

destTyAll :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLType, HOLTerm)
destTyAll = overload1 B.destTyAll

destTyEx :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLType, HOLTerm)
destTyEx = overload1 B.destTyEx

destCons :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destCons = overload1 B.destCons

destList :: HOLTermRep tm cls thry => tm -> HOL cls thry [HOLTerm]
destList = overload1 B.destList

destLet :: HOLTermRep tm cls thry 
        => tm -> HOL cls thry ([(HOLTerm, HOLTerm)], HOLTerm)
destLet = overload1 B.destLet

destNumeral :: HOLTermRep tm cls thry => tm -> HOL cls thry Integer
destNumeral = overload1 B.destNumeral

-- Parser Functions
{-|
  A redefinition of 'P.makeOverloadable' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
makeOverloadable :: HOLTypeRep ty Theory thry 
                 => Text -> ty -> HOL Theory thry ()
makeOverloadable s = overload1 (P.makeOverloadable s)

{-|
  A redefinition of 'P.reduceInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
reduceInterface :: HOLTermRep tm Theory thry
                => Text -> tm -> HOL Theory thry ()
reduceInterface s = overload1 (P.reduceInterface s)

{-|
  A redefinition of 'P.overrideInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overrideInterface :: HOLTermRep tm Theory thry 
                  => Text -> tm -> HOL Theory thry ()
overrideInterface s = overload1 (P.overrideInterface s)

{-|
  A redefinition of 'P.overloadInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overloadInterface :: HOLTermRep tm Theory thry 
                  => Text -> tm -> HOL Theory thry ()
overloadInterface s = overload1 (P.overloadInterface s)

{-|
  A redefinition of 'P.prioritizeOverload' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
prioritizeOverload :: HOLTypeRep ty Theory thry => ty -> HOL Theory thry ()
prioritizeOverload = overload1 P.prioritizeOverload

{-|
  A redefinition of 'P.newTypeAbbrev' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
newTypeAbbrev :: HOLTypeRep ty Theory thry => Text -> ty -> HOL Theory thry ()
newTypeAbbrev s = overload1 (P.newTypeAbbrev s)
