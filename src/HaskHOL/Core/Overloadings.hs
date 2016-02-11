{-# LANGUAGE FlexibleContexts #-}
module HaskHOL.Core.Overloadings where

import qualified HaskHOL.Core.Kernel as K
import qualified HaskHOL.Core.State as S
import qualified HaskHOL.Core.Basics as B
import qualified HaskHOL.Core.Parser as P

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel (HOLTerm, HOLType, HOLThm, SubstTrip, Inst)
import HaskHOL.Core.State (HOL, Theory)
import HaskHOL.Core.Parser 
    (HOLTermRep, HOLTypeRep, HOLThmRep, toHTm, toHTy, toHThm)

-- Kernel Type Functions
mkUType :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
        => ty1 -> ty2 -> HOL cls thry HOLType
mkUType pty1 pty2 =
    do ty1 <- toHTy pty1
       ty2 <- toHTy pty2
       K.mkUType ty1 ty2

mkUTypes :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
         => [ty1] -> ty2 -> HOL cls thry HOLType
mkUTypes ptys pty =
    do tys <- mapM toHTy ptys
       ty <- toHTy pty
       K.mkUTypes tys ty

typeMatch :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
          => ty1 -> ty2 -> HOL cls thry SubstTrip
typeMatch pty1 pty2 =
    do ty1 <- toHTy pty1
       ty2 <- toHTy pty2
       K.typeMatch ty1 ty2 ([],[],[])

-- Kernel Term Functions
mkVar :: HOLTypeRep ty cls thry => Text -> ty -> HOL cls thry HOLTerm
mkVar x pty = 
    do ty <- toHTy pty
       return $! K.mkVar x ty

mkAbs :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
      => tm1 -> tm2 -> HOL cls thry HOLTerm
mkAbs ptm1 ptm2 =
    do tm1 <- toHTm ptm1
       tm2 <- toHTm ptm2
       K.mkAbs tm1 tm2

mkComb :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
       => tm1 -> tm2 -> HOL cls thry HOLTerm
mkComb ptm1 ptm2 =
    do tm1 <- toHTm ptm1
       tm2 <- toHTm ptm2
       K.mkComb tm1 tm2

mkTyAbs :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
        => ty -> tm -> HOL cls thry HOLTerm
mkTyAbs pty ptm =
    do ty <- toHTy pty
       tm <- toHTm ptm
       K.mkTyAbs ty tm

mkTyComb :: (HOLTermRep tm cls thry, HOLTypeRep ty cls thry)
         => tm -> ty -> HOL cls thry HOLTerm
mkTyComb ptm pty =
    do tm <- toHTm ptm
       ty <- toHTy pty
       K.mkTyComb tm ty

-- Kernel Theorem Functions

{-| 
  A redefinition of 'K.primREFL' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primREFL :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primREFL pthm = K.primREFL `fmap` toHTm pthm

{-| 
  A redefinition of 'K.primTRANS' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primTRANS :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
          => thm1 -> thm2 -> HOL cls thry HOLThm
primTRANS pthm1 pthm2 =
    do thm1 <- toHThm pthm1
       thm2 <- toHThm pthm2
       K.primTRANS thm1 thm2

{-| 
  A redefinition of 'K.primMK_COMB' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primMK_COMB :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry)
            => thm1 -> thm2 -> HOL cls thry HOLThm
primMK_COMB pthm1 pthm2 =
    do thm1 <- toHThm pthm1
       thm2 <- toHThm pthm2
       K.primMK_COMB thm1 thm2

{-| 
  A redefinition of 'K.primABS' to overload it for all valid term and theorem
  representations as defined by 'HOLTermRep' and 'HOLThmRep'.
-}
primABS :: (HOLTermRep tm cls thry, HOLThmRep thm cls thry) 
        => tm -> thm -> HOL cls thry HOLThm  
primABS ptm pthm =
    do tm <- toHTm ptm
       thm <- toHThm pthm
       K.primABS tm thm

{-| 
  A redefinition of 'K.primBETA' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primBETA :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primBETA = K.primBETA <=< toHTm

{-| 
  A redefinition of 'K.primASSUME' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primASSUME :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primASSUME = K.primASSUME <=< toHTm

{-| 
  A redefinition of 'K.primEQ_MP' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primEQ_MP :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
          => thm1 -> thm2 -> HOL cls thry HOLThm
primEQ_MP pthm1 pthm2 =
    do thm1 <- toHThm pthm1
       thm2 <- toHThm pthm2
       K.primEQ_MP thm1 thm2

{-| 
  A redefinition of 'K.primDEDUCT_ANTISYM' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primDEDUCT_ANTISYM :: (HOLThmRep thm1 cls thry, HOLThmRep thm2 cls thry) 
                   => thm1 -> thm2 -> HOL cls thry HOLThm
primDEDUCT_ANTISYM pthm1 pthm2 =
    pure K.primDEDUCT_ANTISYM <*> toHThm pthm1 <*> toHThm pthm2

{-| 
  A redefinition of 'K.primINST_TYPE' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primINST_TYPE :: (HOLThmRep thm cls thry, Inst a b) 
              => [(a, b)] -> thm -> HOL cls thry HOLThm
primINST_TYPE tyenv pthm = K.primINST_TYPE tyenv `fmap` toHThm pthm

{-| 
  A redefinition of 'K.primINST_TYPE_FULL' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primINST_TYPE_FULL :: HOLThmRep thm cls thry 
                   => SubstTrip -> thm -> HOL cls thry HOLThm
primINST_TYPE_FULL tyenv pthm = K.primINST_TYPE_FULL tyenv `fmap` toHThm pthm

{-| 
  A redefinition of 'K.primINST' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primINST :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
             HOLThmRep thm cls thry)
         => [(tm1, tm2)] -> thm -> HOL cls thry HOLThm
primINST ptmenv pthm = 
    do tmenv <- mapM (toHTm `ffCombM` toHTm) ptmenv
       K.primINST tmenv =<< toHThm pthm

{-| 
  A redefinition of 'K.primTYABS' to overload it for all valid theorem
  representations as defined by 'HOLThmRep'.
-}
primTYABS :: (HOLTypeRep ty cls thry, HOLThmRep thm cls thry) 
          => ty -> thm -> HOL cls thry HOLThm
primTYABS pty pthm =
    do ty <- toHTy pty
       thm <- toHThm pthm
       K.primTYABS ty thm

{-| 
  A redefinition of 'K.primTYAPP2' to overload it for all valid type and theorem
  representations as defined by 'HOLTypeRep' and 'HOLThmRep'.
-}
primTYAPP2 :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry, 
               HOLThmRep thm cls thry) 
           => ty1 -> ty2 -> thm -> HOL cls thry HOLThm
primTYAPP2 pty1 pty2 pthm =
    do ty1 <- toHTy pty1
       ty2 <- toHTy pty2
       thm <- toHThm pthm
       K.primTYAPP2 ty1 ty2 thm

{-| 
  A redefinition of 'K.primTYAPP' to overload it for all valid type and theorem
  representations as defined by 'HOLTypeRep' and 'HOLThmRep'.
-}
primTYAPP :: (HOLTypeRep ty cls thry, HOLThmRep thm cls thry) 
          => ty -> thm -> HOL cls thry HOLThm
primTYAPP pty pthm =
    do ty <- toHTy pty
       thm <- toHThm pthm
       K.primTYAPP ty thm

{-| 
  A redefinition of 'K.primTYBETA' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
primTYBETA :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLThm
primTYBETA = K.primTYBETA <=< toHTm


-- State Functions
mkType :: HOLTypeRep ty cls thry => Text -> [ty] -> HOL cls thry HOLType
mkType name = S.mkType name <=< mapM toHTy

mkFunTy :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
        => ty1 -> ty2 -> HOL cls thry HOLType
mkFunTy pty1 pty2 =
    do ty1 <- toHTy pty1
       ty2 <- toHTy pty2
       S.mkFunTy ty1 ty2

{-| 
  A redefinition of 'S.newConstant' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newConstant :: HOLTypeRep ty Theory thry => (Text, ty) -> HOL Theory thry ()
newConstant (name, pty) = S.newConstant name =<< toHTy pty

{-| 
  A redefinition of 'S.newAxiom' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newAxiom :: HOLTermRep tm Theory thry => (Text, tm) -> HOL Theory thry HOLThm
newAxiom (name, ptm) = S.newAxiom name =<< toHTm ptm

{-| 
  A redefinition of 'S.newBasicDefinition' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
newBasicDefinition :: HOLTermRep tm Theory thry 
                   => (Text, tm) -> HOL Theory thry HOLThm
newBasicDefinition (lbl, ptm) = S.newBasicDefinition lbl =<< toHTm ptm

-- Basic Functions
tysubst :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry, 
            HOLTypeRep ty3 cls thry) 
        => [(ty1, ty2)] -> ty3 -> HOL cls thry HOLType
tysubst penv pty =
    do env <- mapM (toHTy `ffCombM` toHTy) penv
       ty <- toHTy pty
       B.tysubst env ty

mkEq :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
     => tm1 -> tm2 -> HOL cls thry HOLTerm
mkEq ptm1 ptm2 =
    do tm1 <- toHTm ptm1
       tm2 <- toHTm ptm2
       B.mkEq tm1 tm2

subst :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
          HOLTermRep tm3 cls thry) 
      => [(tm1, tm2)] -> tm3 -> HOL cls thry HOLTerm
subst ptmenv ptm = 
    do tmenv <- mapM (toHTm `ffCombM` toHTm) ptmenv
       tm <- toHTm ptm
       B.subst tmenv tm

listMkComb :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
           => tm1 -> [tm2] -> HOL cls thry HOLTerm
listMkComb ptm ptms =
    do tm <- toHTm ptm
       tms <- mapM toHTm ptms
       B.listMkComb tm tms

listMkTyComb :: (HOLTermRep tm cls thry, HOLTypeRep ty cls thry)
             => tm -> [ty] -> HOL cls thry HOLTerm
listMkTyComb ptm ptys =
    do tm <- toHTm ptm
       tys <- mapM toHTy ptys
       B.listMkTyComb tm tys

listMkAbs :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
          => [tm1] -> tm2 -> HOL cls thry HOLTerm
listMkAbs ptms ptm =
    do tms <- mapM toHTm ptms
       tm <- toHTm ptm
       B.listMkAbs tms tm

listMkTyAbs :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
            => [ty] -> tm -> HOL cls thry HOLTerm
listMkTyAbs ptys ptm = 
    do tys <- mapM toHTy ptys
       tm <- toHTm ptm
       B.listMkTyAbs tys tm

rator :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
rator = B.rator <=< toHTm

rand :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
rand = B.rand <=< toHTm

bndvar :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
bndvar = B.bndvar <=< toHTm

body :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
body = B.body <=< toHTm

bndvarTyabs :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLType
bndvarTyabs = B.bndvarTyabs <=< toHTm

bodyTyabs :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
bodyTyabs = B.bodyTyabs <=< toHTm

mkBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
            HOLTermRep tm3 cls thry) 
        => tm1 -> tm2 -> tm3 -> HOL cls thry HOLTerm
mkBinop ptm1 ptm2 ptm3 =
    do tm1 <- toHTm ptm1
       tm2 <- toHTm ptm2
       tm3 <- toHTm ptm3
       B.mkBinop tm1 tm2 tm3

listMkBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
            => tm1 -> [tm2] -> HOL cls thry HOLTerm
listMkBinop ptm ptms =
    do tm <- toHTm ptm
       tms <- mapM toHTm ptms
       B.listMkBinop tm tms

mkBinder :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
         => Text -> tm1 -> tm2 -> HOL cls thry HOLTerm
mkBinder op ptm1 ptm2 =
    do tm1 <- toHTm ptm1
       tm2 <- toHTm ptm2
       B.mkBinder op tm1 tm2

mkTyBinder :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
           => Text -> ty -> tm -> HOL cls thry HOLTerm
mkTyBinder op pty ptm =
    do ty <- toHTy pty
       tm <- toHTm ptm
       B.mkTyBinder op ty tm

-- Parser Functions
{-|
  A redefinition of 'P.makeOverloadable' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
makeOverloadable :: HOLTypeRep ty Theory thry 
                 => Text -> ty -> HOL Theory thry ()
makeOverloadable s = P.makeOverloadable s <=< toHTy

{-|
  A redefinition of 'P.reduceInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
reduceInterface :: HOLTermRep tm Theory thry
                => Text -> tm -> HOL Theory thry ()
reduceInterface s = P.reduceInterface s <=< toHTm

{-|
  A redefinition of 'P.overrideInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overrideInterface :: HOLTermRep tm Theory thry 
                  => Text -> tm -> HOL Theory thry ()
overrideInterface s = P.overrideInterface s <=< toHTm

{-|
  A redefinition of 'P.overloadInterface' to overload it for all valid term
  representations as defined by 'HOLTermRep'.
-}
overloadInterface :: HOLTermRep tm Theory thry 
                  => Text -> tm -> HOL Theory thry ()
overloadInterface s = P.overloadInterface s <=< toHTm

{-|
  A redefinition of 'P.prioritizeOverload' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
prioritizeOverload :: HOLTypeRep ty Theory thry => ty -> HOL Theory thry ()
prioritizeOverload = P.prioritizeOverload <=< toHTy

{-|
  A redefinition of 'P.newTypeAbbrev' to overload it for all valid type
  representations as defined by 'HOLTypeRep'.
-}
newTypeAbbrev :: HOLTypeRep ty Theory thry => Text -> ty -> HOL Theory thry ()
newTypeAbbrev s = P.newTypeAbbrev s <=< toHTy
