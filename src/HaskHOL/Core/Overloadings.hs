{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module HaskHOL.Core.Overloadings where

import qualified HaskHOL.Core.Kernel as K
import qualified HaskHOL.Core.Basics as B
import qualified HaskHOL.Core.Parser as P

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel (HOLTermEnv, HOLTerm, HOLType, TypeOp, HOLThm, SubstTrip, Inst)
import HaskHOL.Core.State.Monad (HOL, Theory)
import HaskHOL.Core.Parser 
    (HOLTermRep, HOLTypeRep, HOLThmRep, toHTm, toHTy, toHThm)

-- Overloading Skeletons
{-
class Overload a where
  type c x :: Constraint
  overload :: c x => x -> HOL cls thry a

instance Overload HOLTerm where
  type c ty = HOLTypeRep ty cls thry
  overload = toHTy
-}

overloadTy1 :: (MonadThrow m, HOLTypeRep ty cls thry) 
            => (HOLType -> m a) -> ty -> HOL cls thry a
overloadTy1 f = f <=< toHTy

overloadTy2 :: (MonadThrow m, HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
            => (HOLType -> HOLType -> m HOLType) -> ty1 -> ty2 
            -> HOL cls thry HOLType
overloadTy2 f = overloadTy1 (overloadTy1 f)

overloadTm1 :: (MonadThrow m, HOLTermRep tm cls thry) 
            => (HOLTerm -> m a) -> tm -> HOL cls thry a
overloadTm1 f = f <=< toHTm

overloadTms :: (MonadThrow m, HOLTermRep tm cls thry)
            => ([HOLTerm] -> m a) -> [tm] -> HOL cls thry a
overloadTms f = f <=< mapM toHTm

overloadTmEnv2 :: (MonadThrow m, HOLTermRep tm1 cls thry, 
                   HOLTermRep tm2 cls thry, HOLTermRep tm3 cls thry)
               => (HOLTermEnv -> HOLTerm -> m HOLTerm) 
               -> [(tm1, tm2)] -> tm3 -> HOL cls thry HOLTerm
overloadTmEnv2 f penv ptm =
  f <$> mapM (toHTm `ffCombM` toHTm) penv <*> toHTm ptm

-- Kernel Type Functions
destFunTy :: HOLTypeRep ty cls thry => ty -> HOL cls thry (HOLType, HOLType)
destFunTy = destFunTy `fmap` toHTy

tyApp :: HOLTypeRep ty cls thry => TypeOp -> [ty] -> HOL cls thry HOLType
tyApp op = K.tyApp op <=< mapM toHTy

mkUType :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
        => ty1 -> ty2 -> HOL cls thry HOLType
mkUType = overloadTy2 K.mkUType

mkUTypes :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
         => [ty1] -> ty2 -> HOL cls thry HOLType
mkUTypes ptys pty =
    do tys <- mapM toHTy ptys
       ty <- toHTy pty
       K.mkUTypes tys ty

typeMatch :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry,
              HOLTypeRep ty3 cls thry, HOLTypeRep ty4 cls thry,
              HOLTypeRep ty5 cls thry)
          => ty1 -> ty2 
          -> ([(ty3, ty4)], [(K.TypeOp, ty5)], [(K.TypeOp, K.TypeOp)]) 
          -> HOL cls thry SubstTrip
typeMatch pty1 pty2 (ptys, ptyops, pops) =
    do ty1 <- toHTy pty1
       ty2 <- toHTy pty2
       tys <- mapM (toHTy `ffCombM` toHTy) ptys
       tyops <- mapM (return `ffCombM` toHTy) ptyops
       K.typeMatch ty1 ty2 (tys, tyops, pops)

typeMatch_NIL :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
              => ty1 -> ty2 -> HOL cls thry SubstTrip
typeMatch_NIL pty1 pty2 =
    do ty1 <- toHTy pty1
       ty2 <- toHTy pty2
       K.typeMatch ty1 ty2 ([], [], [])

-- Kernel Term Functions
typeOf :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLType
typeOf ptm = K.typeOf `fmap` toHTm ptm

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

destVar :: HOLTermRep tm cls thry => tm -> HOL cls thry (Text, HOLType)
destVar = K.destVar <=< toHTm

destAbs :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destAbs = K.destAbs <=< toHTm

destComb :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destComb = K.destComb <=< toHTm

destTyAbs :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLType, HOLTerm)
destTyAbs = K.destTyAbs <=< toHTm

destTyComb :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLType)
destTyComb = K.destTyComb <=< toHTm

destEq :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destEq = K.destEq <=< toHTm

varSubst :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
             HOLTermRep tm3 cls thry)
         => [(tm1, tm2)] -> tm3 -> HOL cls thry HOLTerm
varSubst penv ptm =
    do env <- mapM (toHTm `ffCombM` toHTm) penv
       tm <- toHTm ptm
       K.varSubst env tm

class InstHOL a b cls thry where
  instHOL :: [(a, b)] -> HOLTerm -> HOL cls thry HOLTerm

instance (HOLTypeRep l cls thry, HOLTypeRep r cls thry) => 
         InstHOL l r cls thry where
    instHOL penv tm =
        do env <- mapM (toHTy `ffCombM` toHTy) penv
           return $! K.inst env tm

instance HOLTypeRep r cls thry => InstHOL TypeOp r cls thry where
    instHOL penv tm = 
        do env <- mapM (return `ffCombM` toHTy) penv
           return $! K.inst env tm

instance InstHOL TypeOp TypeOp cls thry where
    instHOL penv tm = return $! K.inst penv tm


inst :: (InstHOL a b cls thry, HOLTermRep tm cls thry) 
     => [(a, b)] -> tm -> HOL cls thry HOLTerm
inst penv = instHOL penv <=< toHTm

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

-- Core "Basic" Functions
tysubst :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry, 
            HOLTypeRep ty3 cls thry) 
        => [(ty1, ty2)] -> ty3 -> HOL cls thry HOLType
tysubst = overloadTy1 (overloadTyEnv B.tysubst)

alphaUtype :: (HOLTypeRep ty1 cls thry, HOLTypeRep ty2 cls thry)
           => ty1 -> ty2 -> HOL cls thry HOLType
alphaUtype = overloadTy2 B.alphaUtype

mkEq :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
     => tm1 -> tm2 -> HOL cls thry HOLTerm
mkEq = overloadTm2 B.mkEq

subst :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
          HOLTermRep tm3 cls thry) 
      => [(tm1, tm2)] -> tm3 -> HOL cls thry HOLTerm
subst = overloadTmEnv2 B.subst

alpha :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
      => tm1 -> tm2 -> HOL cls thry HOLTerm
alpha = overloadTm2 B.alpha

alphaTyabs :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
           => ty -> tm -> HOL cls thry HOLTerm
alphaTyabs = overloadTm1 (overloadTy1 B.alphaTyabs)

listMkComb :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
           => tm1 -> [tm2] -> HOL cls thry HOLTerm
listMkComb = overloadTms (overloadTm1 B.listMkComb)

listMkTyComb :: (HOLTermRep tm cls thry, HOLTypeRep ty cls thry)
             => tm -> [ty] -> HOL cls thry HOLTerm
listMkTyComb = overloadTys (overloadTm1 B.listMkTyComb)

listMkAbs :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
          => [tm1] -> tm2 -> HOL cls thry HOLTerm
listMkAbs = overloadTm1 (overloadTms B.listMkAbs)

listMkTyAbs :: (HOLTypeRep ty cls thry, HOLTermRep tm cls thry)
            => [ty] -> tm -> HOL cls thry HOLTerm
listMkTyAbs = overloadTm1 (overloadTys B.listMkTyAbs)

rator :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
rator = overloadTm1 B.rator

rand :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
rand = overloadTm1 B.rand

bndvar :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
bndvar = overloadTm1 B.bndvar

body :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
body = overloadTm1 B.body

bndvarTyabs :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLType
bndvarTyabs = overloadTm1 B.bndvarTyabs

bodyTyabs :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
bodyTyabs = overloadTm1 B.bodyTyabs

mkIComb :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
        => tm1 -> tm2 -> HOL cls thry HOLTerm
mkIComb = overloadTm2 B.mkIComb

destBinary :: HOLTermRep tm cls thry 
           => Text -> tm -> HOL cls thry (HOLTerm, HOLTerm)
destBinary s = overloadTm1 (B.destBinary s)

destBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry) 
          => tm1 -> tm2 -> HOL cls thry (HOLTerm, HOLTerm)
destBinop = overloadTm2 B.destBinop

mkBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry, 
            HOLTermRep tm3 cls thry) 
        => tm1 -> tm2 -> tm3 -> HOL cls thry HOLTerm
mkBinop = overloadTm1 (overloadTm2 B.mkBinop)

listMkBinop :: (HOLTermRep tm1 cls thry, HOLTermRep tm2 cls thry)
            => tm1 -> [tm2] -> HOL cls thry HOLTerm
listMkBinop = overloadTms (overlodaTm1 B.listMkBinop)

destGAbs :: HOLTermRep tm cls thry 
           => tm -> HOL cls thry (HOLTerm, HOLTerm)
destGAbs = overloadTm1 B.destGAbs

destBinder :: HOLTermRep tm cls thry 
           => Text -> tm -> HOL cls thry (HOLTerm, HOLTerm)
destBinder op = overloadTm1 (B.destBinder op)

destTyBinder :: HOLTermRep tm cls thry 
             => Text -> tm -> HOL cls thry (HOLType, HOLTerm)
destTyBinder op = overloadTm1 (B.destTyBinder op)

destIff :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destIff = overloadTm1 B.destIff

destConj :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destConj = overloadTm1 B.destConj

destImp :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destImp = overloadTm1 B.destImp

destForall :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destForall = overloadTm1 B.destForall

destExists :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destExists = overloadTm1 B.destExists

destNeg :: HOLTermRep tm cls thry => tm -> HOL cls thry HOLTerm
destNeg = overloadTm1 B.destNeg

destDisj :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destDisj = overloadTm1 B.destDisj

destUExists :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destUExists = overloadTm1 B.destUExists

destTyAll :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLType, HOLTerm)
destTyAll = overloadTm1 B.destTyAll

destTyEx :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLType, HOLTerm)
destTyEx = overloadTm1 B.destTyEx

destCons :: HOLTermRep tm cls thry => tm -> HOL cls thry (HOLTerm, HOLTerm)
destCons = overloadTm1 B.destCons

destList :: HOLTermRep tm cls thry => tm -> HOL cls thry [HOLTerm]
destList = overloadTm1 B.destList

destLet :: HOLTermRep tm cls thry 
        => tm -> HOL cls thry ([(HOLTerm, HOLTerm)], HOLTerm)
destLet = overloadTm1 B.destLet

destNumeral :: HOLTermRep tm cls thry => tm -> HOL cls thry Integer
destNumeral = overloadTm1 B.destNumeral

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
