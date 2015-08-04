{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
{-|
  Module:    HaskHOL.Core.Parser.Elab
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines the elaborators for types and terms.  These elaborators
  convert 'PreType's and 'PreTerm's, as produced by the parsers, into 
  'HOLType's and 'HOLTerm's accordingly.  This conversion includes local type
  inference for terms.
-}
module HaskHOL.Core.Parser.Elab
    ( tyElab
    , elab
    , ElabError(..)
    ) where

import HaskHOL.Core.Lib hiding (ask)
import HaskHOL.Core.Basics hiding (mkMConst, mkGAbs, mkBinder)
import HaskHOL.Core.Kernel

import HaskHOL.Core.Parser.Lib hiding (gets)
import HaskHOL.Core.Parser.Prims

import Control.Lens hiding (op, cons, snoc)
import Control.Monad.ST
import Data.STRef
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- Static Configuration
{-|
  Flag to say whether to warn on invention of system type variables and system
  type operator variables.
-}
invWarning :: Bool
invWarning = True

{-|
  Flag to say whether to automatically turn a universal type into a 
  non-universal type by adding applications of type variables.
-}
addTyAppsAuto :: Bool
addTyAppsAuto = True

{-| 
  Flag to say whether to treat a constant varstruct, i.e.  @\\ const . bod@, as
  variable.
-}
ignoreConstVarstruct :: Bool
ignoreConstVarstruct = True

-- types
type PEnv = [(Integer, PreType)]

data ElabError 
    = MissingConstant Text
    | MissingOverload Text
    | FailedInst Text SubstTrip
    | BadApplication TypeOp [HOLType]
    | TyElabError String

instance Show ElabError where
    show MissingConstant{} = "MissingConstant"
    show MissingOverload{} = "MissingOverload"
    show FailedInst{} = "FailedInst"
    show BadApplication{} = "BadApplication"
    show (TyElabError str) = str

type ElabM s = ExceptT [ElabError] (ReaderT (STRef s ElabState) (ST s))

instance Note (ElabM s) where
    job <?> str = job `catchE` (\ es -> throwE (TyElabError str : es))

data ElabState = ElabState
    { _stvsTrans :: !Bool
    , _stovsTrans :: !Bool
    , _smallSTVS :: ![Integer]
    , _tyCounter :: !Integer
    , _warnings :: ![String]
    , _parseCtxt :: !ParseContext
    }

makeLenses ''ElabState

initElabState :: ParseContext -> ElabState
initElabState = ElabState False False [] 0  []

modElabState :: (ElabState -> ElabState) -> ElabM s ()
modElabState f = lift $ 
    do ref <- ask
       lift $ modifySTRef' ref f

viewElabState :: (ElabState -> a) -> ElabM s a
viewElabState f = lift $
    do ref <- ask
       lift . liftM f $ readSTRef ref

-- utility functions
addWarning :: Bool -> String -> ElabM s ()
addWarning True str = modElabState $ over warnings ((:) str)
addWarning False _ = return ()

getOverloads :: ElabM s (Map Text HOLType)
getOverloads = viewElabState $ view (parseCtxt . overloads)

getInterface :: ElabM s [(Text, (Text, HOLType))]
getInterface = viewElabState $ view (parseCtxt . interface)

getHidden :: ElabM s [Text]
getHidden = viewElabState $ view (parseCtxt . hidden)

getConstants :: ElabM s (Map Text HOLTerm)
getConstants = viewElabState $ view (parseCtxt . termConstants)

getTypeConstants :: ElabM s (Map Text TypeOp)
getTypeConstants = viewElabState $ view (parseCtxt . typeConstants)

getConstType :: Text -> ElabM s HOLType
getConstType name = 
    do consts <- getConstants
       case mapLookup name consts of
         Just res -> return $! typeOf res
         Nothing -> throwE [MissingConstant name]

mkConst :: Text -> SubstTrip -> ElabM s HOLTerm
mkConst name pat =
    do consts <- getConstants
       case mapLookup name consts of
         Just op -> case instConstFull op pat of
                      Just res -> return res
                      Nothing -> throwE [FailedInst name pat]
         Nothing -> throwE [MissingConstant name]

mkMConst :: Text -> HOLType -> ElabM s HOLTerm
mkMConst name ty =
    do uty <- getConstType name <?> "mkMConst: not a constant name."
       (mkConst name . fromJust $ typeMatch uty ty ([], [], [])) <?>
         "mkMConst: generic type cannot be instantiated."

mkBinder :: Text -> HOLTerm -> HOLTerm -> ElabM s HOLTerm
mkBinder op v tm = 
    (do c <- mkConst op ([(tyA, typeOf v)], [], [])
        fromRightM $ mkComb c =<< mkAbs v tm)
    <?> "mkBinder: " ++ show op

mkGAbs :: HOLTerm -> HOLTerm -> ElabM s HOLTerm
mkGAbs tm1@Var{} tm2 =
    mkAbs tm1 tm2 <#?> "mkGAbs: simple abstraction failed"
mkGAbs tm1 tm2 = 
    let fvs = frees tm1 in
      (do fTy <- mkType "fun" [typeOf tm1, typeOf tm2]
          let f = variant (frees tm1++frees tm2) $ mkVar "f" fTy
          bodIn <- flip (foldrM (mkBinder "!")) fvs =<< 
                     mkGEq (fromRight $ mkComb f tm1) tm2
          bndr <- mkConst "GABS" ([(tyA, fTy)], [], [])
          fromRightM $ mkComb bndr =<< mkAbs f bodIn)
      <?> "mkGAbs"
  where mkGEq :: HOLTerm -> HOLTerm -> ElabM s HOLTerm
        mkGEq t1 t2 = 
          do p <- mkConst "GEQ" ([(tyA, typeOf t1)], [], [])
             fromRightM $ mkBinop p t1 t2

mkType :: Text -> [HOLType] -> ElabM s HOLType
mkType name args =
    do consts <- getTypeConstants
       case mapLookup name consts of
         Just tyOp -> mkApp tyOp
         Nothing
             | null args -> 
                   throwE [TyElabError "type operator applied to zero args."]
             | otherwise ->
                   do name' <- if textHead name == '_' 
                               then return $! textTail name 
                               else do addWarning True $ "type " ++ show name ++
                                         " has not been defined. " ++
                                         "Defaulting to type operator variable."
                                       return name
                      mkApp $ mkTypeOpVar name'
  where mkApp :: TypeOp -> ElabM s HOLType
        mkApp tyOp =
            case tyApp tyOp args of
               Right res -> return res
               Left{} -> throwE [BadApplication tyOp args]

destSTV :: PreType -> Maybe Integer
destSTV (STyVar n) = Just n
destSTV _ = Nothing

destUTy :: PreType -> Maybe Text
destUTy (UTyVar _ x _) = Just x
destUTy _ = Nothing

destPUTy :: PreType -> Maybe (PreType, PreType)
destPUTy (PUTy tv ty) = Just (tv, ty)
destPUTy _ = Nothing

freeSTVS0 :: PreType -> [Integer]
freeSTVS0 PTyCon{} = []
freeSTVS0 UTyVar{} = []
freeSTVS0 (STyVar n) = [n]
freeSTVS0 (PTyComb _ args) =
    foldr (\ x y -> freeSTVS0 x `union` y) [] args
freeSTVS0 (PUTy _ tbody) = freeSTVS0 tbody

utyvars :: PreType -> [PreType]
utyvars PTyCon{} = []
utyvars tv@UTyVar{} = [tv]
utyvars STyVar{} = []
utyvars (PTyComb t args) = foldr (union . utyvars) [] $ t : args
utyvars (PUTy tv tbody) = utyvars tbody \\ [tv]

catUTyVars :: [PreType] -> [PreType]
catUTyVars = foldr (union . utyvars) []

variantUTyVar :: [PreType] -> PreType -> PreType
variantUTyVar avoid tv@(UTyVar f name n)
    | tv `elem` avoid = variantUTyVar avoid $ UTyVar f (name `snoc` '\'') n
    | otherwise = tv
variantUTyVar _ tv = tv

mkFunPTy :: PreType -> PreType -> PreType
mkFunPTy pty1 pty2 = PTyComb (PTyCon "fun") [pty1, pty2]

-- Construct a constant that has one or more instantiations provided
mkTIConst :: Text -> HOLType -> HOLTypeEnv -> ElabM s HOLTerm
mkTIConst c ty subs =
    (do cty' <- liftM (typeSubst subs) $ getConstType c
        let (mat1Tys, opTys, opOps) = fromJust $ typeMatch cty' ty ([], [], [])
            tys' = map (second $ typeSubst mat1Tys) subs ++ mat1Tys
            mat2 = (tys', opTys, opOps)
        con <- mkConst c mat2
        if typeOf con == ty
           then return con
           else mzero)
    <?> "mkTIConst"

-- Constructs a PreTerm representation of a provided integer.
pmkNumeral :: Integral i => i -> PreTerm
pmkNumeral = PComb numeral . pmkNumeralRec 
  where numPTy :: PreType
        numPTy = PTyComb (PTyCon "num") []
        
        numeral, bit0, bit1, t0 :: PreTerm
        numeral = PConst "NUMERAL" $ mkFunPTy numPTy numPTy
        bit0 = PConst "BIT0" $ mkFunPTy numPTy numPTy
        bit1 = PConst "BIT1" $ mkFunPTy numPTy numPTy
        t0 = PConst "_0" numPTy

        pmkNumeralRec :: Integral i => i -> PreTerm
        pmkNumeralRec 0 = t0
        pmkNumeralRec n = 
            let op = if n `mod` 2 == 0 then bit0 else bit1 in
              PComb op . pmkNumeralRec $ n `div` 2

{- 
  Checks if a unification for a system type variable is trivial, i.e. unifying
  with itself or a unification between two system type variables already stored
  in the environment.

  Also used to check for cyclic occurances, i.e. a system type variable exists
  as a sub-term of the provided term.
-}
istrivial :: PEnv -> Integer -> PreType -> Maybe Bool
istrivial _ _ PTyCon{} = Just False
istrivial env x (STyVar y)
    | y == x = Just True
    | otherwise = 
        (istrivial env x =<< lookup y env) <|> return False
istrivial _ _ UTyVar{} = Just False
istrivial env x (PTyComb f args) =
    do ys' <- mapM (istrivial env x) $ f : args
       if or ys'
          then Nothing
          else return False
istrivial env x (PUTy _ tbody) =
    do tbody' <- istrivial env x tbody
       if tbody'
          then Nothing
          else return False

-- system type generation
newTypeVar :: ElabM s PreType
newTypeVar = 
    do modElabState $ over tyCounter succ
       liftM STyVar . viewElabState $ view tyCounter

-- Turn UType term into a non-UType by adding type applications
addTyApps :: PreTerm -> PreType -> ElabM s PreTerm
addTyApps tm ty@(PUTy tv tb) =
    do ntv <- newTypeVar
       addTyApps (TyPComb tm ty ntv) $ pretypeSubst [(ntv, tv)] tb
addTyApps tm _ = return tm

-- Check where unification of ty with a UType is potentially possible
unifiableWithUType :: PreType -> PEnv -> Bool
unifiableWithUType ty env
    | ty == dpty = False
    | otherwise = 
          let tys = solve env ty in
            isJust (destPUTy tys) || isJust (destSTV tys)

-- Get a new instance of a constant's generic type modulo interface
getGenericType :: Text -> ElabM s HOLType
getGenericType cname =
    do iface <- getInterface
       case filter (\ (x, _) -> x == cname) iface of
         [(_, (_, ty))] -> return ty
         (_:_:_) -> do overs <- getOverloads
                       case mapLookup cname overs of
                         Just res -> return res
                         Nothing -> throwE [MissingOverload cname]
         _ -> getConstType cname

{- 
  Pretype substitution based on HOLType substitution.
  Note that the PTyCon case is comparatively simpler given the extra work done 
  in the parser.
-}
pretypeSubst :: [(PreType, PreType)] -> PreType -> PreType
pretypeSubst _ ty@PTyCon{} = ty
pretypeSubst tyenv ty@UTyVar{} = revAssocd ty tyenv ty
pretypeSubst _ ty@STyVar{} = ty
pretypeSubst tyenv ty@(PTyComb tyop args) =
    let tyop' = pretypeSubst tyenv tyop 
        args' = map (pretypeSubst tyenv) args in
      if tyop == tyop' && args == args' then ty
      else case tyop' of
             PUTy{} -> let (rtvs, rtbody) = splitList destPUTy tyop' in
                         pretypeSubst (zip args' rtvs) rtbody
             _ -> PTyComb tyop' args'
pretypeSubst tyenv ty@(PUTy tv tbody) =
    let tyenv' = filter (\ (_, x) -> x /= tv) tyenv in
      if null tyenv' then ty
      else if any (\ (t, x) -> tv `elem` utyvars t && 
                               x `elem` utyvars tbody) tyenv'
           then let tvbody = utyvars tbody
                    tvpatts = map snd tyenv'
                    tvrepls = catUTyVars . map (\ x -> revAssocd x tyenv' x) $ 
                                intersect tvbody tvpatts 
                    tv' = variantUTyVar ((tvbody \\ tvpatts) `union` tvrepls) tv
                in
                  PUTy tv' $ pretypeSubst ((tv', tv):tyenv') tbody
           else PUTy tv $ pretypeSubst tyenv' tbody

-- Apply type unifications
solve :: PEnv -> PreType -> PreType
solve _ pty@PTyCon{} = pty 
solve _ pty@UTyVar{} = pty
solve env pty@(STyVar i) =
    fromMaybe pty . liftM (solve env) $ lookup i env
solve env (PTyComb f args) = 
    PTyComb (solve env f) $ map (solve env) args
solve env (PUTy tv tbod) =
    PUTy tv $ solve env tbod

-- Post typechecking elaboration
tyElabRef :: PreType -> ElabM s HOLType
tyElabRef (PTyCon s) = mkType s []
tyElabRef (PTyComb STyVar{} []) =
    do modElabState $ set stvsTrans True
       throwE [TyElabError $ "system type variable present in type " ++
                             "application of null arity."]
tyElabRef (PTyComb (STyVar n) args) =
    do modElabState $ set stovsTrans True
       mkType ('?' `cons` textShow n) =<< mapM tyElabRef args
tyElabRef (PTyComb (UTyVar _ v _) args) =
    mkType v =<< mapM tyElabRef args
tyElabRef (PTyComb (PTyCon s) args) =
    mkType s =<< mapM tyElabRef args
tyElabRef PTyComb{} =
    throwE [TyElabError "unexpected first argument to type combination."]
tyElabRef (PUTy (UTyVar _ s 0) tbody) =
    do tbody' <- tyElabRef tbody
       return . fromRight $ do s' <- mkSmall $ mkVarType s
                               mkUType s' tbody'
tyElabRef (PUTy tv@(STyVar n) tbody) =
    do modElabState $ set stvsTrans True
       addWarning True "tyElab: system type variable in universal type."
       let tv' = pack $ '?' : show n
       tbody' <- tyElabRef $ pretypeSubst [(UTyVar True tv' 0, tv)] tbody
       case mkUType tbody' =<< mkSmall (mkVarType tv') of
         Right res -> return res
         Left e -> throwE [TyElabError e]
tyElabRef PUTy{} =
    throwE [TyElabError "invalid universal type construction."]
tyElabRef (STyVar n) =
    do modElabState $ set stvsTrans True
       let tv = mkVarType $ '?' `cons` textShow n
       stvs <- viewElabState $ view smallSTVS
       if n `elem` stvs
          then return . fromRight $ mkSmall tv
          else return tv
tyElabRef (UTyVar True v _) = 
    return . fromRight . mkSmall $ mkVarType v
tyElabRef (UTyVar False v _) = return $! mkVarType v

tmElab :: PreTerm -> ElabM s HOLTerm
tmElab ptm =
    do modElabState $ \ st -> st & 
         set stvsTrans False & set stovsTrans False
       tm <- tmElabRec ptm
       fl1 <- viewElabState $ view stvsTrans
       fl2 <- viewElabState $ view stovsTrans
       addWarning (fl1 && invWarning) 
         "warning: inventing type variables"
       addWarning (fl2 && invWarning) 
         "warning: inventing type operator variables"
       return tm
  where tmElabRec :: PreTerm -> ElabM s HOLTerm
        tmElabRec PApp{} =
            fail $ "tmElab: type application present outside of type " ++
                   "combination term"
        tmElabRec (PVar s pty) = 
            liftM (mkVar s) $ tyElabRef pty
        tmElabRec (PConst s pty) = 
            mkMConst s =<< tyElabRef pty
        tmElabRec (PInst tvis (PConst c pty)) =
            do tvis' <- mapM (\ (ty, x) -> do ty' <- tyElabRef ty
                                              return (mkVarType x, ty')) tvis
               pty' <- tyElabRef pty
               mkTIConst c pty' tvis'
        tmElabRec PInst{} =
            fail "tmElab: body of TYINST not a constant."
        tmElabRec (PComb l r) =
            do (l', r') <- pairMapM tmElabRec (l, r)
               return . fromRight $ mkComb l' r'
        tmElabRec (PAbs v bod) =
            do (v', bod') <- pairMapM tmElabRec (v, bod)
               mkGAbs v' bod'
        tmElabRec (TyPAbs tv t) =
            do tv' <- tyElabRef tv
               t' <- tmElabRec t
               return . fromRight $ mkTyAbs tv' t'
        tmElabRec (TyPComb t _ ti) =
            do t' <- tmElabRec t
               ti' <- tyElabRef ti
               return . fromRight $ mkTyComb t' ti'
        tmElabRec (PAs tm _) = tmElabRec tm

-- retypecheck
preTypeOf :: [(Text, PreType)] -> PreTerm -> ElabM s PreTerm
preTypeOf senv pretm = 
    do ty <- newTypeVar
       modElabState $ set smallSTVS []
       (tm', _, env) <- typify ty (pretm, senv, []) <?> 
                          "typechecking: initial type assignment"
       env' <- resolveInterface tm' return env <?> 
                 "typechecking: overload resolution"
       solvePreterm env' tm'
  where typify :: PreType -> (PreTerm, [(Text, PreType)], PEnv) -> 
                  ElabM s (PreTerm, [(Text, PreType)], PEnv)
        typify ty (PVar s _, venv, uenv) =
            case lookup s venv of
              Just ty' -> 
                let flag = addTyAppsAuto
                    tys = if flag then solve uenv ty' else ty' in
                  if flag && isJust (destPUTy tys) && 
                     not (unifiableWithUType ty uenv)
                  then do tys' <- addTyApps (PVar s tys) tys
                          typify ty (tys', venv, uenv)
                  else do uenv' <- unify uenv (tys, ty)
                          return (PVar s tys, [], uenv')
              Nothing -> 
                case numOfString (unpack s) of
                  Just s' -> 
                    let t = pmkNumeral (s' :: Integer) in
                      do uenv' <- unify uenv (PTyComb (PTyCon "num") [], ty)
                         return (t, [], uenv')
                  Nothing ->
                    (do s' <- getGenericType s
                        hid <- getHidden
                        if s `notElem` hid
                           then do tys <- pretypeInstance s'
                                   if addTyAppsAuto && isJust (destPUTy tys) &&
                                      not (unifiableWithUType ty uenv)
                                      then do ty' <- addTyApps (PVar s tys) tys
                                              typify ty (ty', venv, uenv)
                                      else do uenv' <- unify uenv (tys, ty)
                                              return (PConst s tys, [], uenv')
                           else mzero)
                    <|> return (PVar s ty, [(s, ty)], uenv)
        typify ty (PInst tvis (PVar c _), _, uenv) =
            do c' <- getGenericType c <?> ("typify: TYINST can only be " ++ 
                                           "applied to a constant: " ++ show c)
               let cty = pretypeOfType c'
               let tvs = map snd tvis
                   rtvs = filter (\ x -> case x of
                                             UTyVar _ x' _ -> x' `elem` tvs
                                             _ -> False) $ utyvars cty
               if length rtvs < length tvs
                  then let rtvNames = fromJust $ mapM destUTy rtvs
                           (missing:_) = filter (`notElem` rtvNames) tvs in
                         fail $ "typify: TYINST: type does not contain " ++ 
                                "tyvar " ++ show missing
                  else let subs = fromJust $ mapM (\ tv -> 
                                                   do x <- destUTy tv
                                                      x' <- revAssoc x tvis
                                                      return (x', tv)) rtvs
                           tvis' = fromJust $ mapM (\ (t, tv) ->
                                                    do x <- destUTy tv
                                                       return (t, x)) subs in
                         do ctyrep <- replaceUtvsWithStvs tvs cty
                            let cty' = pretypeSubst subs ctyrep
                            uenv' <- unify uenv (cty', ty)
                            return (PInst tvis' (PConst c cty'), [], uenv')
        typify ty (PComb t (PApp ti), venv, uenv) =
            do ntv <- newTypeVar
               (t', venv1, uenv1) <- typify ntv (t, venv, uenv)
               case solve uenv1 ntv of
                 ty'@(PUTy ty1 ty2) -> 
                     do uenv1' <-  unify uenv1 
                                     (ty, pretypeSubst [(ti, ty1)] ty2)
                        return (TyPComb t' ty' ti, venv1, uenv1')
                 _ -> fail $ "typify: Type application argument maybe not " ++
                             "of universal type: " ++ show t
        typify ty (PComb f x, venv, uenv) =
            do ty'' <- newTypeVar
               let ty' = mkFunPTy ty'' ty
               (f', venv1, uenv1) <- typify ty' (f, venv, uenv)
               (x', venv2, uenv2) <- typify (solve uenv1 ty'') 
                                       (x, venv1 ++ venv, uenv1)
               return (PComb f' x', venv1 ++ venv2, uenv2)
        typify ty (ptm@(PAs tm pty), venv, uenv) =
            if addTyAppsAuto && isJust (destPUTy pty) && 
               not (unifiableWithUType ty uenv)
            then do ty' <- addTyApps ptm pty
                    typify ty (ty', venv, uenv)
            else do uenv' <- unify uenv (ty, pty)
                    typify ty (tm, venv, uenv')
        typify ty (PAbs v bod, venv, uenv) =
            do (ty', ty'') <- case ty of
                                  PTyComb (PTyCon "fun") [ty', ty''] -> 
                                      return (ty', ty'')
                                  _ -> do ty1 <- newTypeVar
                                          ty2 <- newTypeVar
                                          return (ty1, ty2)
               uenv0 <- unify uenv (mkFunPTy ty' ty'', ty)
               (v', venv1, uenv1) <- do (v', venv1, uenv1) <- typify ty' 
                                                                (v, [], uenv0)
                                        case v' of
                                          PConst s _ -> return $! 
                                            if ignoreConstVarstruct
                                            then (PVar s ty', [(s, ty')], uenv0)
                                            else (v', venv1, uenv1)
                                          _ -> return (v', venv1, uenv1)
               (bod', venv2, uenv2) <- typify ty'' (bod, venv1 ++ venv, uenv1)
               return (PAbs v' bod', venv2, uenv2)
        typify ty (TyPAbs tv bod, venv, uenv) =
            do ntv <- newTypeVar
               (bod', venv1, uenv1) <- typify ntv (bod, venv, uenv)
               uenv2 <- unify uenv1 (PUTy tv $ solve uenv1 ntv, ty)
               return (TyPAbs tv bod', venv1, uenv2)
        typify ty (TyPComb t (PUTy ty1 ty2) ti, venv, uenv) =
            do uenv0 <- unify uenv (pretypeSubst [(ti, ty1)] ty2, ty)
               (t', venv1, uenv1) <- typify (PUTy ty1 ty2) (t, venv, uenv0)
               return (TyPComb t' (PUTy ty1 ty2) ti, venv1, uenv1)
        typify _ (ptm, _, _) =
            fail $ "typify: unexpected preterm at this stage: " ++ show ptm

-- Give system type vars for all free type vars, except those in tys
        replaceUtvsWithStvs :: [Text] -> PreType -> ElabM s PreType
        replaceUtvsWithStvs tys pty =
            do tyvs' <- mapM subsf $ utyvars pty
               return $! pretypeSubst tyvs' pty    
          where subsf :: PreType -> ElabM s (PreType, PreType)
                subsf tv@(UTyVar f s _) =
                    if s `elem` tys then return (tv, tv)
                    else do tv'@(STyVar n) <- newTypeVar
                            when f . modElabState $ over smallSTVS ((:) n)
                            return (tv', tv)
                subsf _ = fail "replaceUtvsWithStvs"

        pretypeInstance :: HOLType -> ElabM s PreType
        pretypeInstance = replaceUtvsWithStvs [] . pretypeOfType

-- Type constraint specialization by resolving overloadings
        resolveInterface :: PreTerm -> (PEnv -> ElabM s PEnv) -> PEnv -> 
                            ElabM s PEnv
        resolveInterface PApp{} _ _ =
            fail "resolveInterface: type application"
        resolveInterface (PComb f x) cont env =
            resolveInterface f (resolveInterface x cont) env
        resolveInterface (PAbs v bod) cont env =
            resolveInterface v (resolveInterface bod cont) env
        resolveInterface (TyPAbs _ bod) cont env =
            resolveInterface bod cont env
        resolveInterface (TyPComb t _ _) cont env =
            resolveInterface t cont env
        resolveInterface PAs{} _ _ =
            fail "resolveInterface: type ascription"
        resolveInterface (PInst _ bod) cont env =
            resolveInterface bod cont env
        resolveInterface PVar{} cont env =
            cont env
        resolveInterface (PConst s ty) cont env =
            do iface <- getInterface
               let maps = filter (\ (s', _) -> s' == s) iface
               if null maps 
                  then cont env
                  else tryFind (\ (_, (_, ty')) -> 
                                do ty'' <- pretypeInstance ty'
                                   cont =<< unify env (ty'', ty)) maps

-- Push specialization throughout a preterm
        solvePreterm :: PEnv -> PreTerm -> ElabM s PreTerm
        solvePreterm _ PApp{} =
            fail "solvePreterm: type application"
        solvePreterm env (PVar s ty) = return . PVar s $ solve env ty
        solvePreterm env (PComb f x) =
            do (f', x') <- pairMapM (solvePreterm env) (f, x)
               return $! PComb f' x'
        solvePreterm env (PAbs v bod) =
            do (v', bod') <- pairMapM (solvePreterm env) (v, bod)
               return $! PAbs v' bod'
        solvePreterm env (TyPAbs tv bod) =
            liftM (TyPAbs tv) $ solvePreterm env bod
        solvePreterm env (TyPComb t ty ti) =
            let ti' = solve env ti in
              do modElabState $ over smallSTVS (`union` freeSTVS0 ti')
                 t' <- solvePreterm env t
                 return $! TyPComb t' (solve env ty) ti'
        solvePreterm env (PInst tys bod) =
            liftM (PInst tys) $ solvePreterm env bod
        solvePreterm _ PAs{} =
            fail "solvePreterm: type ascription"
        solvePreterm env (PConst s ty) =
            let tys = solve env ty in
              (do iface <- getInterface
                  c' <- tryFind (\ (s', (c', ty')) ->
                                 if s == s'
                                 then do ty'' <- pretypeInstance ty'
                                         _ <- unify env (ty'', ty)
                                         return c'
                                 else mzero) iface
                  pmkCV c' tys)
              <|> (return $! PConst s tys)
          where pmkCV :: Text -> PreType -> ElabM s PreTerm
                pmkCV name pty = 
                    do cond <- can getConstType name
                       return $! if cond then PConst name pty 
                                 else PVar name pty

-- Unification of types
        unify :: PEnv -> (PreType, PreType) -> ElabM s PEnv
        unify env (ty1, ty2)
            | ty1 == ty2 = return env
            | otherwise = 
                case (ty1, ty2) of
                  (PTyComb f@STyVar{} fargs, PTyComb g gargs) ->
                    if length fargs == length gargs
                    then foldrM (flip unify) env $ (f, g) : zip fargs gargs
                    else fail $ "unify: " ++ show f ++ " WITH " ++ show g
                  (PTyComb{}, PTyComb STyVar{} _) ->
                    unify env (ty2, ty1)
                  (PTyComb f fargs, PTyComb g gargs) ->
                    if f == g && length fargs == length gargs
                    then foldrM (flip unify) env $ zip fargs gargs
                    else fail $ "unify: " ++ show f ++ " WITH " ++ show g
                  (PUTy tv1@UTyVar{} tbody1, PUTy tv2@UTyVar{} tbody2) ->
                      if tv1 == tv2 then unify env (tbody1, tbody2)
                      else let tv = variantUTyVar (utyvars tbody1 `union` 
                                                   utyvars tbody2) tv1 in
                             unify env (pretypeSubst [(tv, tv1)] tbody1, 
                                        pretypeSubst [(tv, tv2)] tbody2)
                  (STyVar x, t) -> handleSTVS x t
                  (t, STyVar x) -> handleSTVS x t
                  _ -> fail $ "unify: " ++ show ty1 ++ " WITH " ++ show ty2
          where handleSTVS :: Integer -> PreType -> ElabM s PEnv
                handleSTVS x t =
                  case lookup x env of
                   Just x' -> unify env (x', t)
                   _ -> case istrivial env x t of
                          Just True -> return env
                          Just False ->
                              do stvs <- viewElabState $ view smallSTVS
                                 let t' = destSTV t
                                 when (isJust t' && x `elem` stvs) .
                                   modElabState $ 
                                     over smallSTVS ((:) (fromJust t'))
                                 return $! insertMap x t env
                          Nothing -> fail "handleSTVS"

-- | Elaborator for 'PreType's.
tyElab :: ParseContext -> PreType -> Either [ElabError] HOLType
tyElab ctxt pty = runST $
    do ref <- newSTRef $ initElabState ctxt
       runReaderT (runExceptT $ tyElabRef pty) ref

-- | Elaborator and type inference for 'PreTerm's.
elab :: ParseContext -> PreTerm -> Either [ElabError] HOLTerm
elab ctxt ptm = runST $
    do ref <- newSTRef $ initElabState ctxt
       runReaderT (runExceptT $ tmElab =<< preTypeOf [] ptm) ref
