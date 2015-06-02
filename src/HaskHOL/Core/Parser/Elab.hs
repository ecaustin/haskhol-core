{-# LANGUAGE OverloadedStrings #-}
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
    , getOverloads
    , getInterface
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State
import HaskHOL.Core.Basics

import HaskHOL.Core.Parser.Lib
import HaskHOL.Core.Parser.Prims

{-| 
  Returns the list of all overloadable symbols paired with their most generic 
  types.
-}
getOverloads :: HOL cls thry (Map Text HOLType)
getOverloads = viewParseContext overloads

-- | Returns the list of all currently defined interface overloads.
getInterface :: HOL cls thry [(Text, (Text, HOLType))]
getInterface = viewParseContext interface

{- 
  PEnv is an environment that carries an association between system type
  variables and their unifiable type.  It is defined as a type synonym in case
  we ever need to change its implementation for performance reasons.
-}
type PEnv = [(Integer, PreType)]

-- utility functions
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

{- 
  Used to construct a constant that has one or more instantiations provided for
  its type operator variables.
-}
mkTIConst :: Text -> HOLType -> HOLTypeEnv -> HOL cls thry HOLTerm
mkTIConst c ty subs =
    (do cty' <- liftM (typeSubst subs) $ getConstType c
        let (mat1Tys, opTys, opOps) = fromJust $ typeMatch cty' ty ([], [], [])
            tys' = map (second $ typeSubst mat1Tys) subs ++ mat1Tys
            mat2 = (tys', opTys, opOps)
        con <- mkConstFull c mat2
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
newTypeVar :: TypingState -> HOL cls thry PreType
newTypeVar ref = return STyVar <*> tickTypeCounter' ref

-- Turn UType term into a non-UType by adding type applications
addTyApps :: TypingState -> PreTerm -> PreType -> HOL cls thry PreTerm
addTyApps ref tm ty@(PUTy tv tb) =
    do ntv <- newTypeVar ref
       addTyApps ref (TyPComb tm ty ntv) $ pretypeSubst [(ntv, tv)] tb
addTyApps _ tm _ = return tm

-- Check where unification of ty with a UType is potentially possible
unifiableWithUType :: PreType -> PEnv -> Bool
unifiableWithUType ty env
    | ty == dpty = False
    | otherwise = 
          let tys = solve env ty in
            isJust (destPUTy tys) || isJust (destSTV tys)

-- Get a new instance of a constant's generic type modulo interface
getGenericType :: Text -> HOL cls thry HOLType
getGenericType cname =
    do iface <- getInterface
       case filter (\ (x, _) -> x == cname) iface of
         [(_, (_, ty))] -> return ty
         (_:_:_) -> do overs <- getOverloads
                       liftMaybe "getGenericType" $ mapLookup cname overs
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
                    tv' = variantUTyVar ((tvbody \\ tvpatts) `union` tvrepls) tv in
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
                            
-- Final type checker for preterms
{- 
  Local reference that tracks if we invent system type (operator) variables.
  Also used to track the smallness constraints of system type variables.
-}
type TypingState = HOLRef TypingRefs
data TypingRefs = TypingRefs
    { stvsTrans :: !Bool
    , stovsTrans :: !Bool
    , smallSTVS :: ![Integer]
    , tyCounter :: !Integer
    }

tickTypeCounter' :: TypingState -> HOL cls thry Integer
tickTypeCounter' ref = 
    do n' <- liftM (succ . tyCounter) $ readHOLRef ref
       modifyHOLRef ref $ \ st -> st { tyCounter = n' }
       return n'

-- Post typechecking elaboration
tyElabRef :: TypingState -> PreType -> HOL cls thry HOLType
tyElabRef _ (PTyCon s) = mkType s []
tyElabRef ref (PTyComb STyVar{} []) =
    do modifyHOLRef ref $ \ st -> st { stvsTrans = True }
       fail $ "tyElab: system type variable present in type application of " ++
              "null arity"
tyElabRef ref (PTyComb (STyVar n) args) =
    do modifyHOLRef ref $ \ st -> st { stovsTrans = True }
       mkType ('?' `cons` textShow n) =<< mapM (tyElabRef ref) args
tyElabRef ref (PTyComb (UTyVar _ v _) args) =
    mkType v =<< mapM (tyElabRef ref) args
tyElabRef ref (PTyComb (PTyCon s) args) =
    mkType s =<< mapM (tyElabRef ref) args
tyElabRef _ PTyComb{} =
    fail "tyElab: unexpected first argument to type combination"
tyElabRef ref (PUTy (UTyVar _ s 0) tbody) =
    do tbody' <- tyElabRef ref tbody
       liftM1 mkUType (mkSmall $ mkVarType s) tbody' <#?> 
         "tyElab: universal type"
tyElabRef ref (PUTy STyVar{} _) =
    do modifyHOLRef ref $ \ st -> st { stvsTrans = True }
       fail "tyElab: system type variable in universal type."
-- Alternative way to handle this if we don't want to make it an error, 
-- per HOL2P:
{-
tyElabRef ref (PUTy tv@(STyVar n) tbody) =
    do modifyHOLRef ref $ \ st -> st { stvsTrans = True }
       warn True "tyElab: system type variable in universal type."
       let tv' = '?' : show n
       tbody' <- tyElabRef ref $ pretypeSubst [(UTyVar True tv' 0, tv)] tbody
       liftMaybe "tyElab: universal type - system type variable case" $
         do tv'' <- mkSmall $ mkVarType tv'
            mkUType tv'' tbody'
-}
tyElabRef _ PUTy{} =
    fail "tyElab: invalid universal type construction."
tyElabRef ref (STyVar n) =
    do modifyHOLRef ref $ \ st -> st { stvsTrans = True }
       let tv = mkVarType $ '?' `cons` textShow n
       st <- readHOLRef ref
       if n `elem` smallSTVS st
          then mkSmall tv <#?> "tyElab: small system type variable"
          else return tv
tyElabRef _ (UTyVar True v _) = 
    mkSmall (mkVarType v) <#?> "tyElab: small user type variable"
tyElabRef _ (UTyVar False v _) = return $! mkVarType v

tmElab :: TypingState -> PreTerm -> HOL cls thry HOLTerm
tmElab ref ptm =
    do modifyHOLRef ref $ \ st -> st { stvsTrans = False, stovsTrans = False }
       tm <- tmElabRec ptm
       st <- readHOLRef ref
       flag1 <- getBenignFlag FlagTyInvWarning
       flag2 <- getBenignFlag FlagTyOpInvWarning
       warn (stvsTrans st && flag1) 
         "warning: inventing type variables"
       warn (stovsTrans st && flag2) 
         "warning: inventing type operator variables"
       return tm
  where tmElabRec :: PreTerm -> HOL cls thry HOLTerm
        tmElabRec PApp{} =
            fail $ "tmElab: type application present outside of type " ++
                   "combination term"
        tmElabRec (PVar s pty) = 
            liftM (mkVar s) $ tyElabRef ref pty
        tmElabRec (PConst s pty) = 
            mkMConst s =<< tyElabRef ref pty
        tmElabRec (PInst tvis (PConst c pty)) =
            do tvis' <- mapM (\ (ty, x) -> do ty' <- tyElabRef ref ty
                                              return (mkVarType x, ty')) tvis
               pty' <- tyElabRef ref pty
               mkTIConst c pty' tvis'
        tmElabRec PInst{} =
            fail "tmElab: body of TYINST not a constant."
        tmElabRec (PComb l r) =
            do (l', r') <- pairMapM tmElabRec (l, r)
               liftEither "tmElab" $ mkComb l' r'
        tmElabRec (PAbs v bod) =
            do (v', bod') <- pairMapM tmElabRec (v, bod)
               mkGAbs v' bod'
        tmElabRec (TyPAbs tv t) =
            do tv' <- tyElabRef ref tv
               t' <- tmElabRec t
               liftEither "tmElab" $ mkTyAbs tv' t'
        tmElabRec (TyPComb t _ ti) =
            do t' <- tmElabRec t
               ti' <- tyElabRef ref ti
               liftEither "tmElab" $ mkTyComb t' ti'
        tmElabRec (PAs tm _) = tmElabRec tm

-- retypecheck
preTypeOf :: TypingState -> [(Text, PreType)] -> PreTerm -> 
             HOL cls thry PreTerm
preTypeOf ref senv pretm = 
    do ty <- newTypeVar ref
       modifyHOLRef ref $ \ st -> st { smallSTVS = [] }
       (tm', _, env) <- typify ty (pretm, senv, []) <?> 
                          "typechecking: initial type assignment"
       env' <- resolveInterface tm' return env <?> 
                 "typechecking: overload resolution"
       solvePreterm env' tm'
  where typify :: PreType -> (PreTerm, [(Text, PreType)], PEnv) -> 
                  HOL cls thry (PreTerm, [(Text, PreType)], PEnv)
        typify ty (PVar s _, venv, uenv) =
            case lookup s venv of
              Just ty' -> 
                do flag <- getBenignFlag FlagAddTyAppsAuto
                   let tys = if flag then solve uenv ty' else ty'
                   if flag && isJust (destPUTy tys) && 
                      not (unifiableWithUType ty uenv)
                      then do tys' <- addTyApps ref (PVar s tys) tys
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
                        hidden <- getHidden
                        if s `notElem` hidden
                           then do flag <- getBenignFlag FlagAddTyAppsAuto
                                   tys <- pretypeInstance s'
                                   if flag && isJust (destPUTy tys) &&
                                      not (unifiableWithUType ty uenv)
                                      then do ty' <- addTyApps ref (PVar s tys) tys
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
            do ntv <- newTypeVar ref
               (t', venv1, uenv1) <- typify ntv (t, venv, uenv)
               case solve uenv1 ntv of
                 ty'@(PUTy ty1 ty2) -> 
                     do uenv1' <-  unify uenv1 
                                     (ty, pretypeSubst [(ti, ty1)] ty2)
                        return (TyPComb t' ty' ti, venv1, uenv1')
                 _ -> fail $ "typify: Type application argument maybe not " ++
                             "of universal type: " ++ show t
        typify ty (PComb f x, venv, uenv) =
            do ty'' <- newTypeVar ref
               let ty' = mkFunPTy ty'' ty
               (f', venv1, uenv1) <- typify ty' (f, venv, uenv)
               (x', venv2, uenv2) <- typify (solve uenv1 ty'') 
                                       (x, venv1 ++ venv, uenv1)
               return (PComb f' x', venv1 ++ venv2, uenv2)
        typify ty (ptm@(PAs tm pty), venv, uenv) =
            do flag <- getBenignFlag FlagAddTyAppsAuto
               if flag && isJust (destPUTy pty) && 
                  not (unifiableWithUType ty uenv)
                  then do ty' <- addTyApps ref ptm pty
                          typify ty (ty', venv, uenv)
                  else do uenv' <- unify uenv (ty, pty)
                          typify ty (tm, venv, uenv')
        typify ty (PAbs v bod, venv, uenv) =
            do (ty', ty'') <- case ty of
                                  PTyComb (PTyCon "fun") [ty', ty''] -> 
                                      return (ty', ty'')
                                  _ -> do ty1 <- newTypeVar ref
                                          ty2 <- newTypeVar ref
                                          return (ty1, ty2)
               uenv0 <- unify uenv (mkFunPTy ty' ty'', ty)
               (v', venv1, uenv1) <- do (v', venv1, uenv1) <- typify ty' 
                                                                (v, [], uenv0)
                                        flag <- getBenignFlag 
                                                  FlagIgnoreConstVarstruct
                                        case v' of
                                          PConst s _ -> return $! 
                                            if flag
                                            then (PVar s ty', [(s, ty')], uenv0)
                                            else (v', venv1, uenv1)
                                          _ -> return (v', venv1, uenv1)
               (bod', venv2, uenv2) <- typify ty'' (bod, venv1 ++ venv, uenv1)
               return (PAbs v' bod', venv2, uenv2)
        typify ty (TyPAbs tv bod, venv, uenv) =
            do ntv <- newTypeVar ref
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
        replaceUtvsWithStvs :: [Text] -> PreType -> HOL cls thry PreType
        replaceUtvsWithStvs tys pty =
            do tyvs' <- mapM subsf $ utyvars pty
               return $! pretypeSubst tyvs' pty    
          where subsf :: PreType -> HOL cls thry (PreType, PreType)
                subsf tv@(UTyVar f s _) =
                    if s `elem` tys then return (tv, tv)
                    else do tv'@(STyVar n) <- newTypeVar ref
                            when f $ modifyHOLRef ref 
                              (\ st -> st { smallSTVS = n : smallSTVS st})
                            return (tv', tv)
                subsf _ = fail "replaceUtvsWithStvs"

        pretypeInstance :: HOLType -> HOL cls thry PreType
        pretypeInstance = replaceUtvsWithStvs [] . pretypeOfType

-- Type constraint specialization by resolving overloadings
        resolveInterface :: PreTerm -> (PEnv -> HOL cls thry PEnv) -> PEnv -> 
                            HOL cls thry PEnv
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
        solvePreterm :: PEnv -> PreTerm -> HOL cls thry PreTerm
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
              do modifyHOLRef ref $
                   \ st -> st { smallSTVS = smallSTVS st `union` freeSTVS0 ti' }
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
          where pmkCV :: Text -> PreType -> HOL cls thry PreTerm
                pmkCV name pty = 
                    do cond <- can getConstType name
                       return $! if cond then PConst name pty 
                                 else PVar name pty

-- Unification of types
        unify :: PEnv -> (PreType, PreType) -> HOL cls thry PEnv
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
          where handleSTVS :: Integer -> PreType -> HOL cls thry PEnv
                handleSTVS x t =
                  case lookup x env of
                   Just x' -> unify env (x', t)
                   _ -> case istrivial env x t of
                          Just True -> return env
                          Just False ->
                              do st <- readHOLRef ref
                                 let t' = destSTV t
                                 when (isJust t' && x `elem` smallSTVS st) $
                                   modifyHOLRef ref $ \ s -> 
                                     s { smallSTVS = fromJust t' : smallSTVS s }
                                 return $! insertMap x t env
                          Nothing -> fail "handleSTVS"

-- | Elaborator for 'PreType's.
tyElab :: PreType -> HOL cls thry HOLType
tyElab pty =
    do ref <- newHOLRef $ TypingRefs False False [] 0
       tyElabRef ref pty

-- | Elaborator and type inference for 'PreTerm's.
elab :: PreTerm -> HOL cls thry HOLTerm
elab ptm = 
    do ref <- newHOLRef $ TypingRefs False False [] 0
       tmElab ref =<< preTypeOf ref [] ptm
