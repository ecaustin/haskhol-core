{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
module HaskHOL.Core.Basics.Stateful where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel

-- Pre-Req Syntax
{-| 
  A version of 'mkBinary' that accepts the operator as a pre-constructed term.
-}
mkBinop :: MonadCatch m => HOLTerm -> HOLTerm -> HOLTerm -> m HOLTerm
mkBinop op tm1 tm2 = 
    (do tm <- mkComb op tm1
        mkComb tm tm2) <?> "mkBinop"

{-|
  A version of 'mkComb' that instantiates the type variables in the left hand
  argument.  Relies internally on 'typeMatch' in order to provide a match
  between the domain type of the function and the type of the argument.  Fails
  with 'Nothing' if instantiation is impossible.
-}
mkIComb :: MonadCatch m => HOLTerm -> HOLTerm -> m HOLTerm
mkIComb tm1 tm2 =
    do (ty, _) <- destFunTy $ typeOf tm1
       mat <- typeMatch ty (typeOf tm2) ([], [], [])
       mkComb (instFull mat tm1) tm2

{-|
  Constructs a complex combination that represents the application of a 
  function to a list of arguments.  Fails with 'Left' if any internal call to 
  'mkComb' fails.
-}
listMkComb :: MonadThrow m => HOLTerm -> [HOLTerm] -> m HOLTerm
listMkComb = foldlM mkComb

-- Stateful Basics
{-|
  Retrieves the type of a given term constant.  Throws a 'HOLException' if the
  provided term constant name is not defined.
-}
getConstType :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
             => Text -> m HOLType
getConstType name =
    do consts <- ?constsFun
       (typeOf `fmap` mapAssoc name consts) <?> 
         "getConstType: not a constant name"

{-|
  Constructs a type application given an operator name and a list of argument
  types.  If the provided name is not a currently defined type constant then
  this function defaults it to a type operator variable.  Throws a 
  'HOLException' in the following cases:

  * A type operator's arity disagrees with the length of the argument list.

  * A type operator is applied to zero arguments.
-}
mkType :: (MonadCatch m, ?typesFun :: m (Map Text TypeOp)) 
       => Text -> [HOLType] -> m HOLType
mkType name args =
    do consts <- ?typesFun
       case runCatch $ mapAssoc name consts of
         Right tyOp -> tyApp tyOp args <?> 
                        "mkType: type constructor application failed"
         Left{} -> 
           {- This seemed to be the easiest way to supress superfluous warnings
              when parsing type operators. -}
           do name' <- if textHead name == '_'
                       then return $! textTail name
                       else return name
              failWhen (return $ null args)
                "mkType: type operator applied to zero args."
              tyApp (mkTypeOpVar name') args <?> 
                "mkType: type operator variable application failed"

{-|
  Constructs a function type safely using 'mkType'.  Should never fail provided
  that the initial value for type constants has not been modified.
-}
mkFunTy :: (MonadCatch m, ?typesFun :: m (Map Text TypeOp)) 
        => HOLType -> HOLType -> m HOLType
mkFunTy ty1 ty2 = mkType "fun" [ty1, ty2]

{-|
  Constructs a specific instance of a term constant when provided with its name
  and a type substition environment.  Throws a 'HOLException' in the 
  following cases:

  * The instantiation as performed by 'instConst' fails.

  * The provided name is not a currently defined constant.
-}
mkConst :: (MonadCatch m, TypeSubst l r, ?constsFun :: m (Map Text HOLTerm)) 
        => Text -> [(l, r)] -> m HOLTerm
mkConst name tyenv =
    do consts <- ?constsFun
       tm <- mapAssoc name consts <?> "mkConst: not a constant name"
       instConst tm tyenv <?> "mkConst: instantiation failed"
  
{-| 
  A version of 'mkConst' that accepts a triplet of type substitition 
  environments.  Frequently used with the 'typeMatch' function.
-}
mkConst_FULL :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm))
             => Text -> SubstTrip -> m HOLTerm
mkConst_FULL name tyenv =
    do consts <- ?constsFun
       tm <- mapAssoc name consts <?>  "mkConstFull: not a constant name"
       instConstFull tm tyenv <?> "mkConstFull: instantiation failed"

mkConst_NIL :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm))
            => Text -> m HOLTerm
mkConst_NIL tm = mkConst tm ([] :: HOLTypeEnv)

-- matching version of mkConst
{-|
  Constructs an instance of a constant of the provided name and type.  Relies
  internally on 'typeMatch' in order to provide a match between the most general
  type of the constant and the provided type.  Throws a 'HOLException' in the
  following cases:

  * The provided string is not the name of a defined constant.

  * Type matching fails.
-}
mkMConst :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm))
         => Text -> HOLType -> m HOLTerm
mkMConst name ty = 
  do uty <- getConstType name <?> "mkMConst: not a constant name"
     (mkConst_FULL name =<< typeMatch uty ty ([], [], [])) <?>
       "mkMConst: generic type cannot be instantiated"

{-|
  An iterative version of 'mkIComb' that builds a complex combination given a
  constant name and a list of arguments, attempting to find a correct
  instantiation at every step.  Throws a 'HOLException' in the following cases:

  * The provided name is not a currently defiend constant.

  * Any internal call to mkIComb fails.
-}
listMkIComb :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
            => Text -> [HOLTerm] -> m HOLTerm
listMkIComb cname args =
    do cnst <- mkConst_NIL cname <?> "listMkIComb: not a constant name"
       foldlM mkIComb cnst args <?> "listMkIComb: type cannot be instantiated"

{-|
  Constructs a binary application given a constant name and two argument terms.
  Note that no instantiation is performed, thus the constant must be monomorphic
  or the provided arguments must match the constant's general type.  Throws a
  'HOLException' if any of the internal calls to 'mkConst' or 'mkComb' fail.
-}
mkBinary :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
         => Text -> HOLTerm -> HOLTerm -> m HOLTerm
mkBinary s l r = 
  (do c <- mkConst_NIL s
      let c' = inst [(tyA, typeOf l), (tyB, typeOf r)] c
      l' <- mkComb c' l
      mkComb l' r)
  <?> "mkBinary: " ++ show s

{-|
  Constructs an abstraction given a binder name and two argument terms.  Throws
  a 'HOLException' if any of the internal calls to 'mkConst', 'mkAbs', or 
  'mkComb' fail.

  Note that the given string can actually be any constant name of type 
  @(A -> *) -> *@, such that a well-typed term of the form @c (\\x . t)@ can be
  produced.
-}
mkBinder :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
         => Text -> HOLTerm -> HOLTerm -> m HOLTerm
mkBinder op v tm = 
    (do op' <- mkConst op [(tyA, typeOf v)]
        mkComb op' =<< mkAbs v tm) <?> "mkBinder: " ++ show op

{-|
  Constructs a type abstraction given a type binder name, a type variable to
  find, and a body term.  Throws a 'HOLException' if any of the internal calls
  to 'mkConst', 'mkTyAbs', or 'mkComb' fail.

  Note that the given string can actually be any constant name of type
  @(% 'a . *) -> *@, such that a well-typed term of the form @c (\\\\x . t)@ can
  be produced.
-}
mkTyBinder :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
           => Text -> HOLType -> HOLTerm -> m HOLTerm
mkTyBinder op v tm =
    (do op' <- mkConst_NIL op
        mkComb op' =<< mkTyAbs v tm) <?> "mkTyBinder: " ++ show op

{-|
  Constructor for boolean conjunctions.  Throws a 'HOLException' if the internal
  calls to 'mkComb' fail.
-}
mkIff :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
      => HOLTerm -> HOLTerm -> m HOLTerm
mkIff l r = 
    (do op <- mkConst "=" [(tyA, tyBool)]
        l' <- mkComb op l
        mkComb l' r) <?> "mkIff"

{-|
  Constructor for boolean conjunctions.  Throws a 'HOLException' if the internal
  call to 'mkBinary' fails.
-}
mkConj :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
       => HOLTerm -> HOLTerm -> m HOLTerm
mkConj = mkBinary "/\\"

{-|
  Constructor for boolean implications.  Throws a 'HOLException' if the internal
  call to 'mkBinary' fails.
-}
mkImp :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
      => HOLTerm -> HOLTerm -> m HOLTerm
mkImp = mkBinary "==>"

{-| 
  Constructor for universal term quantification.  Throws a 'HOLException' if the
  internal call to 'mkBinder' fails.
-}
mkForall :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
         => HOLTerm -> HOLTerm -> m HOLTerm
mkForall = mkBinder "!"

{-| 
  Constructor for existential term quantification.  Throws a 'HOLException' if 
  the internal call to 'mkBinder' fails.
-}
mkExists :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
         => HOLTerm -> HOLTerm -> m HOLTerm
mkExists = mkBinder "?"

{-|
  Constructor for boolean disjunctions.  Throws a 'HOLException' if the internal
  call to 'mkBinary' fails.
-}
mkDisj :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
       => HOLTerm -> HOLTerm -> m HOLTerm
mkDisj = mkBinary "\\/"

{-|
  Constructor for boolean negations.  Throws a 'HOLException' if any of the 
  internal calls to 'mkConst' or 'mkComb' fail.
-}
mkNeg :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
      => HOLTerm -> m HOLTerm
mkNeg tm = 
    (do op <- mkConst_NIL "~"
        mkComb op tm) <?> "mkNeg"

{-| 
  Constructor for unique, existential term quantification.  Throws a 
  'HOLException' if the internal call to 'mkBinder' fails.
-}
mkUExists :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
          => HOLTerm -> HOLTerm -> m HOLTerm
mkUExists = mkBinder "?!"

{-|
  Constructor for term-level universal type quantification.  Throws a 
  'HOLException' if the internal call to 'mkTyBinder' fails.
-}
mkTyAll :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
        => HOLType -> HOLTerm -> m HOLTerm
mkTyAll = mkTyBinder "!!"

{-|
  Constructor for term-level existential type quantification.  Throws a 
  'HOLException' if the internal call to 'mkTyBinder' fails.
-}
mkTyEx :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
       => HOLType -> HOLTerm -> m HOLTerm
mkTyEx = mkTyBinder "??"

-- | Constructs a complex conjunction from a given list of propositions.
listMkConj :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
           => [HOLTerm] -> m HOLTerm
listMkConj = foldr1M mkConj

-- | Constructs a complex disjunction from a given list of propositions.
listMkDisj :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
           => [HOLTerm] -> m HOLTerm
listMkDisj = foldr1M mkDisj

-- | A specific version of 'listMkAbs' for universal term quantification.
listMkForall :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
             => [HOLTerm] -> HOLTerm -> m HOLTerm
listMkForall = flip (foldrM mkForall)

-- | A specific version of 'listMkAbs' for existential term quantification.
listMkExists :: (MonadCatch m, ?constsFun :: m (Map Text HOLTerm)) 
             => [HOLTerm] -> HOLTerm -> m HOLTerm
listMkExists = flip (foldrM mkExists)

{-|
  Constructor for generalized abstractions.  Generalized abstractions extend
  term abstractions to the more general of notion of a function mapping some
  structure to some term.  This allows us to bind patterns more complicated
  than a variable, i.e. binding pairs

  > \ (x:num, y:num) -> x + y

  or lists

  > \ CONS x xs -> x

  Note that in the case where the pattern to bind is simply a variable 'mkGAbs'
  just calls 'mkAbs'.
-}
mkGAbs :: forall m. (MonadCatch m, ?typesFun :: m (Map Text TypeOp), 
                     ?constsFun :: m (Map Text HOLTerm)) 
       => HOLTerm -> HOLTerm -> m HOLTerm
mkGAbs tm1@Var{} tm2 = mkAbs tm1 tm2 <?> "mkGAbs: simple abstraction failed"
mkGAbs tm1 tm2 =
    let fvs = frees tm1 in
      (do fTy <- mkFunTy (typeOf tm1) $ typeOf tm2
          let f = variant (frees tm1++frees tm2) $ mkVar "f" fTy
          tm1' <- mkComb f tm1
          bodIn <- listMkForall fvs =<< mkGEq tm1' tm2
          bndr <- mkConst "GABS" [(tyA, fTy)]
          mkComb bndr =<< mkAbs f bodIn) <?> "mkGAbs"
  where mkGEq :: HOLTerm -> HOLTerm -> m HOLTerm
        mkGEq l r = 
            do op <- mkConst "GEQ" [(tyA, typeOf l)]
               mkBinop op l r

-- | A specific version of 'listMkAbs' for general abstractions.
listMkGAbs :: (MonadCatch m, ?typesFun :: m (Map Text TypeOp), 
               ?constsFun :: m (Map Text HOLTerm))
           => [HOLTerm] -> HOLTerm -> m HOLTerm
listMkGAbs = flip (foldrM mkGAbs)

{-|
  Constructs a let binding term provided a list of variable/value pairs and a
  body term.
-}
mkLet :: (MonadCatch m, ?typesFun :: m (Map Text TypeOp), 
          ?constsFun :: m (Map Text HOLTerm))
      => HOLTermEnv -> HOLTerm -> m HOLTerm
mkLet assigs bod =
    do tmLetEnd <- mkConst "LET_END" [(tyA, typeOf bod)]
       let (ls, rs) = unzip assigs
       lend <- mkComb tmLetEnd bod
       lbod <- listMkGAbs ls lend
       (ty1, ty2) <- destFunTy $ typeOf lbod
       tmLet <- mkConst "LET" [(tyA, ty1), (tyB, ty2)]
       listMkComb tmLet (lbod:rs)
