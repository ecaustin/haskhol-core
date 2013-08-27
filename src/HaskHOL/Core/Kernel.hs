{-|
  Module:    HaskHOL.Core.Kernel
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module exports the logical kernel of HaskHOL.  It consists of:

  * The view pattern required to pattern match on terms outside of the kernel.

  * A safe view of HOL theorems for HaskHOL.

  * The primitive inference rules of the system.

  * The primitive, stateless theory extension functions.

  For clarity, all of these items have been seperated based on their influential
  system: HOL Light, Stateless HOL, and HOL2P.

  Note that, per the stateless approach, any stateful, but still primitive,
  functions related to theorems or theory extension have been relocated to the 
  "HaskHOL.Core.State" module.
-}
module HaskHOL.Core.Kernel
    ( -- * A View of HOL Types, Terms, and Theorems
       -- ** A Quick Note on View Patterns
        -- $ViewPatterns
      view -- :: a -> b
       -- ** Destructors and Accessors for Theorems
    , HOLThm
    , HOLThmView(..)
    , destThm -- :: HOLThm -> ([HOLTerm], HOLTerm)
    , hyp     -- :: HOLThm -> [HOLTerm]
    , concl   -- :: HOLThm -> HOLTerm
      -- * HOL Light Primitive Inference Rules
    , primREFL                -- :: HOLTerm -> HOLThm
    , primTRANS               -- :: HOLThm -> HOLThm -> Either String HOLThm
    , primMK_COMB             -- :: HOLThm -> HOLThm -> Either String HOLThm	
    , primABS                 -- :: HOLTerm -> HOLThm -> Either String HOLThm	
    , primBETA                -- :: HOLTerm -> Either String HOLThm
    , primASSUME              -- :: HOLTerm -> Maybe HOLThm
    , primEQ_MP               -- :: HOLThm -> HOLThm -> Either String HOLThm
    , primDEDUCT_ANTISYM_RULE -- :: HOLThm -> HOLThm -> HOLThm
    , primINST_TYPE           -- :: Inst a b => [(a, b)] -> HOLThm -> HOLThm
    , primINST_TYPE_FULL      -- :: SubstTrip -> HOLThm -> HOLThm
    , primINST                -- :: HOLTermEnv -> HOLThm -> HOLThm
      -- * HOL2P Primitive Inference Rules
    , primTYABS  -- :: HOLType -> HOLThm -> Either String HOLThm
    , primTYAPP2 -- :: HOLType -> HOLType -> HOLThm -> Either String HOLThm
    , primTYAPP  -- :: HOLType -> HOLThm -> Maybe HOLThm
    , primTYBETA -- :: HOLTerm -> Either String HOLThm
      -- * Stateless HOL Primitive Theory Extensions
    , axiomThm         -- :: HOLTerm -> HOLThm
    , newDefinedConst  -- :: HOLTerm -> Either String (HOLTerm, HOLThm)
    , newDefinedTypeOp -- :: String -> String -> String -> HOLThm -> Either 
                       --    String (TypeOp, HOLTerm, HOLTerm, HOLThm, HOLThm)
      -- * Primitive Re-Exports
    , module HaskHOL.Core.Kernel.Types
    , module HaskHOL.Core.Kernel.Terms
    ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel.Prims
import HaskHOL.Core.Kernel.Types
import HaskHOL.Core.Kernel.Terms

{-
  Used to quickly make an equality between two terms we know to be of the same
  type.  Not exposed to the user.
-}
safeMkEq :: HOLTerm -> HOLTerm -> HOLTerm
safeMkEq l = CombIn $ CombIn (tmEq $ typeOf l) l

{- 
  Unions two lists of terms, ordering the result modulo alpha-equivalence.  Not
  exposed to the user.
-}
termUnion :: [HOLTerm] -> [HOLTerm] -> [HOLTerm]
termUnion [] l2 = l2
termUnion l1 [] = l1
termUnion l1@(h1:t1) l2@(h2:t2) = 
    case alphaOrder h1 h2 of
      EQ -> h1 : termUnion t1 t2
      LT -> h1 : termUnion t1 l2
      _  -> h2 : termUnion l1 t2

{- 
  Removes a term from a term list, ordering the result modulo alpha-equivalence.
  Not exposed to the user.
-}
termRemove :: HOLTerm -> [HOLTerm] -> [HOLTerm]
termRemove _ [] = []
termRemove t l@(s:ss) =
    case alphaOrder t s of
      GT -> s : termRemove t ss
      EQ -> ss
      _  -> l

{- 
  Maps a function over a list of terms, termUnion-ing the result at each step.
  Roughly equivalent to a composition of nub and map that orders the result
  modulo alpha-equivalence.  Not exposed to the user
-}
termImage :: (HOLTerm -> HOLTerm) -> [HOLTerm] -> [HOLTerm]
termImage _ [] = []
termImage f (h:t) = termUnion [f h] $ termImage f t

{-
   HOL Light Theorem Primitives
-}
{-| 
  Destructs a theorem, returning its list of assumption terms and conclusion
  term.
-}
destThm :: HOLThm -> ([HOLTerm], HOLTerm)
destThm (ThmIn a c) = (a, c)

-- | Accessor for the hypotheses, or assumption terms, of a theorem.
hyp :: HOLThm -> [HOLTerm]
hyp (ThmIn a _) = a

-- | Accessor for the conclusion term of a theorem.
concl :: HOLThm -> HOLTerm
concl (ThmIn _ c) = c

{-
   HOL Light Primitive Inference Rules
-}

-- Basic Equality Rules

{-|@
     t    
-----------
 |- t = t
@

  Never fails.
-}
primREFL :: HOLTerm -> HOLThm
primREFL t = ThmIn [] $ safeMkEq t t

{-|@
 A1 |- t1 = t2   A2 |- t2 = t3
-------------------------------
       A1 U A2 |- t1 = t3     
@

  Fails with 'Left' in the following cases:
  
  * The middle terms are not alpha-equivalent.
  
  * One, or both, of the theorem conclusions is not an equation.
-}
primTRANS :: HOLThm -> HOLThm -> Either String HOLThm
primTRANS (ThmIn a1 (CombIn eql@(CombIn (ConstIn "=" _ Prim) _) m1))	
          (ThmIn a2 (CombIn (CombIn (ConstIn "=" _ Prim) m2) r))
    | m1 `aConv` m2 =
        Right . ThmIn (termUnion a1 a2) $ CombIn eql r
    | otherwise = Left "primTRANS: middle terms don't agree"	
primTRANS _ _ = Left "primTRANS: not both equations"

-- Basic Congruence Rules

{-|@
 A1 |- f = g   A2 |- x = y
---------------------------
    A1 U A2 |- f x = g y
@

  Fails with 'Left' in the following cases:
  
  * One, or both, of the theorem conclusions is not an equation.
  
  * The first theorem conclusion is not an equation of function terms.
  
  * The types of the function terms and argument terms do not agree.
-}
primMK_COMB :: HOLThm -> HOLThm -> Either String HOLThm	
primMK_COMB (ThmIn a1 (CombIn (CombIn (ConstIn "=" _ Prim) l1) r1))
            (ThmIn a2 (CombIn (CombIn (ConstIn "=" _ Prim) l2) r2)) =
    case typeOf l1 of
      (TyAppIn (TyPrim "fun" _) (ty:_:_))
          | typeOf l2 `tyAConv` ty ->
              Right . 
                ThmIn (termUnion a1 a2) . safeMkEq (CombIn l1 l2) $ CombIn r1 r2
          | otherwise -> Left "primMK_COMB: types do not agree"
      _ -> Left "primMK_COMB: not a function type"
primMK_COMB _ _ = Left "primMK_COMB: not both equations"

{-|@
          A |- t1 = t2
-------------------------------
 A |- (\\ x . t1) = (\\ x . t2)
@

  Fails with 'Left' in the following cases:
  
  * The term to bind is free in the assumption list of the theorem.
  
  * The conclusion of the theorem is not an equation.
-}
primABS :: HOLTerm -> HOLThm -> Either String HOLThm	
primABS v@VarIn{} (ThmIn a (CombIn (CombIn (ConstIn "=" _ Prim) l) r))
    | any (varFreeIn v) a = 
        Left "primABS: variable is free in assumptions"	
    | otherwise = 
        Right . ThmIn a . safeMkEq (AbsIn v l) $ AbsIn v r
primABS _ _ = Left "primABS: not an equation"

-- Beta Reduction
{-|@
        (\\ x . t[x]) x
-------------------------------
     |- (\\ x . t) x = t[x]
@

  Fails with 'Left' in the following cases:
  
  * The term is not a valid application.
  
  * The reduction is not a trivial one, i.e. the argument term is not equivalent
    to the bound variable.
-}
primBETA :: HOLTerm -> Either String HOLThm
primBETA tm@(CombIn (AbsIn bv bod) arg)
    | arg == bv = Right . ThmIn [] $ safeMkEq tm bod
    | otherwise = Left "primBETA_PRIM: not a trivial beta reduction"
primBETA _ = Left "primBETA_PRIM: not a valid application"

-- Deduction Rules
{-|@
     t
-----------
   t |- t
@

  Fails with 'Nothing' if the term is not a proposition.
-}
primASSUME :: HOLTerm -> Maybe HOLThm
primASSUME tm
    | typeOf tm == tyBool = Just $ ThmIn [tm] tm
    | otherwise = Nothing

{-|@
 A1 |- t1 = t2   A2 |- t1
----------------------------
      A1 U A2 |- t2
@

  Fails with 'Left' in the following cases:

  * The conclusion of the first theorem is not an equation.

  * The conclusion term of the second theorem and the left hand side of the 
    equation are not alpha-equivalent.
-}
primEQ_MP :: HOLThm -> HOLThm -> Either String HOLThm
primEQ_MP (ThmIn a1 (CombIn (CombIn (ConstIn "=" _ Prim) l) r)) (ThmIn a2 c)
    | l `aConv` c = Right $ ThmIn (termUnion a1 a2) r
    | otherwise = Left "primEQ_MP: terms do not agree"
primEQ_MP _ _ = Left "primEQ_MP: term is not an equation"

{-|@
       A |- p       B |- q       
----------------------------------
 (A - {q}) U (B - {p}) |- p \<=\> q
@

  Never fails.
-}
primDEDUCT_ANTISYM_RULE :: HOLThm -> HOLThm -> HOLThm
primDEDUCT_ANTISYM_RULE (ThmIn a p) (ThmIn b q) =
    ThmIn (termRemove q a `termUnion` termRemove p b) $ safeMkEq p q

-- Instantiation Rules
{-|@
 [(ty1, tv1), ..., (tyn, tvn)]   A |- t              
----------------------------------------
   A[ty1, ..., tyn/tv1, ..., tvn]
    |- t[ty1, ..., tyn/tv1, ..., tvn]
@

  Never fails.
-}
primINST_TYPE :: Inst a b => [(a, b)] -> HOLThm -> HOLThm
primINST_TYPE tyenv (ThmIn a t) = 
    let instFun = inst tyenv in
      ThmIn (termImage instFun a) $ instFun t

-- | A version of 'primINST_TYPE' that instantiates a theorem via 'instFull'.
primINST_TYPE_FULL :: SubstTrip -> HOLThm -> HOLThm
primINST_TYPE_FULL tyenv (ThmIn a t) =
    let instFun = instFull tyenv in
      ThmIn (termImage instFun a) $ instFun t

{-|@
 [(t1, x1), ..., (tn, xn)]   A |- t          
------------------------------------
   A[t1, ..., tn/x1, ..., xn]
    |- t[t1, ..., tn/x1, ..., xn]   
@

  Never fails.
-}
primINST :: HOLTermEnv -> HOLThm -> HOLThm
primINST env (ThmIn a t) = 
    let instFun = varSubst env in
      ThmIn (termImage instFun a) $ instFun t

{-
   HOL2P Primitive Inference Rules
-}

-- Type Congruence rules

{-|@
          A |- t1 = t2
-------------------------------
 A |- (\\\\ x . t1) = (\\\\ x . t2)
@

  Fails with 'Left' in the following cases:

  * The type to bind is not a small type variable. 

  * The conclusion of the theorem is not an equation.

  * The type to bind is free in the assumption list of the theorem. 
  
  * The type variable to bind is free in the conclusion of the theorem.
-}
primTYABS :: HOLType -> HOLThm -> Either String HOLThm
primTYABS tv@(TyVarIn True _) 
             (ThmIn a (CombIn (CombIn (ConstIn "=" _ Prim) l) r))
    | tv `notElem` typeVarsInTerms a =
        let fvs = frees l `union` frees r in
          if any (\ x -> tv `elem` tyVars (typeOf x)) fvs
          then Left "primTYABS: type variable is free in conclusion"
          else Right . ThmIn a . safeMkEq (TyAbsIn tv l) $ TyAbsIn tv r
    | otherwise =
        Left "primTYABS: type variable is free in assumptions"
primTYABS (TyVarIn True _) _ = 
    Left "primTYABS: conclusion not an equation"
primTYABS _ _ =
    Left "primTYABS: first argument not a small type variable"

{-|@
          A |- t1 = t2
-------------------------------
 A |- t1 [: ty1] = t2 [: ty2]
@

  Fails with 'Left' in the following cases:

  * The conclusion of the theorem is not an equation of terms of universal type.

  * The type arguments are not alpha-equivalent.

  * One, or both, of the type arguments is not small.
-}
primTYAPP2 :: HOLType -> HOLType -> HOLThm -> Either String HOLThm
primTYAPP2 ty1 ty2 (ThmIn a (CombIn (CombIn (ConstIn "=" _ Prim) l) r))
    | ty1 `tyAConv` ty2 = 
        case typeOf l of
          UTypeIn{} 
              | not $ isSmall ty1 ->
                  Left "primTYAPP2: ty1 not small"
              | not $ isSmall ty2 ->
                  Left "primTYAPP2: ty2 not small"
              | otherwise -> 
                  Right . ThmIn a . safeMkEq (TyCombIn l ty1) $ TyCombIn r ty2
          _ -> Left "primTYAPP2: terms not of universal type"
    | otherwise = 
        Left "primTYAPP2: type arguments not alpha-convertible"
primTYAPP2 _ _ _ = Left "primTYAPP2: conclusion not an equation"
    
{-|@
        A |- t1 = t2
----------------------------
 A |- t1 [: ty] = t2 [: ty]
@

  Fails with 'Nothing' if the conclusion of the theorem is not an equation.

  Note that 'primTYAPP' is equivalent to 'primTYAPP2' when the same type is
  applied to both sides, i.e. 

  @ primTYAPP ty === primTYAPP2 ty ty
  @
-}
primTYAPP :: HOLType -> HOLThm -> Maybe HOLThm
primTYAPP ty (ThmIn a (CombIn (CombIn (ConstIn "=" _ Prim) l) r)) = 
    Just . ThmIn a $ safeMkEq (TyCombIn l ty) (TyCombIn r ty)
primTYAPP _ _ = Nothing

-- Type Beta Reduction

{-|@
     (\\\\ ty . t[ty]) [: ty]    
---------------------------------
 |- (\\\\ ty . t[ty]) [: ty] = t
@

  Fails with 'Left' in the following cases:

  * The term is not a valid type application.

  * The reduction is not a trivial one, i.e. the argument type is not equivalent
    to the bound type variable.
-}
primTYBETA :: HOLTerm -> Either String HOLThm
primTYBETA tm@(TyCombIn (TyAbsIn tv bod) argt)
    | argt == tv = Right . ThmIn [] $ safeMkEq tm bod
    | otherwise = Left "primTYBETA: not a trivial type beta reduction"
primTYBETA _ = Left "primTYBETA: not a valid type application"

{-
   Stateless HOL Theory Extension Primitives
   Note that the following primitives are in HaskHOL.Core.State as per
   Stateless HOL:
   axioms, newAxiom, newBasicDefinition, newBasicTypeDefinition
-}

{-|
  Creates a new axiom theorem.  

  Note that, as discussed in the documentation for 'HOLThm', the introduction of
  axioms is not tracked until the stateful layer of the system is introduced so 
  be careful using this function.
-}
axiomThm :: HOLTerm -> HOLThm	
axiomThm = ThmIn []

{-|@
   c = t  
-----------
 |- c = t
@

  Creates a new defined constant given a term that equates a variable of the
  desired constant name and type to its desired definition.  The return value 
  is a pair of the new constant and its definitional theorem.  

  Note that internally the constant is tagged with its definitional term via the
  @Defined@ 'ConstTag'.

  Fails with 'Left' in the following cases:

  * The provided term is not an equation.

  * The provided term is not closed.

  * There are free type variables present in the definition that are not also in
    the desired type of the constant.
-} 
newDefinedConst :: HOLTerm -> Either String (HOLTerm, HOLThm)
newDefinedConst tm@(CombIn (CombIn (ConstIn "=" _ Prim) (VarIn cname ty)) r)
    | not (freesIn [] r) =
        Left "newDefinedConst: not closed"
    | not (subset (typeVarsInTerm r) (tyVars ty)) =
        Left "newDefinedConst: type vars not refelcted in const"
    | otherwise =        
        let c = ConstIn cname ty $ Defined tm
            dth = ThmIn [] $ safeMkEq c r in
          Right (c, dth)
newDefinedConst _ = Left "newDefinedConst: not an equation"

{-|@
                           |- p x:rep
-----------------------------------------------------------------
 (|- mk:rep->ty (dest:ty->rep a) = a, |- P r \<=\> dest(mk r) = r)
@

  Creates a new defined type constant that is defined as an inhabited subset
  of an existing type constant.  The return value is a pentuple that 
  collectively provides a bijection between the new type and the old type.

  The following four items are taken as input:

  * The name of the new type constant - @ty@ in the above sequent.

  * The name of the new term constant that will be used to make an instance of 
    the new type - @mk@ in the above sequent.

  * The name of the new term constant that will be used to destruct an instance
    of the new type - @dest@ in the above sequent.

  * A theorem proving that the desired subset is non-empty.  The conclusion of
    this theorem must take the form @p x@ where @p@ is the predicate that
    defines the subset and @x@ is a witness to inhabitation.

  The following items are returned as part of the resultant pentuple:

  * The new defined type operator.  These type operators carry their name,
    arity, and definitional theorem.  The arity, in this case, is inferred from
    the number of free type variables found in the predicate of the definitional
    theorem.

  * The new term constants, @mk@ and @dest@, as described above.  Note that 
    constants constructed in this manner are tagged with special instances of 
    'ConstTag', @MkAbstract@ and @DestAbstract@ accordingly, that carry the 
    name, arity, and definitional theorem of their related type constant.

  * The two theorems proving the bijection, as shown in the sequent above.
-}
newDefinedTypeOp :: String -> String -> String -> HOLThm -> 
                    Either String (TypeOp, HOLTerm, HOLTerm, HOLThm, HOLThm)
newDefinedTypeOp tyname absname repname dth'@(ThmIn [] (CombIn p x))
    | containsUType $ typeOf x =
        Left "newDefinedTypeOp: must not contain universal types"
    | not $ freesIn [] p =
        Left "newDefinedTypeOp: predicate is not closed"
    | otherwise = 
        let tys = sort (<=) (typeVarsInTerm p)
            arity = length tys
            atyop = TyDefined tyname arity dth'
            rty = typeOf x
            aty = TyAppIn atyop tys
            atm = VarIn "a" aty
            rtm = VarIn "r" rty
            absCon = ConstIn absname (TyAppIn tyOpFun [rty, aty]) $ 
                       MkAbstract tyname arity dth'
            repCon = ConstIn repname (TyAppIn tyOpFun [aty, rty]) $ 
                       DestAbstract tyname arity dth' in
          Right (atyop, absCon, repCon,
                 ThmIn [] (safeMkEq (CombIn absCon (CombIn repCon atm)) atm),
                 ThmIn [] (safeMkEq (CombIn p rtm) $ 
                             safeMkEq (CombIn repCon (CombIn absCon rtm)) rtm))
newDefinedTypeOp _ _ _ _ = Left "newDefinedTypeOp: poorly formed predicate"


-- Documentation copied from HaskHOL.Core.Prims

{-$ViewPatterns
  The primitive data types of HaskHOL are implemented using view patterns in
  order to simulate private data types:

  * Internal constructors are hidden to prevent manual construction of terms.

  * View constructors (those of 'HOLTypeView', 'HOLTermView', and 'HOLThmView') 
    are exposed to enable pattern matching. 

  * View patterns, as defined by instances of the 'view' function from the 
    @Viewable@ class, provide a conversion between the two sets of constructors.
-}
