{-# LANGUAGE ScopedTypeVariables #-}
{-|
  Module:    HaskHOL.Core.Ext
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module exports HaskHOL's non-trivial extensions to the underlying HOL
  system, i.e. the compile time operations.  These operations are split into
  three categories:

  * Methods related to the Protect and Serve Mechanism for sealing and unsealing
    data against a provided theory context.

  * Methods related to quasi-quoting of 'HOLTerm's.  

  * Methods related to compile time extension and caching of theory contexts.
-}
module HaskHOL.Core.Ext
    ( -- * Protected Data Methods
       -- $Protect
      module HaskHOL.Core.Ext.Protected
      -- * Quasi-Quoter Methods
       -- $QQ
    , module HaskHOL.Core.Ext.QQ
      -- * Theory Extension Methods
    , templateProvers
    , extendTheory
      -- * Template Haskell Re-Exports
    , module Language.Haskell.TH {-|
        Re-exports 'Q', 'Dec', and 'Exp' for the purpose of writing type
        signatures external to this module.
      -}
    , module Language.Haskell.TH.Quote {-|
        Re-exports 'QuasiQuoter' for the purpose of writing type signatures
        external to this module.
      -}
    ) where

import HaskHOL.Core.Lib hiding (combine)
import HaskHOL.Core.State.Monad

import HaskHOL.Core.Ext.Protected
import HaskHOL.Core.Ext.QQ

import Data.Char (toUpper, toLower)

import Language.Haskell.TH (Q, Dec, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax  

import Prelude hiding (FilePath)
import Paths_haskhol_core
import Shelly
import System.FilePath (combine)

{-|
  The 'templateTypes' splice automatically generates the types and type 
  families associated with a theory extension.  It constructs the following:

  * A new empty data declaration associated with the new theory.

  * A type synonym for the application of the new data type.

  * A closed type family for constraining computations to be used only with
    contexts containing this checkpoint.
  
  The first argument is the context to be extended and the second argument is
  the name of the new theory checkpoint.
  For example,

  > templateTypes ctxtBase "Bool"

  will produce the following declarations:

  > data BoolThry
  > instance CtxtName BoolThry where
  >     ctxtName _ = "BoolCtxt"
  > type BoolType = ExtThry BoolThry BaseThry
  >
  > type family BoolContext a :: Bool where
  >     BoolContext BaseThry = False
  >     BoolContext (ExtThry a b) = (a == BoolThry) || (BoolContext b)
  >
  > type instance BoolThry == BoolThry = True

  Regrettably, Template Haskell cannot currently handle splicing type 
  equalities.  Thus, to provide a cleaner interface, it is recommended to
  manually construct another type family to be used as the main 'Constraint':

  > type family BoolCtxt a :: Constraint where
  >     BoolCtxt a = (BaseCtxt a, BoolContext a ~ True)

  Note that this new 'Constraint' can also be used to enforce the linearity of
  type theory contexts.
-}
templateTypes :: forall thry. CtxtName thry => TheoryPath thry -> String 
              -> Q [Dec]
templateTypes _ lbl =
        -- upper case label for everything else
    let upLbl = toUpper (head lbl) : tail lbl
        -- type of old theory
        oldThry = ConT . mkName $
                    let oldCtxt = ctxtName (undefined::thry)
                        oldType = take (length oldCtxt - 4) oldCtxt in
                      if oldType == "Base" then "BaseThry"
                      else oldType ++ "Type"
        -- general use type variables
        aName = mkName "a"
        aVar = VarT aName
        bVar = VarT $ mkName "b"
-- build data types
        dataName = mkName $ upLbl ++ "Thry"
        dataType = ConT dataName
        -- splices: instance CtxtName _Thry where ...
        instDec = InstanceD [] (AppT (ConT ''CtxtName) (ConT dataName))
                    [FunD 'ctxtName [Clause [WildP] 
                      (NormalB (LitE (StringL $ upLbl ++ "Ctxt"))) []]]
        dataDec = DataD [] dataName [] [] []
        tyName = mkName $ upLbl ++ "Type"
        newThry = ConT ''ExtThry `AppT` dataType `AppT` oldThry
        -- splices: type _Type = ExtThry _Thry oldThry
        tyDec = TySynD tyName [] newThry
-- build class and instances
        clsName = mkName $ upLbl ++ "Context"
        -- splices: type instance _Context BaseThry = False
        famBase =TySynEqn [ConT ''BaseThry] $ PromotedT 'False
        famCond = AppT (AppT (ConT ''(||))
                        (AppT (ConT clsName) bVar))
                   (AppT (AppT (ConT ''(==)) aVar) (ConT dataName))     
        -- splices: type instance _Context (ExtThry a b) = 
        --              (a == _Thry) || (_Context b)     
        famInd = TySynEqn [AppT (AppT (ConT ''ExtThry) aVar) bVar] famCond
        -- splices: type family _Context a :: Bool
        fam = ClosedTypeFamilyD clsName [PlainTV aName] (Just $ ConT ''Bool)
                [famBase, famInd]
-- class wrapper
        -- splices: type instance _Thry == _Thry = True
        clsEq = TySynInstD ''(==) . 
                  TySynEqn [ConT dataName, ConT dataName] $ PromotedT 'True in
          return [ dataDec, tyDec, instDec               -- types
                 , fam
                 , clsEq
                 ]

{-|
  The 'templateProvers' splice automatically generates the 'QuasiQuoter'
  associated with a theory extension.  The provided
  name should be the theory context checkpoint built with the 'extendTheory' 
  splice.

  For example,

  > templateProvers 'ctxtBool

  will produce the following declarations:

  > bool :: QuasiQuoter
  > bool = baseQuoter ctxtBool
-}
templateProvers :: Name -> Q [Dec]
templateProvers ctxName =
-- build QuasiQuoter
    let upLbl = drop 4 $ nameBase ctxName
        lowLbl = toLower (head upLbl) : tail upLbl
        --tyName = mkName $ upLbl ++ "Type"
        qqName = mkName lowLbl
        -- splices: _ :: QuasiQuoter
        qqTySig = SigD qqName $ ConT ''QuasiQuoter
        -- splices: _ = baseQuoter ctxt_
        qqDec = ValD (VarP qqName) (NormalB $ 
                  VarE 'baseQuoter `AppE` VarE ctxName) [] in
      return [ qqTySig, qqDec ]

-- Documentation copied from sub-modules

{-$Protect
  The basic goal behind the Protect and Serve mechanism is to recapture some of
  the efficiency lost as a result of moving from an impure, interpretted host 
  language to a pure, compiled one.  We do this by forcing the evaluation of 
  large computations, usually proofs, such that they are only run once. To
  maintain soundness of our proof system, we must track what information
  was used to force the computation and guarantee that information is present
  in all cases where this new value is to be used.  This is the purpose of the
  @Protected@ class and the 'liftProtectedExp' and 'liftProtected' methods.
-}

{-$QQ
  Quasi-quoting provides a way to parse 'HOLTerm's at compile time safely.
  Just as with proofs, we seal these terms against the theory context used to
  parse them with 'protect' and 'serve' to preserve soundness.  See the
  documentation for 'base' for a brief discussion on when quasi-quoting should
  be used vs. 'toHTm'.
-}

{-|
  The 'extendTheory' splice acts as a compile-time wrapper to 'runHOL', used for
  the purpose of creating new theory contexts.  It takes three arguments:

  * The theory to be extended.

  * The base name of the new theory to be created.

  * The 'HOL' computation that will perform the extension.

  For example:

  > extendTheory ctxtBase "Bool" $ ...

  Additionally, 'extendTheory' calls the 'templateTypes' splice to create the
  type class wizardy associated with theory contexts and creates an
  appropriately typed wrapper for 'mkTheoryPath', e.g.:

  > ctxtBool :: TheoryPath BoolType
  > ctxtBool = mkTheoryPath
-}
extendTheory :: CtxtName thry => TheoryPath thry -> String -> HOL Theory thry ()
             -> Q [Dec]
extendTheory old new ld =
       -- run the load function
    do runIO $
         do dir <- getDataDir
            let new' = dir `combine` (new ++ "Ctxt")
            shelly $ 
              do cond <- test_d . fromText $ pack new'
                 when cond . echo . pack $
                   "Note:  Using existing theory context for " ++ new
                 unless cond . liftIO $
                   runHOL (ld >> checkpointProofs) old new'
       -- splices: ctxt_ :: TheoryPath _Type
       --          ctxt_ = mkTheoryPath
       let cname = mkName $ "ctxt" ++ new
           ctype = ConT . mkName $ new ++ "Type"
           tySig = SigD cname (ConT ''TheoryPath `AppT` ctype)
           def = ValD (VarP cname) (NormalB (VarE 'mkTheoryPath)) []
       tyDefs <- templateTypes old new
       return $! tyDefs ++ [tySig, def]
