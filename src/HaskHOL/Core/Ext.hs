{-# LANGUAGE TemplateHaskell #-}

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
    , extendCtxt -- :: Typeable thry => HOLContext thry -> 
                 --    HOL cls thry () -> Name -> String -> Q [Dec]
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

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel hiding (typeOf)
import HaskHOL.Core.State

import HaskHOL.Core.Ext.Protected
import HaskHOL.Core.Ext.QQ

import Data.Char (toUpper, toLower)
import Data.Typeable (typeOf, typeRepArgs, TypeRep)

import Language.Haskell.TH (Q, Dec, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax

{-|
  Extends a theory by evaluating a provided computation, returning a list of
  declarations containing:

  * A new empty data declaration associated with the new theory.

  * A new type class associated with the new theory to be used with
    @DerivedCtxt@ along with the appropriate instances.

  * The context value for the new theory.

  * A class constraint alias that can be safely exported for use in type
    signatures external to the library where it was defined.

  * A quasiquoter for the new theory.

  * A compile-time proof function for the new theory.

  For example:

  > extendCtxt ctxtBase loadBoolLib "bool"

  will produce the following code

  > data BoolThry deriving Typeable
  > type BoolType = ExtThry BoolThry BaseThry
  >
  > class BaseCtxt a => BoolContext a
  > instance BaseCtxt b => BoolContext (ExtThry BoolThry b)
  > instance BoolContext b => BoolContext (ExtThry a b)
  > 
  > class BoolContext a => BoolCtxt a
  > instance BoolContext a => BoolCtxt a
  >
  > ctxtBool :: HOLContext BoolType
  > ctxtBool = ...
  >
  > bool :: QuasiQuoter
  > bool = baseQuoter ctxtBool
  >
  > proveBool :: String -> HOL Proof BoolType HOLThm -> Q [Dec]
  > proveBool = proveCompileTime ctxtBool
  >
  > proveBoolMany :: [String] -> HOL Proof BoolType [HOLThm] -> Q [Dec]
  > proveBoolMany = proveCompileTimeMany ctxtBool
-}             
extendCtxt :: Typeable thry =>
              HOLContext thry -> HOL cls thry () -> String -> Q [Dec]
extendCtxt ctx ld lbl =
        -- lower case label for quasiquoter    
    let lowLbl = toLower (head lbl) : tail lbl
        -- upper case label for everything else
        upLbl = toUpper (head lbl) : tail lbl
        -- type of old theory
        oldThry = buildOldThry . head . typeRepArgs $ typeOf ctx
        -- general use type variables
        aName = mkName "a"
        aVar = VarT aName
        bVar = VarT $ mkName "b"
-- build data types
        dataName = mkName $ upLbl ++ "Thry"
        dataType = ConT dataName
        dataDec = DataD [] dataName [] [] [''Typeable]
        tyName = mkName $ upLbl ++ "Type"
        newThry = extThry `AppT` dataType `AppT` oldThry
        tyDec = TySynD tyName [] newThry
-- build class and instances
        clsName = mkName $ upLbl ++ "Context"
        oldThryName = let oldt = stripList (\ x -> case x of
                                                     AppT l r -> Just (l, r)
                                                     _ -> Nothing ) oldThry in
                        case oldt of
                          (ConT x:[]) -> show x
                          (_:ConT x:_) -> show x
                          _ -> error "extendCtxt: bad theory type."
        -- ConT XThry ---> XCtxt
        oldClsName = mkName $ take (length oldThryName - 4) oldThryName ++ 
                              "Ctxt"
        clsCon = ConT clsName
        clsDec = ClassD [ClassP oldClsName [aVar]] clsName [PlainTV aName] [] []
        clsIn1Dec = InstanceD [ClassP oldClsName [bVar]]
                      (clsCon `AppT` (extThry `AppT` dataType `AppT` bVar)) []
        clsIn2Dec = InstanceD [ClassP clsName [bVar]]
                      (clsCon `AppT` (extThry `AppT` aVar `AppT` bVar)) []
-- class wrapper
        clsName' = mkName $ upLbl ++ "Ctxt"
        clsCon' = ConT clsName'
        clsDec' = ClassD [ClassP clsName [aVar]] clsName' [PlainTV aName] [] []
        clsInDec' = InstanceD [ClassP clsName [aVar]] (clsCon' `AppT` aVar) []
-- build context type; we build value later
        ctxtName = mkName $ "ctxt" ++ upLbl
        ctxtTySig = SigD ctxtName $ ConT ''HOLContext `AppT` newThry
-- build QuasiQuoter
        qqName = mkName lowLbl
        qqTySig = SigD qqName $ ConT ''QuasiQuoter
        qqDec = ValD (VarP qqName) (NormalB $ 
                  VarE 'baseQuoter `AppE` VarE ctxtName) []
-- build provers
        -- Q [Dec]
        qdecType = ConT ''Q `AppT` (ListT `AppT` ConT ''Dec)
        cont b = if b then AppT ListT else id
        name b = mkName $ "prove" ++ upLbl ++ if b then "Many" else ""
        proveName b = if b then 'proveCompileTimeMany 
                           else 'proveCompileTime
        -- HOL Proof newThry HOLThm
        holType b = ConT ''HOL `AppT` ConT ''Proof `AppT` 
                    newThry `AppT` cont b (ConT ''HOLThm)
        proverTySig b = SigD (name b) $
                        ArrowT `AppT` cont b (ConT ''String) `AppT`
                        (ArrowT `AppT` holType b `AppT` qdecType)
        proveDec b = ValD (VarP $ name b) (NormalB $ 
                     VarE (proveName b) `AppE` VarE ctxtName) [] in
-- build values
      do lctx <- lift =<< runIO (execHOLCtxt ld ctx)
         let ctxtDec = ValD (VarP ctxtName) (NormalB lctx) []
         return [ dataDec, tyDec               -- types
                , clsDec, clsIn1Dec, clsIn2Dec -- class and instances
                , clsDec', clsInDec'           -- class alias
                , ctxtTySig, ctxtDec           -- context value
                , qqTySig, qqDec               -- quasiquoter
                , proverTySig False, proveDec False -- provers
                , proverTySig True, proveDec True
                ]
         

  where extThry :: Type
        extThry = ConT ''ExtThry
        
        buildOldThry :: TypeRep -> Type
        buildOldThry ty = 
            case typeRepArgs ty of
              [] -> ConT . mkName $ show ty
              ts -> let ts' = map buildOldThry ts in
                      foldl1 (\ acc x -> extThry `AppT` acc `AppT` x) ts' 

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
