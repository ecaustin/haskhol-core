{-# LANGUAGE PatternSynonyms #-}
{-|
  Module:    HaskHOL.Core.Basics.Nets
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines term nets, an efficient tree structure used for fast 
  lookups of values that match a given "pattern" term.  Typically term nets are
  used to store a collection of conversions or tactics to be used for rewriting.
  By associating these operations with the pattern that they are valid for, the
  rewrite process can quickly prune computations that will obviously fail.

  For more information see the "nets" module from John Harrison's HOL Light.
-}
module HaskHOL.Core.Basics.Nets
       ( Net
       , netEmpty
       , netMap
       , netEnter
       , netLookup
       , netMerge
       ) where

import HaskHOL.Core.Lib
import HaskHOL.Core.Kernel
import HaskHOL.Core.State.Monad

-- ordered, unique insertion for sets as lists
setInsert :: Ord a => a -> [a] -> [a]
setInsert a xs = tryd xs $ sinsert a xs
  where sinsert :: Ord a => a -> [a] -> Catch [a]
        sinsert x [] = return [x]
        sinsert x l@(h:t)
            | h == x = fail' "sinsert"
            | x < h = return (x:l)
            | otherwise = do t' <- sinsert x t
                             return (h:t')

-- ordered, unique merging of two sets
setMerge :: Ord a => [a] -> [a] -> [a]
setMerge [] l2 = l2
setMerge l1 [] = l1
setMerge l1@(h1:t1) l2@(h2:t2)
    | h1 == h2 = h1 : setMerge t1 t2
    | h1 < h2 = h1 : setMerge t1 l2
    | otherwise = h2 : setMerge l1 t2

-- The data type that defines a label for each node in a term net.
data TermLabel 
     = VNet             -- variables
     | LCNet Text Int -- local constants
     | CNet Text Int  -- constants
     | LNet Int         -- term abstraction
     | LTyAbs           -- type abstraction
     | LTyComb          -- type combination
     deriving (Eq, Ord, Show, Typeable)

deriveSafeCopy 0 'base ''TermLabel

{-|
  Internally, 'Net's are represented with a tree structure; each node has a list
  of labeled branches and a list of values.  The node labels are generated via
  the following guidelines:

  * Flattening of combinations favors the left hand side such that the head of 
    an application is looked at first.

  * If the head of an application is variable, the whole term is considered 
    variable.

  * Type abstractions and type combinations are effectively treated as local 
    constants, though they do have their own node lable representations to avoid
    any potential issues with user provided variable lists for 'enter'.

  * Matching is conservative, such that all matching values will be returned, 
    but some non-matching values may be returned.  For example, a pattern term 
    of the form @x \`op\` x@ will match any term of the form @a \`op\` b@ 
    regardless of the values of @a@ and @b@.
-}
data Net a = NetNode !(Map TermLabel (Net a)) [a] deriving (Show, Typeable)

--EvNote: GHC7.10 broke auto-deriving for SafeCopy
deriveSafeCopy 0 'base ''Net
{-
instance SafeCopy a => SafeCopy (Net a) where
    putCopy (NetNode m xs) = contain $ do p1 <- getSafePut
                                          p2 <- getSafePut
                                          p1 m
                                          p2 xs
                                          return ()
    getCopy = contain $ do g1 <- getSafeGet
                           g2 <- getSafeGet
                           return NetNode <*> g1 <*> g2
    version = 0
    kind = base
-}

-- | The empty 'Net'.
netEmpty :: Net a
netEmpty = NetNode mapEmpty []

-- | A version of 'map' for Nets.
netMap :: (a -> b) -> Net a -> Net b
netMap f (NetNode xs ys) =
    NetNode (mapMap (netMap f) xs) $ map f ys

{-
  Generates a net node label given a pattern term.  Differs from labelToLookup
  in that it accepts a list of variables to treat as local constants when
  generating the label.
-}
labelToStore :: [HOLTerm] -> HOLTerm -> (TermLabel, [HOLTerm])
labelToStore lconsts tm = 
    let (op, args) = revSplitList destComb tm in
      case op of
        (Const x _) -> (CNet x (length args), args)
        (Abs bv bod) -> 
            let bod' = if bv `elem` lconsts
                       then let v = unsafeGenVar $ typeOf bv in
                              try' $! varSubst [(bv, v)] bod
                       else bod in
              (LNet (length args), bod':args)
        (TyAbs _ t) -> (LTyAbs, [t])
        (TyComb t _) -> (LTyComb, [t])
        (Var x _) -> if op `elem` lconsts
                     then (LCNet x (length args), args)
                     else (VNet, [])
        _ -> error "labelToStore: stripComb broken"

{- 
  Used by enter in order to update a net.  Recursively generates node labels for
  the provided pattern using labelToStore.
-}
netUpdate :: Ord a => [HOLTerm] -> (a, [HOLTerm], Net a) -> Net a
netUpdate _ (b, [], NetNode edges tips) = 
    NetNode edges $ setInsert b tips
netUpdate lconsts (b, tm:rtms, NetNode edges tips) =
    let (label, ntms) = labelToStore lconsts tm
        (child, others) = tryd (netEmpty, edges) $ mapRemove label edges
        newChild = netUpdate lconsts (b, ntms++rtms, child) in
      NetNode (mapInsert label newChild others) tips

{-| 
  Inserts a new element, paired with a pattern term, into a provided net.  The 
  first argument is a list of variables that should be treated as local 
  constants, such that only patterns with those variables at the exact same 
  position will match.  See the documentation for 'Net' for more details.

  Never fails.
-}
netEnter :: Ord a => [HOLTerm] -> (HOLTerm, a) -> Net a -> Net a
netEnter lconsts (tm, b) net = netUpdate lconsts (b, [tm], net)

{-
  Generates a node label from a provided pattern term.  Differs from
  labelToStore in that no list of local constants to consider is given.
-}
labelForLookup :: HOLTerm -> (TermLabel, [HOLTerm])
labelForLookup tm =
    let (op, args) = revSplitList destComb tm in
      case op of
        (Const x _) -> (CNet x (length args), args)
        (Abs _ bod) -> (LNet (length args), bod:args)
        (TyAbs _ t) -> (LTyAbs, [t])
        (TyComb t _) -> (LTyComb, [t])
        (Var x _) -> (LCNet x (length args), args)
        _ -> error "labelForLookup: stripComb broken"

{-
  Traverses a Net following the labels generated from pattern terms via
  labelForLookup.  Returns a list of all values that satisfy the generated
  pattern.
-}
follow :: ([HOLTerm], Net a) -> [a]
follow ([], NetNode _ tips) = tips
follow (tm:rtms, NetNode edges _) = 
    let (label, ntms) = labelForLookup tm
        collection = case mapAssoc label edges of
                       Just child -> follow (ntms++rtms, child)
                       Nothing -> [] in
      if label == VNet then collection
      else case mapAssoc VNet edges of
             Just vn -> collection ++ follow (rtms, vn)
             Nothing -> collection

{-|
  Returns the list of all values stored in a term net that satisfy a provided
  pattern term.  See the documentation for 'Net' for more details.
-}
netLookup :: HOLTerm -> Net a -> [a]
netLookup tm net = follow ([tm], net)

{-|
  Merges two term nets together.  The values for the two nets are merged,
  maintaining order and uniqueness, with the term labels adjusted appropriately.
  The algorithm to do so is courtesy of Don Syme via John Harrison's
  implementation in HOL Light.
-}
netMerge :: Ord a => Net a -> Net a -> Net a
netMerge (NetNode l1 data1) (NetNode l2 data2) =
    NetNode (mapFoldrWithKey addNode (mapFoldrWithKey addNode mapEmpty l1) l2) $
      setMerge data1 data2
  where addNode :: Ord a => TermLabel -> Net a -> Map TermLabel (Net a) ->
                            Map TermLabel (Net a)
        addNode lab net l =
            case mapRemove lab l of
              Just (net', rest) ->
                  mapInsert lab (netMerge net net') rest
              Nothing -> mapInsert lab net l
