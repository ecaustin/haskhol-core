{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, 
             StandaloneDeriving, TemplateHaskell #-}

{-|
  Module:    HaskHOL.Core.Lib
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines or re-exports common utility functions, type classes, 
  and auxilliary data types used in HaskHOL.  The following conventions hold 
  true:
  * Where possible, we favor re-exporting common functions rather than
    redefining them. 
  * We favor re-exporting individual functions rather entire modules to reduce
    the number of items in our utility library.
  * We default to the names of functions commonly used by Haskell libraries,
    however, if there's a different name for a function in HOL systems we
    include an alias for it.  For example, 'iComb' and 'id'.

  Note that none of the functions in this module depend on data types 
  introduced by HaskHOL.  Utility functions that do have such a dependence are 
  found in the "HaskHOL.Core.Basics" module.
-}
module HaskHOL.Core.Lib
    ( -- * Function Combinators
      iComb   -- :: a -> a
    , kComb   -- :: a -> b -> a
    , cComb   -- :: (a -> b -> c) -> b -> a -> c
    , wComb   -- :: (a -> a -> b) -> a -> b
    , ffComb  -- :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
    , ffCombM -- :: Monad m => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
    , liftM1  -- :: Monad m => (a -> b -> m c) -> m a -> b -> m c
      -- * Basic Operations on Pairs
    , swap     -- :: (a, b) -> (b, a)
    , pairMap  -- :: (a -> b) -> (a, a) -> (b, b)
    , pairMapM -- :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
    , first    -- :: (a -> c) -> (a, b) -> (c, b)
    , firstM   -- :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
    , second   -- :: (b -> c) -> (a, b) -> (a, c)
    , secondM  -- :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
      -- * Basic Operations on Lists
    , tryHead      -- :: [a] -> Maybe a
    , tryTail      -- :: [a] -> Maybe a
    , tryInit      -- :: [a] -> Maybe [a]
    , butLast      -- :: [a] -> Maybe [a]
    , tryLast      -- :: [a] -> Maybe a
    , tryIndex     -- :: [a] -> Int -> Maybe a
    , el           -- :: Int -> [a] -> Maybe a
    , rev          -- :: [a] -> [a]
      -- * Basic Operations on Association Lists
    , assoc      -- :: Eq a => a -> [(a, b)] -> Maybe b
    , revLookup  -- :: Eq a => a -> [(b, a)] -> Maybe b
    , revAssoc   -- :: Eq a => a -> [(b, a)] -> Maybe b
    , assocd     -- :: Eq a => a -> [(a, b)] -> b -> b
    , lookupd    -- :: Eq a => a -> [(a, b)] -> b -> b
    , revLookupd -- :: Eq a => a -> [(b, a)] -> b -> b
    , revAssocd  -- :: Eq a => a -> [(b, a)] -> b -> b
      -- * Methods for Error Handling
    , can        -- :: (Alternative m, Monad m) => (a -> m b) -> a -> m Bool
    , canNot     -- :: (Alternative m, Monad m) => (a -> m b) -> a -> m Bool
    , check      -- :: (a -> Bool) -> a -> Maybe a
    , note       -- :: a -> Maybe b -> Either a b
    , hush       -- :: Either a b -> Maybe b
    , fromRight  -- :: Either err a -> a
    , fromRightM -- :: MonadPlus m => Either err a -> m a
    , fromJustM  -- :: MonadPlus m => Maybe a -> m a
    , LiftOption(..)
    , Note(..)
      -- * Methods for Function Repetition
    , funpow  -- :: Int -> (a -> a) -> a -> a
    , funpowM -- :: Monad m => Int -> (a -> m a) -> a -> m a
    , repeatM -- :: (Alternative M, Monad m) => (a -> m a) -> a -> m a
    , map2    -- :: (a -> b -> c) -> [a] -> [b] -> Maybe c
    , map2M   -- :: (Monad m, MonadPlus m) => 
              --    (a -> b -> m c) -> [a] -> [b] -> m c
    , doList  -- :: Monad m => (a -> m b) -> [a] -> m ()
    , allpairs -- :: (a -> b -> c) -> [a] -> [b] -> [c]
      -- * Methods for List Iteration
    , itlist     -- :: (a -> b -> b) -> [a] -> b -> b
    , itlistM    -- :: (F.Foldable t, Monad m) =>
                 --    (a -> b -> m b) -> t a -> b -> m b
    , foldrM     -- :: (F.Foldable t, Monad m) => 
                 --    (a -> b -> m b) -> b -> t a -> m b
    , revItlist  -- :: (a -> b -> b) -> [a] -> b -> b
    , foldlM     -- :: (F.Foldable t, Monad m) => 
                 --    (a -> b -> m b) -> a -> t b -> m a
    , tryFoldr1  -- :: (a -> a -> a) -> [a] -> Maybe a
    , endItlist  -- :: (a -> a -> a) -> [a] -> Maybe a
    , foldr1M    -- :: (Monad m, MonadPlus m) => (a -> a -> m a) -> [a] -> m a
    , foldr2     -- :: (a -> b -> c -> c) -> c -> [a] -> [b] -> Maybe c
    , itlist2    -- :: (a -> b -> c -> c) -> [a] -> [b] -> c -> Maybe c
    , foldr2M    -- :: (Monad m, MonadPlus m) => 
                 --    (a -> b -> c -> m c) -> c -> [a] -> [b] -> m c
    , foldl2     -- :: (c -> a -> b -> c) -> c -> [a] -> [b] -> Maybe c
    , revItlist2 -- :: (b -> b -> c -> c) -> [a] -> [b] -> c -> Maybe c
    , foldl2M    -- :: (Monad m, MonadPlus m) => 
                 --    (c -> a -> b -> m c) -> c -> [b] -> m c
      -- * Methods for Sorting and Merging Lists
    , sort      -- :: Eq a => (a -> a -> Bool) -> [a] -> [a]
    , sortBy    -- :: (a -> a -> Ordering) -> [a] -> [a]
    , merge     -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    , mergesort -- :: forall a. (a -> a -> Bool) -> [a] -> [a]
      -- * Methods for Splitting and Stripping Binary Terms
    , splitList     -- :: (b -> Maybe (a, b)) -> b -> ([a], b)
    , splitListM    -- :: (Alternative m, Monad m) => 
                    --    (b -> m (a, b)) -> b -> m ([a], b)
    , revSplitList  -- :: (a -> Maybe (a, a)) -> a -> (a, [a])
    , revSplitListM -- :: (Alternative m, Monad m) => 
                    --    (b -> m (b, b)) -> b -> m (b, [b])
    , nsplit        -- :: (a -> Maybe (a, a)) -> [b] -> a -> Maybe ([a], a)
    , nsplitM       -- :: Monad m => (b -> m (b, b)) -> [c] -> b -> m ([b], b)
    , stripList     -- :: (a -> Maybe (a, a)) -> a -> [a]
    , stripListM    -- :: (Alternative m, Monad m) => 
                    --    (a -> m (a, a)) -> a -> m [a]
      -- * Methods for Searching and Manipulating Lists
    , forall      -- :: (a -> Bool) -> [a] -> Bool
    , forall2     -- :: (a -> b -> Bool) -> [a] -> [b] -> Maybe Bool
    , exists      -- :: (a -> Bool) -> [a] -> Bool
    , partition   -- :: (a -> Bool) -> [a] -> ([a], [a])
    , mapFilter   -- :: (a -> Maybe b) -> [a] -> [b]
    , mapFilterM  -- :: (Alternative m, Monad m) => (a -> m b) -> [a] -> m [b]
    , find        -- :: (a -> Bool) -> [a] -> Maybe a
    , findM       -- :: (Monad m, MonadPlus m) => (a -> m Bool) -> [a] -> m a
    , tryFind     -- :: (Monad m, MonadPlus m) => (a -> m b) -> [a] -> m b
    , flat        -- :: [[a]] -> [a]
    , dropWhileEnd -- :: (a -> Bool) -> [a] -> [a]
    , remove      -- :: (a -> Bool) -> [a] -> Maybe (a, [a])
    , trySplitAt  -- :: Int -> [a] -> Maybe ([a], [a])
    , chopList    -- :: Int -> [a] -> Maybe ([a], [a])
    , elemIndex   -- :: Eq a => a -> [a] -> Maybe Int
    , index       -- :: Eq a => a -> [a] -> Maybe Int
    , stripPrefix -- :: Eq a => [a] -> [a] -> Maybe [a]
    , uniq        -- :: Eq a => [a] -> [a]
    , shareOut    -- :: [[a]] -> [b] -> Maybe [[b]]
      -- * Set Operations on Lists
    , mem       -- :: Eq a => a -> [a] -> Bool
    , insert    -- :: Eq a => a -> [a] -> [a]
    , insertMap -- :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
    , union     -- :: Eq a => [a] -> [a] -> [a]
    , unions    -- :: Eq a => [[a]] -> [a]
    , intersect -- :: Eq a => [a] -> [a] -> [a]
    , delete    -- :: Eq a => a -> [a] -> [a]
    , (\\)      -- :: Eq a => [a] -> [a] -> [a]
    , subset    -- :: Eq a => [a] -> [a] -> Bool
    , setEq     -- :: Eq a => [a] -> [a] -> Bool
    , setify    -- :: Eq a => [a] -> [a]
    , nub       -- :: Eq a => [a] -> [a]
      -- * Set Operations Parameterized by Predicate
    , mem'      -- :: (a -> a -> Bool) -> a -> [a] -> Bool
    , insert'   -- :: (a -> a -> Bool) -> a -> [a] -> [a]
    , union'    -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    , unions'   -- :: (a -> a -> Bool) -> [[a]] -> [a]
    , subtract' -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    , group'    -- :: (a -> a -> Bool) -> [a] -> [[a]]
    , uniq'     -- :: Eq a => (a -> a -> Bool) -> [a] -> [a]
    , setify'   -- :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [a]
      -- * Operations on \"Num\" Types
      -- $NumAliases
    , num0        -- :: Integer
    , num1        -- :: Integer
    , num2        -- :: Integer
    , num10       -- :: Integer
    , pow2        -- :: Integer -> Integer
    , pow10       -- :: Integer -> Integer
    , numdom      -- :: Real a => a -> Rational
    , numerator   -- :: Rational -> Integer
    , denominator -- :: Rational -> Integer
    , gcdNum      -- :: Integer -> Integer -> Integer
    , lcmNum      -- :: Integer -> Integer -> Integer
    , numOfString -- :: (Eq a, Num a) => String -> Maybe a
      -- * Classes for Common \"Language\" Operations
      -- $LangClasses
    , Lang(..)
    , LangSeq(..)
      -- * Miscellaneous Re-exported Libraries
    , module HaskHOL.Core.Lib.Lift {-|
        Re-exports 'deriveLift', 'deriveLiftMany', and 'Lift' to be used with
        our extensible state mechanisms.
      -}
    , module Control.Applicative {-| 
        Re-exports 'Applicative', 'Alternative', and the utility functions for
        use with the 'HOL' monad.
      -}
    , module Control.DeepSeq {-|
        Re-exports the entirety of the library, but currently only 'NFData' is
        used.  Necessary for using the "Criterion" benchmarking library.
      -}
    , module Control.Monad {-|
        Re-exports the entirety of the library for use with the 'HOL' monad.
      -}
    , module Data.Maybe {-|
        Re-exports the entirety of the library.  Used primarily to make
        interacting with primitive rules easier at later points in the system.
      -}
    , module Data.Either {-|
        Re-exports the entirety of the library.  Used primarily to make
        interacting with primitive rules easier at later points in the system.
      -}
    , module Data.Typeable {-|
        Re-exports the 'Typeable' class name for use in deriving clauses.
      -}
    , module Text.ParserCombinators.Parsec.Expr {-|
        Re-exports the entirety of the library.  Used primarily for its 'Assoc'
        data type, but also contains a number of primitives used by the parser.
      -}
    ) where

import HaskHOL.Core.Lib.Lift

-- Libraries re-exported in their entirety, except for applicative
import Control.Applicative hiding (Const, WrappedMonad, WrappedArrow, ZipList)
import Control.DeepSeq
import Control.Monad
import Data.Maybe
import Data.Either
import Data.Typeable (Typeable)
import Text.ParserCombinators.Parsec.Expr

-- Libraries containing Re-exports
import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Ratio as R
import qualified Data.Tuple as T

-- Libraries containing utility functions used, but not exported directly
import Numeric (readInt, readHex, readDec)
import Data.Char (digitToInt)

-- combinators

-- | The I combinator.  An alias for 'id'.
{-# INLINEABLE iComb #-}
iComb :: a -> a
iComb = id

-- | The K combinator.  An alias for 'const'.
{-# INLINEABLE kComb #-}
kComb :: a -> b -> a
kComb = const

-- | The C combinator.  An alias for 'flip'.
{-# INLINEABLE cComb #-}
cComb :: (a -> b -> c) -> b -> a -> c
cComb = flip

{-| 
  The W combinator.  Takes a function of arity 2 and applies a single argument
  to it twice.
-}
{-# INLINEABLE wComb #-}
wComb :: (a -> a -> b) -> a -> b
wComb f x = f x x

-- | The FF combinator.  An alias for the arrow combinator 'A.***'.  
{-# INLINEABLE ffComb #-}
ffComb :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
ffComb = (A.***)

{-| 
  The monadic version of the FF combinator.  An alias for the arrow combinator
  'A.***' lifted for 'A.Kleisli' arrows.
-}
{-# INLINEABLE ffCombM #-}
ffCombM :: Monad m => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
ffCombM f g = A.runKleisli $ A.Kleisli f A.*** A.Kleisli g

{-|
  Promotes a function to a monad, but only for its first argument, i.e.
  
  > liftM1 f a b === flip f b =<< a
-}
{-# INLINEABLE liftM1 #-}
liftM1 :: Monad m => (a -> b -> m c) -> m a -> b -> m c
liftM1 f a b = flip f b =<< a

-- pair basics

-- | Swaps the order of a pair.  A re-export of 'T.swap'.
{-# INLINEABLE swap #-}
swap :: (a, b) -> (b, a)
swap = T.swap

-- | Applies a function to both elements of a pair using the 'A.***' operator.
{-# INLINEABLE pairMap #-}
pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f = f A.*** f

-- | The monadic version of 'pairMap'.
{-# INLINEABLE pairMapM #-}
pairMapM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
pairMapM f = f `ffCombM` f

{-|
  Applies a function only to the first element of a pair.  A re-export of 
  'A.first'.
-}
{-# INLINEABLE first #-}
first :: (a -> c) -> (a, b) -> (c, b)
first = A.first

-- | A monadic version of 'first' lifted for 'A.Kleisli' arrows.
{-# INLINEABLE firstM #-}
firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM = A.runKleisli . A.first . A.Kleisli

{-| 
  Applies a function only to the second element of a pair.  A re-export of 
  'A.second'.
-}
{-# INLINEABLE second #-}
second :: (b -> c) -> (a, b) -> (a, c)
second = A.second

-- | A monadic version of 'second' lifted for 'A.Kleisli' arrows.
{-# INLINEABLE secondM #-}
secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM = A.runKleisli . A.second . A.Kleisli

-- list basics
{-| 
  A safe version of 'head'.  Fails with 'Nothing' when trying to take the head
  of an empty list.
-}
tryHead :: [a] -> Maybe a
tryHead (x:_) = Just x
tryHead _ = Nothing

{-| 
  A safe version of 'tail'.  Fails with 'Nothing' when trying to take the tail
  of an empty list.
-}
tryTail :: [a] -> Maybe [a]
tryTail (_:t) = Just t
tryTail _ = Nothing

{-|
  A safe version of 'init'.  Fails with 'Nothing' when trying to drop the last
  element of an empty list.
-}
tryInit :: [a] -> Maybe [a]
tryInit (_:[]) = Just []
tryInit (x:xs) = do xs' <- tryInit xs
                    return (x:xs')
tryInit _ = Nothing

-- | An alias to 'tryInit' for HOL users more familiar with this name.
{-# INLINEABLE butLast #-}
butLast :: [a] -> Maybe [a]
butLast = tryInit

{-| 
  A safe version of 'last'.  Fails with 'Nothing' when trying to take the last
  element of an empty list.
-}
tryLast :: [a] -> Maybe a
tryLast (x:[]) = Just x
tryLast (_:xs) = tryLast xs
tryLast _ = Nothing

{-| 
  A safe version of 'index'.  Fails with 'Nothing' if the selected index does
  not exist.
-}
tryIndex :: [a] -> Int -> Maybe a
tryIndex xs n
    | n >= 0 = tryHead $ drop n xs
    | otherwise = Nothing

{-|
  An alias to 'tryIndex' for HOL users more familiar with this name.  Note that
  the order of the arguments is flipped.
-}
{-# INLINEABLE el #-}
el :: Int -> [a] -> Maybe a
el = flip tryIndex

-- | An alias to 'reverse' for HOL users more familiar with this name.
{-# INLINEABLE rev #-}
rev :: [a] -> [a]
rev = reverse

-- association lists
-- | An alias to 'lookup' for HOL users more familiar with this name.
{-# INLINEABLE assoc #-}
assoc :: Eq a => a -> [(a, b)] -> Maybe b
assoc = lookup

{-| 
  A version of 'lookup' where the search is performed against the second element
  of the pair instead of the first.  Still fails with 'Nothing' if the desired
  value is not found.
-}
revLookup :: Eq a => a -> [(b, a)] -> Maybe b
revLookup _ [] = Nothing
revLookup x ((f, s):as)
  | x == s = Just f
  | otherwise = revLookup x as

-- | An alias to 'revLookup' for HOL users who are more familiar with this name.
{-# INLINEABLE revAssoc #-}
revAssoc :: Eq a => a -> [(b, a)] -> Maybe b
revAssoc = revLookup

-- | A version of 'lookup' that defaults to a provided value rather than fail.
lookupd :: Eq a => a -> [(a, b)] -> b -> b
lookupd x xs b = fromMaybe b $ lookup x xs

-- | An alias to 'lookupd' for HOL users who are more familiar with this name.
{-# INLINEABLE assocd #-}
assocd :: Eq a => a -> [(a, b)] -> b -> b
assocd = lookupd

{-| 
  A version of 'revLookup' that defaults to a provided value rather than fail.
-}
revLookupd :: Eq a => a -> [(b, a)] -> b -> b
revLookupd x xs b = fromMaybe b $ revLookup x xs

{-| 
  An alias to 'revLookupd' for HOL users who are more familiar with this name.
-}
{-# INLINEABLE revAssocd #-}
revAssocd :: Eq a => a -> [(b, a)] -> b -> b
revAssocd = revLookupd

-- error handling and checking
{-| 
  Returns a boolean value indicating whether a monadic computation succeeds or
  fails.  The '<|>' operator is used for branching.
-}
can :: (Alternative m, Monad m) => (a -> m b) -> a -> m Bool
can f x = (f x >> return True) <|> return False

{-| 
  The opposite of 'can'.  Functionally equivalent to 

  > \ f -> liftM not . can f
-}
canNot :: (Alternative m, Monad m) => (a -> m b) -> a -> m Bool
canNot f x = (f x >> return False) <|> return True

{-| 
  Checks if a predicate succeeds for a provided value, returning that value
  guarded by a 'Maybe' type if so.
-} 
check :: (a -> Bool) -> a -> Maybe a
check p x
  | p x = Just x
  | otherwise = Nothing

-- | Takes a default error value to convert a 'Maybe' type to an 'Either' type.
note :: a -> Maybe b -> Either a b
note l Nothing = Left l
note _ (Just r) = Right r

{-| 
  Suppresses the error value of an 'Either' type to convert it to a 'Maybe' 
  type.
-}
hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right r) = Just r

{-|
  An analogue of 'fromJust' for the 'Either' type.  Fails with 'error' when
  provided a 'Left' value, so take care only to use it in cases where you know 
  you are working with a 'Right' value or are catching exceptions. 
-}
fromRight :: Either err a -> a
fromRight (Right res) = res
fromRight _ = error "fromRight"

{-|
  A version of 'fromRight' that maps 'Left' values to 'mzero' rather than
  failing.
-}
fromRightM :: MonadPlus m => Either err a -> m a
fromRightM (Right res) = return res
fromRightM _ = mzero

{-|
  A version of 'fromJust' that maps 'Nothing' values to 'mzero' rather than
  failing.
-}
fromJustM :: MonadPlus m => Maybe a -> m a
fromJustM (Just res) = return res
fromJustM _ = mzero

infixr 1 #<<
infixl 4 <#>
{-|
  The 'LiftOption' class provides an infix operator to more cleanly apply the
  'fromJustM' and 'fromRightM' methods to a value that will be passed to a
  monadic computation.
-}
class Monad m => LiftOption l m where
    {-| 
      Used to lift an option value, i.e. 'Maybe' or 'Either', so that it can be
      passed as an argument to a monadic computation.
    -}
    liftO :: l a -> m a

    -- | A version of '=<<' composed with 'liftO' for the right argument.
    (#<<) :: (a -> m b) -> l a -> m b
    l #<< r = l =<< liftO r

    -- | A version of '<=<' composed with 'liftO' for the right argument.
    (<#<) :: (b -> m c) -> (a -> l b) -> a -> m c
    (<#<) l r x = l =<< liftO (r x)

    -- | A version of 'liftM1' composed with 'liftO' for the right argument.
    (<#>) :: (a -> b -> m c) -> l a -> b -> m c
    l <#> r = liftM1 l $ liftO r

instance MonadPlus m => LiftOption Maybe m where
    liftO = fromJustM

instance MonadPlus m => LiftOption (Either a) m where
    liftO = fromRightM

infix 0 <?>
{-| 
  The 'Note' class provides an ad hoc way of tagging an error case with a
  string.
-}
class (Alternative m, Monad m) => Note m where
  {-| 
    Used to annotate more precise error messages.  Replaces the '<|>' operator 
    in cases such as  
    
    > ... <|> fail "..."
  -}
  (<?>) :: m a -> String -> m a

  {-|
    Replaces the common pattern 

    > m >>= \ cond -> if cond then fail "..."
    
    The default case is defined in terms of 'empty' and '<?>'.
  -}
  failWhen :: m Bool -> String -> m ()
  failWhen m str =
      do cond <- m
         when cond empty <?> str

instance Note (Either String) where
  job <?> str = job <|> Left str

-- repetition of a functions
{-| 
  Repeatedly applies a function to an argument @n@ times.  Rather than fail,
  the original argument is returned when @n<=0@.
-}
funpow :: Int -> (a -> a) -> a -> a
funpow n f x
    | n <= 0 = x
    | otherwise = funpow (n - 1) f (f x)

-- | The monadic version of 'funpow'.
funpowM :: Monad m => Int -> (a -> m a) -> a -> m a
funpowM n f x
    | n <= 0 = return x
    | otherwise = funpowM (n - 1) f =<< f x

{-| 
  Repeatedly applies a monadic computation to an argument until there is a 
  failure.  The '<|>' operator is used for branching.
-}
repeatM :: (Alternative m, Monad m) => (a -> m a) -> a -> m a
repeatM f x = (repeatM f =<< f x) <|> return x


{-| 
  A safe version of a list map for functions of arity 2.  Fails with 'Nothing'
  if the two lists are of different lengths.
-}
map2 :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
map2 _ [] [] = Just []
map2 f (x:xs) (y:ys) =
  do zs <- map2 f xs ys
     return $! f x y : zs
map2 _ _ _ = Nothing

{-| 
  The monadic version of 'map2'.  Fails with 'mzero' if the two lists are of
  different lengths.
-}
map2M :: (Monad m, MonadPlus m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
map2M _ [] [] = return []
map2M f (x:xs) (y:ys) =
  do h <- f x y
     t <- map2M f xs ys
     return (h : t)
map2M _ _ _ = mzero

{-|
  Map a monadic function over a list, ignoring the results.  A re-export of 
  'mapM_'.
-}
doList :: Monad m => (a -> m b) -> [a] -> m ()
doList = mapM_

-- all pairs arrising from applying a function over two lists
{-|
  Produces a list containing the results of applying a function to all possible 
  combinations of arguments from two lists.  Rather than failing if the lists
  are of different lengths, iteration is shortcutted to end when the left most
  list is null.
-}
allpairs :: (a -> b -> c) -> [a] -> [b] -> [c]
allpairs _ [] _ = []
allpairs f (h:t) l2 = foldr (\ x a -> f h x : a) (allpairs f t l2) l2

-- list iteration
{-| 
  An alias to 'foldr' for HOL users more familiar with this name.  Note that the
  order of the list and base case arguments is flipped.
-}
{-# INLINEABLE itlist #-}
itlist :: (a -> b -> b) -> [a] -> b -> b
itlist f = flip (foldr f)

-- | The monadic version of 'itlist'.
itlistM :: (F.Foldable t, Monad m) => (a -> b -> m b) -> t a -> b -> m b
itlistM f = flip (F.foldrM f)

-- | The monadic version of 'foldr'.  A re-export of 'F.foldrM'.
{-# INLINEABLE foldrM #-}
foldrM :: (F.Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM = F.foldrM

{-|
  An alias to 'foldl' for HOL users more familiar with this name.  Note that the
  order of the list and base case arguments is flipped, as is the order of the
  arguments to the function.
-}
{-# INLINEABLE revItlist #-}
revItlist :: (a -> b -> b) -> [a] -> b -> b
revItlist f = flip (foldl $ flip f)

-- | The monadic version of 'foldl'.  A re-export of 'F.foldlM'.
{-# INLINEABLE foldlM #-}
foldlM :: (F.Foldable t, Monad m) => (a -> b -> m a) -> a -> t b -> m a
foldlM = F.foldlM

{-| 
  A safe version of 'foldr1'.  Fails with 'Nothing' if an empty list is provided
  as an argument.
-}
tryFoldr1 :: (a -> a -> a) -> [a] -> Maybe a
tryFoldr1 _ [] = Nothing
tryFoldr1 _ (x:[]) = Just x
tryFoldr1 f (x:xs) = liftM (f x) $ tryFoldr1 f xs

-- | An alias to 'tryFoldr1' for HOL users more familiar with this name.
{-# INLINEABLE endItlist #-}
endItlist :: (a -> a -> a) -> [a] -> Maybe a
endItlist = tryFoldr1

{-| 
  The monadic version of 'foldr1'.  Fails with 'mzero' if an empty list is
  provided as an argument.
-}
foldr1M :: (Monad m, MonadPlus m) => (a -> a -> m a) -> [a] -> m a
foldr1M _ [] = mzero
foldr1M _ [x] = return x
foldr1M f (h:t) = f h =<< foldr1M f t

{-| 
  A safe version of a right, list fold for functions of arity 2.  Fails with
  'Nothing' if the two lists are of different lengths.
-}
foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> Maybe c
foldr2 _ b [] [] = Just b
foldr2 f b (x:xs) (y:ys) =
    do b' <- foldr2 f b xs ys
       return $! f x y b'
foldr2 _ _ _ _ = Nothing

{-|
  An alias to 'foldr2' for HOL users more familiar with this name.  Note that
  the order of the two list arguments and the base case argument is flipped.
-}
{-# INLINE itlist2 #-}
itlist2 :: (a -> b -> c -> c) -> [a] -> [b] -> c -> Maybe c
itlist2 f xs ys b = foldr2 f b xs ys

{-|
  The monadic version of 'foldr2'.  Fails with 'mzero' if the two lists are
  of different lengths.
-}
foldr2M :: (Monad m, MonadPlus m) => 
           (a -> b -> c -> m c) -> c -> [a] -> [b] -> m c
foldr2M _ b [] [] = return b
foldr2M f b (h1:t1) (h2:t2) = f h1 h2 =<< foldr2M f b t1 t2
foldr2M _ _ _ _ = mzero

{-|
  A safe version of a left, list fold for functions of arity 2.  Fails with
  'Nothing' if the two lists are of different lengths.
-}
foldl2 :: (c -> a -> b -> c) -> c -> [a] -> [b] -> Maybe c
foldl2 _ b [] [] = Just b
foldl2 f b (x:xs) (y:ys) =
  foldl2 f (f b x y) xs ys 
foldl2 _ _ _ _ = Nothing

{-|
  An alias to 'foldl2' for HOL users more familiar with this name.  Note that
  the order of the two list arguments and base case argument is flipped, as is
  the order of the arguments to the provided function.
-}
{-# INLINEABLE revItlist2 #-}
revItlist2 :: (a -> b -> c -> c) -> [a] -> [b] -> c -> Maybe c
revItlist2 f xs ys b = foldl2 (\ z x y -> f x y z) b xs ys

{-|
  The monadic version of 'foldl2'.  Fails with 'mzero' if the two lists are
  of different lengths.
-}
foldl2M :: (Monad m, MonadPlus m) => 
           (c -> a -> b -> m c) -> c -> [a] -> [b] -> m c
foldl2M _ b [] [] = return b
foldl2M f b (h1:t1) (h2:t2) =
    do b' <- f b h1 h2
       foldl2M f b' t1 t2
foldl2M _ _ _ _ = mzero

-- sorting and merging of lists

{-|
  Sorts a list using a partitioning predicate to build an implied ordering.
  If @p@ is the predicate and @x \`p\` y@ and @not (y \`p\` x)@ are true then 
  @x@ will be in front of @y@ in the sorted list.
-}
sort :: Eq a => (a -> a -> Bool) -> [a] -> [a]
sort _ [] = []
sort f (piv:rest) =
    let (r, l) = partition (f piv) rest in
      sort f l ++ (piv : sort f r)

{-| 
  A more traditional sort using an 'Ordering' relationship between elements. A
  re-export of 'L.sortBy'.
-}
{-# INLINEABLE sortBy #-}
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = L.sortBy

{-|
  Merges two lists using a partitioning predicate to build an implied ordering.
  See 'sort' for more information on how the predicate affects the order of the
  resultant list.
-}
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] l2 = l2
merge _ l1 [] = l1
merge ord l1@(x:xs) l2@(y:ys)
  | ord x y = x : merge ord xs l2
  | otherwise = y : merge ord l1 ys

{-|
  Sorts a list using a partitioning predicate to build an implied ordering;
  uses 'merge' internally.  See 'sort' for more information on how the predicate
  affects the order of the resultant list.
-}
mergesort :: forall a. (a -> a -> Bool) -> [a] -> [a]
mergesort _ [] = []
mergesort ord l = mergepairs [] $ map (: []) l
  where mergepairs :: [[a]] -> [[a]] -> [a]
        mergepairs (x:[]) [] = x
        mergepairs xs [] = mergepairs [] xs
        mergepairs xs (y:[]) = mergepairs (y:xs) []
        mergepairs xs (y1:y2:ys) = mergepairs (merge ord y1 y2 : xs) ys

-- iterative term splitting and stripping via destructor
{-|
  Repeatedly applies a binary destructor function to a term until failure.
  
  Application is forward, or left-associative, such that for a term of the form
  @x1 \`f\` x2 \`f\` b@ calling this function with a destructor for @f@ will
  produce the result @([x1, x2], b)@.
-}
splitList :: (b -> Maybe (a, b)) -> b -> ([a], b)
splitList f x = 
    case f x of
      Just (l, r) -> 
          let (ls, res) = splitList f r in
            (l:ls, res)
      Nothing -> ([], x)

-- | The monadic version of 'splitList'.
splitListM :: (Alternative m, Monad m) => (b -> m (a, b)) -> b -> m ([a], b)
splitListM f x = 
    (do (l, r) <- f x
        (ls, res) <- splitListM f r
        return (l:ls, res))
    <|> return ([], x)

{-|
  Repeatedly applies a binary destructor function to a term until failure.
  
  Application is reverse, or right-associative, such that for a term of the form
  @x1 \`f\` x2 \`f\` b@ calling this function with a destructor for @f@ will
  produce the result @(f, [x1, x2 \`f\` b])@.
-}
revSplitList :: forall a. (a -> Maybe (a, a)) -> a -> (a, [a])
revSplitList f = recSplit []
  where recSplit :: [a] -> a -> (a, [a])
        recSplit ls y = 
            case f y of
              Just (l, r) -> recSplit (r:ls) l
              Nothing -> (y, ls)

-- | The monadic version of 'revSplitList'.
revSplitListM :: forall m b. (Alternative m, Monad m) => 
                             (b -> m (b, b)) -> b -> m (b, [b])
revSplitListM f = rsplist []
  where rsplist :: [b] -> b -> m (b, [b])
        rsplist ls y = 
            (do (l, r) <- f y
                rsplist (r:ls) l)
            <|> return (y, ls)
                            
{-|
  Repeatedly applies a binary destructor function to a term for every element
  in a provided list.
  
  Application is reverse, or right-associative, such that for a term of the form
  @f x1 (f x2 ...(f xn b))@ calling this function with a destructor for @f@ and
  a list @l@ will produce the result @([x1 .. xk], f x(k+1) ...(f xn b))@ where 
  @k@ is the length of list @l@.
-}
nsplit :: (a -> Maybe (a, a)) -> [b] -> a -> Maybe ([a], a)
nsplit _ [] x = return ([], x)
nsplit dest (_:cs) x =
    do (l, r) <- dest x
       (ll, y) <- nsplit dest cs r
       return (l:ll, y)

-- | The monadic version of 'nsplit'.
nsplitM :: Monad m => (b -> m (b, b)) -> [c] -> b -> m ([b], b)
nsplitM _ [] x = return ([], x)
nsplitM dest (_:n) x = 
    do (l, r) <- dest x
       (ll, y) <- nsplitM dest n r
       return (l:ll, y)

{-|
  Repeatedly applies a binary destructor function to a term until failure.
  
  Application is forward, or left-associative, such that for a term of the form
  @x1 \`f\` x2 \`f\` x3@ calling this function with a destructor for @f@ will
  produce the result @[x1, x2, x3]@.
-}
stripList :: forall a. (a -> Maybe (a, a)) -> a -> [a]
stripList dest x = strip x []
  where strip :: a -> [a] -> [a]
        strip x' acc =
            case dest x' of
              Just (l, r) -> strip l $ strip r acc
              Nothing -> x' : acc

-- | The monadic version of 'stripList'.
stripListM :: forall m a. (Alternative m, Monad m) => 
                          (a -> m (a, a)) -> a -> m [a]
stripListM dest x = strip x []
  where strip :: a -> [a] -> m [a]
        strip x' acc =
          (do (l, r) <- dest x'
              strip l =<< strip r acc)
          <|> return (x' : acc)


-- miscellaneous list methods

-- | An alias to 'all' for HOL users who are more familiar with this name.
{-# INLINEABLE forall #-}
forall :: (a -> Bool) -> [a] -> Bool
forall = all

{-| 
  A version of 'all' for predicates of arity 2.  Iterates down two lists
  simultaneously with 'map2', using 'and' to combine the results.
-}
forall2 :: (a -> b -> Bool) -> [a] -> [b] -> Maybe Bool
forall2 f xs = liftM and . map2 f xs

-- | An alias to 'any' for HOL users who are more familiar with this name.
{-# INLINEABLE exists #-}
exists :: (a -> Bool) -> [a] -> Bool
exists = any

{-| 
  Separates a list of elements using a predicate.  A re-export of 'L.partition'.
-}
{-# INLINEABLE partition #-}
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = L.partition

-- | An alias to 'mapMaybe' for HOL users more familiar with this name.
{-# INLINEABLE mapFilter #-}
mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter = mapMaybe

{-| 
  The monadic version of 'mapFilter'.  The '(<|>)' operator is used for 
  branching.
-}
mapFilterM :: (Alternative m, Monad m) => (a -> m b) -> [a] -> m [b]
mapFilterM _ [] = return []
mapFilterM f (x:xs) =
    do xs' <- mapFilterM f xs
       (do x' <- f x
           return (x':xs'))
         <|> return xs'

-- | A re-export of 'L.find'.
{-# INLINEABLE find #-}
find :: (a -> Bool) -> [a] -> Maybe a
find = L.find

{-| 
  The monadic version of 'find'.  Fails if the monadic predicate does.  Also 
  fails with 'mzero' if an empty list is provided.
-}
findM :: (Monad m, MonadPlus m) => (a -> m Bool) -> [a] -> m a
findM _ [] = mzero
findM f (x:xs) =
    do b <- f x
       if b
          then return x
          else findM f xs

{-|
  An alternative monadic version of 'find' where the predicate is a monadic
  computation not necessarily of a boolean return type.  Returns the result of
  the first successful application of the predicate to an element of the list.
  Fails with 'mzero' if called on an empty list.  

  Note that 'mplus' is used for branching instead of '<|>' to minimize the 
  constraint type; for the vast majority of monads these two functions should be
  identical anyway.
-}
tryFind :: (Monad m, MonadPlus m) => (a -> m b) -> [a] -> m b
tryFind _ [] = mzero
tryFind f (x:xs) = f x `mplus` tryFind f xs

-- | An alias to 'concat' for HOL users who are more familiar with this name.
{-# INLINEABLE flat #-}
flat :: [[a]] -> [a]
flat = concat

{-| 
  Drops elements from the end of a list while a predicate is true.  A re-export
  of 'L.dropWhileEnd'.
-}
{-# INLINEABLE dropWhileEnd #-}
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd = L.dropWhileEnd

{-| 
  Separates the first element of a list that satisfies a predicate.  Fails with
  'Nothing' if no such element is found.
-}
remove :: (a -> Bool) -> [a] -> Maybe (a, [a])
remove _ [] = Nothing
remove p (h:t)
  | p h = Just (h, t)
  | otherwise =
      do (y, n) <- remove p t
         return (y, h:n)

{-|
  A safe version of 'splitAt'.   Fails with 'Nothing' if a split is attempted
  at an index that doesn't exist.
-}
trySplitAt :: Int -> [a] -> Maybe ([a], [a])
trySplitAt n l
    | n < 0 = Nothing
    | n == 0 = Just ([], l)
    | otherwise = 
        case l of
          [] -> Nothing
          (x:xs) -> do (m, l') <- trySplitAt (n - 1) xs
                       return (x:m, l')

-- | An alias to 'trySplitAt' for HOL users more familiar with this name
{-# INLINEABLE chopList #-}
chopList :: Int -> [a] -> Maybe ([a], [a])
chopList = trySplitAt

{-|
  Returns the first index where an element appears in list.  Fails with 
  'Nothing' if no such element is found.  A re-export of 'L.elemIndex'.
-}
{-# INLINEABLE elemIndex #-}
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex = L.elemIndex

-- | An alias to 'elemIndex' for HOL users more familiar with this name.
{-# INLINEABLE index #-}
index :: Eq a => a -> [a] -> Maybe Int
index = elemIndex

{-|
  Drops the given prefix from a list.  Fails with 'Nothing' if there is no such
  prefix.  A re-export of 'L.stripPrefix'.
-}
{-# INLINEABLE stripPrefix #-}
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix = L.stripPrefix

-- | Removes adjacent, equal elements from a list.
uniq :: Eq a => [a] -> [a]
uniq (x:y:t) =
  let t' = uniq t in
    if x == y then t' else x : t'
uniq l = l

{-| 
  Partitions a list into a list of lists matching the structure of the first 
  argument. For example:
  @shareOut [[1, 2], [3], [4, 5]] \"abcde\" === [\"ab\", \"c\", \"de\"]@
-}
shareOut :: [[a]] -> [b] -> Maybe [[b]]
shareOut [] _ = Just []
shareOut (p:ps) bs = 
    do (l, r) <- chopList (length p) bs
       ls <- shareOut ps r
       return (l : ls)

-- set operations on lists
-- | An alias to 'elem' for HOL users who are more familiar with this name.
{-# INLINEABLE mem #-}
mem :: Eq a => a -> [a] -> Bool
mem = elem

{-|  
  Inserts an item into a list if it would be a unique element.

  Important note:  This insert is unordered, unlike the 'L.insert' in the
  "Data.List" module.
-}
insert :: Eq a => a -> [a] -> [a]
insert x l
    | x `elem` l = l
    | otherwise = x : l

{-|
  Inserts, or updates, a key value pair in an association list.

  Note that this insert is unordered, but uniqueness preserving.
-}
insertMap :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
insertMap key v [] = [(key, v)]
insertMap key v (x@(key', _):xs)
    | key == key' = (key, v) : xs
    | otherwise = x : insertMap key v xs

{-|
  Unions two list maintaining uniqueness of elements.

  Important note:  This union is unordered, unlike the 'L.union' in the
  "Data.List" module.
-}
union :: Eq a => [a] -> [a] -> [a]
union l1 l2 = foldr insert l2 l1

-- | Unions a list of lists using 'union'.
unions :: Eq a => [[a]] -> [a]
unions = foldr union []

-- | Finds the intersection of two lists.  A re-export of 'L.intersect'.
{-# INLINEABLE intersect #-}
intersect :: Eq a => [a] -> [a] -> [a]
intersect = L.intersect

-- | Removes an item from a list.  A re-export of 'L.delete'.
{-# INLINEABLE delete #-}
delete :: Eq a => a -> [a] -> [a]
delete = L.delete 

-- | Subtracts one list from the other.  A re-export of 'L.\\'.
{-# INLINEABLE (\\) #-}
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = (L.\\)

-- | Tests if the first list is a subset of the second.
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

-- | A test for set equality using 'subset'.
setEq :: Eq a => [a] -> [a] -> Bool
setEq l1 l2 = subset l1 l2 && subset l2 l1

-- | Converts a list to a set by removing duplicates.  A re-export of 'L.nub'.
{-# INLINEABLE nub #-}
nub :: Eq a => [a] -> [a]
nub = L.nub

-- | An alias to 'nub' for HOL users more familiar with this name.
{-# INLINEABLE setify #-}
setify :: Eq a => [a] -> [a]
setify = nub

-- set operations parameterized by equality
{-|
  A version of 'mem' where the membership test is an explicit predicate, rather
  than a strict equality test.
-}
mem' :: (a -> a -> Bool) -> a -> [a] -> Bool
mem' _ _ [] = False
mem' eq a (x:xs) = eq a x || mem' eq a xs

{-|
  A version of 'insert' where the uniqueness test is an explicit predicate, 
  rather than a strict equality test.
-}
insert' :: (a -> a -> Bool) -> a -> [a] -> [a]
insert' eq x xs
  | mem' eq x xs = xs
  | otherwise = x : xs

{-|
  A version of 'union' where the uniqueness test is an explicit predicate, 
  rather than a strict equality test.
-}
union' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
union' eq xs ys = foldr (insert' eq) ys xs

{-|
  A version of 'unions' where the uniqueness test is an explicit predicate, 
  rather than a strict equality test.
-}
unions' :: (a -> a -> Bool) -> [[a]] -> [a]
unions' eq = foldr (union' eq) []

{-|
  A version of 'subtract' where the uniqueness test is an explicit predicate, 
  rather than a strict equality test.
-}
subtract' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
subtract' eq xs ys = filter (\ x -> not $ mem' eq x ys) xs

{-|
  Groups neighbors in a list together based on a predicate.  A re-export of
  'L.groupBy'.
-}
{-# INLINEABLE group' #-}
group' :: (a -> a -> Bool) -> [a] -> [[a]]
group' = L.groupBy

-- | A version of 'uniq' that eliminates elements based on a provided predicate.
uniq' :: Eq a => (a -> a -> Bool) -> [a] -> [a]
uniq' eq l@(x:t@(y:_)) =
    let t' = uniq' eq t in
      if x `eq` y then t'
      else if t' == t then l else x:t'
uniq' _ l = l

{-| 
  A version of 'setify' that eliminates elements based on a provided predicate.
-}
setify' :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [a]
setify' le eq xs = uniq' eq $ sort le xs

-- some useful functions on "num" types
{-$NumAliases

  The following are aliases to frequently used values and functions for
  arbitrary-precision integers.  Typically, they are used when converting to and
  from numbers in the implementation language and the logic language.  The
  aliases clarify the intended use and saves us from having lots of explicit
  type annotations to force 'Integer' values.
-}
-- | > 0 :: Integer
{-# INLINEABLE num0 #-}
num0 :: Integer
num0 = 0

-- | > 1 :: Integer
{-# INLINEABLE num1 #-}
num1 :: Integer
num1 = 1

-- | > 2 :: Integer
{-# INLINEABLE num2 #-}
num2 :: Integer
num2 = 2

-- | > 10 :: Integer
{-# INLINEABLE num10 #-}
num10 :: Integer
num10 = 10

-- | > x ^ (2 :: Integer)
{-# INLINEABLE pow2 #-}
pow2 :: Integer -> Integer
pow2 x = x ^ (2 :: Integer)

-- | > x ^ (10 :: Integer)
{-# INLINEABLE pow10 #-}
pow10 :: Integer -> Integer
pow10 x = x ^ (10 :: Integer)

{-| 
  Converts a real number to a rational representation.  
  An alias to 'toRational' for HOL users more familiar with this name.
-}
{-# INLINEABLE numdom #-}
numdom :: Real a => a -> Rational
numdom = toRational

-- | Returns the numerator of a rational number.  A re-export of 'R.numerator'.
{-# INLINEABLE numerator #-}
numerator :: Rational -> Integer
numerator = R.numerator

{-| 
  Returns the denominator of a rational number.  A re-export of 'R.denominator'.
-}
{-# INLINEABLE denominator #-}
denominator :: Rational -> Integer
denominator = R.denominator

{-| 
  Finds the least common denominator between two numbers.  An alias to 'gcd' for
  HOL users more familiar with this name.
-}
{-# INLINEABLE gcdNum #-}
gcdNum :: Integer -> Integer -> Integer
gcdNum = gcd

{-|
  Finds the least common multiplier between two numbers.  An alias to 'lcm' for
  HOL users more familiar with this name.
-}
{-# INLINEABLE lcmNum #-}
lcmNum :: Integer -> Integer -> Integer
lcmNum = lcm

{-|
  Converts a string representation of a number to an appropriate instance of
  the 'Num' class.  Fails with 'Nothing' if the conversion cannot be performed.

  Note:  The following prefixes are valid:

  * @0x@ - number read as a hexidecimal value

  * @0b@ - number read as a binary value

  * Any other prefix causes the number to be read as a decimal value
-}
numOfString :: forall a. (Eq a, Num a) => String -> Maybe a
numOfString s =
    case res of
      [(x, "")] -> Just x
      _ -> Nothing
   where res :: [(a, String)]
         res = case s of
                 ('0':'x':s') -> readHex s'
                 ('0':'b':s') -> readInt 2 (`elem` "01") digitToInt s'
                 _ -> readDec s

-- language type classes
{-$LangClasses
  The following two classes are used as an ad hoc mechanism for sharing
  \"language\" operations between different types.  For example, both tactics
  and conversions share a number of the same operations.  Rather than having 
  multiple functions, such as @thenTac@ and @thenConv@, we've found it easier to
  have a single, polymorphic function to use, '_THEN'.

  The sequencing operations are seperated in their own class, 'LangSeq', because
  their tactic instances have a reliance on the boolean logic theory.  Rather 
  than unecessarily propogate this prerequisite for all members of the 'Lang' 
  class, we elected to separate them.
-}
{-| 
  The 'Lang' class defines common language operations and combinators not based
  on sequencing.
-}
class Lang a where
    {-| 
      A primitive language operation that always fails.  Typically this is
      written using 'throw'.
    -}
    _FAIL :: String -> a
    -- | An instance of '_FAIL' with a fixed failure string.
    _NO :: a
    -- | A primitive language operation that always succeeds.
    _ALL :: a
    {-| 
      A language combinator for branching based on failure.  The language
      equivalent of the '<|>' operator.
    -}
    _ORELSE :: a -> a -> a
    -- | A language combinator that performs the first operation in a list.
    _FIRST :: [a] -> a
    {-| 
      A language combinator that fails if the wrapped operation doesn't invoke
      some change, i.e. a tactic fails to change the goal state.
    -}
    _CHANGED :: a -> a
    {-| 
      A language combinator that prevents the wrapped operation from having an
      effect if it fails.  The language equivalent of the backtracking 'try' 
      operator.
    -}
    _TRY :: a -> a

{-|
  The 'LangSeq' class defines common language operations and combinators based
  on sequencing.  See the note at the top of this section for more details as
  to why these are separated on their own.
-}
class LangSeq a where
    -- | A language combinator that sequences operations.
    _THEN :: a -> a -> a
    {-| 
      A language combinator that repeatedly applies a language operation until 
      failure.
    -}
    _REPEAT :: a -> a
    {-| 
      A language combinator that performs every operation in a list  
      sequentially.
    -}
    _EVERY :: [a] -> a

-- Not currently part of the Parsec library, so we define it here
-- both orphan instances
deriving instance Eq Assoc

deriveLift ''Assoc
