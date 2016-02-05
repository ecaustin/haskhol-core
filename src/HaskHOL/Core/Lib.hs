{-# LANGUAGE ScopedTypeVariables #-}
{-|
  Module:    HaskHOL.Core.Lib
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
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
      wComb
    , ffComb
    , ffCombM
    , on
      -- * Basic Operations on Pairs
    , swap
    , pairMap
    , pairMapM
    , first
    , firstM
    , second
    , secondM
      -- * Basic Operations on Lists
    , tryHead
    , tryTail
    , tryInit
    , tryLast
    , tryIndex
      -- * Basic Operations on Association Lists
    , assoc
    , revAssoc
    , assocd
    , revAssocd
      -- * Methods for Error Handling
    , (<|>)
    , (<?>)
    , note
    , fail'
    , failWhen
    , maybeToFail
    , try'
    , tryd
    , test'
    , can
    , can'
    , canNot
    , check
      -- * Methods for Function Repetition
    , funpow 
    , funpowM
    , repeatM
    , map2
    , map2M
    , allpairs
      -- * Methods for List Iteration
    , foldrM
    , itlistM
    , foldlM
    , revItlistM
    , tryFoldr1
    , foldr1M
    , foldr2
    , foldr2M
    , foldl2 
    , foldl2M
      -- * Methods for Sorting and Merging Lists
    , sort
    , sortBy
    , merge
    , mergesort
      -- * Methods for Splitting and Stripping Binary Terms
    , splitList
    , splitListM
    , revSplitList
    , revSplitListM
    , nsplit
    , stripList  
    , stripListM 
      -- * Methods for Searching and Manipulating Lists
    , all2 
    , partition 
    , mapFilter 
    , mapFilterM
    , find      
    , findM   
    , tryFind  
    , dropWhileEnd
    , remove   
    , trySplitAt
    , index
    , stripPrefix
    , uniq
    , shareOut
      -- * Set Operations on Lists   
    , insert
    , insertMap
    , union 
    , unions
    , intersect
    , delete
    , (\\)
    , subset
    , setEq
    , setify
    , nub
      -- * Set Operations Parameterized by Predicate
    , mem'
    , insert'
    , union'
    , unions'
    , subtract'
    , group'
    , uniq'
    , setify'
      -- * Operations on \"Num\" Types
    , (%)
    , numdom     
    , numerator   
    , denominator
    , numOfString
      -- * Polymorphic, Finite, Partial Functions Via Patricia Trees
    , Func
    , funcEmpty
    , isEmpty
    , funcMap
    , funcFoldl
    , funcFoldr
    , graph
    , dom
    , ran
    , applyd
    , apply
    , tryApplyd
    , defined
    , undefine
    , (|->)
    , combine
    , (|=>)
    , choose
      -- * Re-exported 'Map' primitives
    , Map
    , mapEmpty
    , mapInsert
    , mapUnion
    , mapDelete
    , mapAssoc
    , mapElems
    , mapFromList
    , mapToList
    , mapMap
    , mapFoldrWithKey
    , mapRemove
    , mapToAscList
      -- * Re-exported 'Text' primitives
    , Text
    , append
    , cons
    , snoc
    , pack
    , unpack
    , textHead
    , textTail
    , textNull
    , textEmpty
    , textStrip
    , textShow
      -- * Re-exported 'AcidState' primitives
    , Update
    , Query
    , get
    , put
    , ask
    , makeAcidic
      -- * Classes for Common \"Language\" Operations
      -- $LangClasses
    , Lang(..)
    , LangSeq(..)
      -- * Miscellaneous Re-exported Libraries
    , module HaskHOL.Core.Lib.Families {-|
        Re-exports a few type families used for basic, type-level boolean
        computation.
      -}
    , module Control.DeepSeq {-|
        Re-exports the entirety of the library, but currently only 'NFData' is
        used.  Necessary for using the "Criterion" benchmarking library.
      -}
    , module Control.Monad {-|
        Re-exports the entirety of the library for use with the 'HOL' monad.
      -}
    , module Data.Data {-|
        Re-exports the 'Data' and 'Typeable' class names for use in deriving 
        clauses.
      -}
    , module Data.SafeCopy {-|
        Re-exports the entirety of the library for use with the 'HOL' monad's
        acid state primitives.
      -}
    , module Control.Monad.Catch {-|
        Re-exports the entirety of the library, serving as the basis for our
        extensible exception handling.
      -}
    , module Control.Monad.Catch.Pure {-|
        Re-exports the entirety of the library, serving as a pure alternative
        to the methods in @Control.Monad.Catch@
      -}
    ) where

import HaskHOL.Core.Kernel.Prims (HOLPrimError(..))
import HaskHOL.Core.Lib.Families

-- Libraries re-exported in their entirety, except for applicative
import Control.DeepSeq
import Control.Monad hiding (mzero)
import Data.Data (Data, Typeable)
import Data.SafeCopy
import Control.Monad.Catch
import Control.Monad.Catch.Pure

-- Libraries containing Re-exports
import qualified Control.Arrow as A
import qualified Data.Foldable as F
import qualified Data.Function as DF (on)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Ratio as R
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified TextShow as Text
import qualified Data.Tuple as T
-- Acid State imports
import qualified Control.Monad.State as State (MonadState, get, put)
import qualified Control.Monad.Reader as Reader (MonadReader, ask)
import Data.Acid (Update, Query)
import qualified Data.Acid as Acid
import Language.Haskell.TH

-- Libraries containing utility functions used, but not exported directly
import Numeric (readInt, readHex, readDec)
import Data.Char (digitToInt)
import Data.Bits
import Data.Hashable

-- combinators

{-| 
  The W combinator.  Takes a function of arity 2 and applies a single argument
  to it twice.
-}
wComb :: (a -> a -> b) -> a -> b
wComb f x = f x x

-- | The FF combinator.  An alias for the arrow combinator 'A.***'.  
ffComb :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
ffComb = (A.***)

{-| 
  The monadic version of the FF combinator.  An alias for the arrow combinator
  'A.***' lifted for 'A.Kleisli' arrows.
-}
ffCombM :: Monad m => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
ffCombM f g = A.runKleisli $ A.Kleisli f A.*** A.Kleisli g

{-|
  Re-export of the 'DF.on' combination from @Data.Function@.
-}
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = DF.on

-- pair basics

-- | Swaps the order of a pair.  A re-export of 'T.swap'.
swap :: (a, b) -> (b, a)
swap = T.swap

-- | Applies a function to both elements of a pair using the 'A.***' operator.
pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f = f A.*** f

-- | The monadic version of 'pairMap'.
pairMapM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
pairMapM f = f `ffCombM` f

{-|
  Applies a function only to the first element of a pair.  A re-export of 
  'A.first'.
-}
first :: (a -> c) -> (a, b) -> (c, b)
first = A.first

-- | A monadic version of 'first' lifted for 'A.Kleisli' arrows.
firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM = A.runKleisli . A.first . A.Kleisli

{-| 
  Applies a function only to the second element of a pair.  A re-export of 
  'A.second'.
-}
second :: (b -> c) -> (a, b) -> (a, c)
second = A.second

-- | A monadic version of 'second' lifted for 'A.Kleisli' arrows.
secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM = A.runKleisli . A.second . A.Kleisli

-- list basics
-- | A guarded version of 'head'.
tryHead :: MonadThrow m => [a] -> m a
tryHead (x:_) = return x
tryHead _ = fail' "tryHead"

-- | A guarded version of 'tail'.
tryTail :: MonadThrow m => [a] -> m [a]
tryTail (_:t) = return t
tryTail _ = fail' "tryTail"

-- | A guarded version of 'init'.
tryInit :: MonadThrow m => [a] -> m [a]
tryInit [_] = return []
tryInit (x:xs) = (:) x `fmap` tryInit xs
tryInit _ = fail' "tryInit"

-- | A guarded version of 'last'.
tryLast :: MonadThrow m => [a] -> m a
tryLast [x] = return x
tryLast (_:xs) = tryLast xs
tryLast _ = fail' "tryLast"

-- | A guarded version of 'index'.
tryIndex :: MonadThrow m => [a] -> Int -> m a
tryIndex xs n
    | n >= 0 = tryHead $ drop n xs
    | otherwise = fail' "tryIndex"

-- association lists
-- | A guarded alias to 'lookup'.
assoc :: (MonadThrow m, Eq a) => a -> [(a, b)] -> m b
assoc x = maybeToFail "assoc" . lookup x

{-| 
  A guarded version of 'lookup' where the search is performed against the 
  second element of the pair instead of the first.
-}
revAssoc :: (MonadThrow m, Eq a) => a -> [(b, a)] -> m b
revAssoc _ [] = fail' "revAssoc"
revAssoc x ((f, s):as)
  | x == s = return f
  | otherwise = revAssoc x as

-- | A version of 'assoc' that defaults to a provided value rather than fail.
assocd :: Eq a => a -> [(a, b)] -> b -> b
assocd x xs b = tryd b $ assoc x xs

{-| 
  A version of 'revAssoc' that defaults to a provided value rather than fail.
-}
revAssocd :: Eq a => a -> [(b, a)] -> b -> b
revAssocd x xs b = tryd b $ revAssoc x xs

-- error handling and checking

infixl 3 <|>
-- | A version of the alternative operator based on 'MonadCatch'.
(<|>) :: forall m a. MonadCatch m => m a -> m a -> m a
job <|> err = job `catch` ferr
    where ferr :: SomeException -> m a
          ferr _ = err

infix 0 <?>
{-| 
  Replaces the exception thrown by a computation with a provided 'String' 
  message, i.e.
  
  > m (<?>) str = m <|> fail' str
-}  
(<?>) :: (MonadCatch m, MonadThrow m) => m a -> String -> m a
m <?> str = m <|> fail' str

{-| 
  Converts the exception thrown by a computaiton to a 'String' and prepends it
  with a provided message.
-}
note :: (MonadCatch m, MonadThrow m) => String -> m a -> m a
note str m =
    m `catch` (\ e -> case fromException e of
                        Just (HOLErrorMsg str2) -> 
                            throwM $! HOLErrorMsg (str ++ ": " ++ str2)
                        _ ->
                            throwM $! HOLErrorMsg (str ++ ": " ++ show e))

-- | A version of 'fail' based on 'MonadThrow' using 'HOLPrimError'.
fail' :: MonadThrow m => String -> m a
fail' = throwM . HOLErrorMsg

-- | The 'fail'' method guarded by 'when'.
failWhen :: (MonadCatch m, MonadThrow m) => m Bool -> String -> m ()
failWhen m str =
    do cond <- m
       when cond $ fail' str

-- | Converts a 'Maybe' value to a 'MonadThrow' value using 'fail''.
maybeToFail :: MonadThrow m => String -> Maybe a -> m a
maybeToFail str = maybe (fail' str) return

{-| 
  Forces the evaluation of a 'Catch' computation, relying on 'error' if it 
  fails.
-}
try' :: Catch a -> a
try' = either (\ _ -> error "try'") id . runCatch

{-|
  Forces the evaluation of a 'Catch' computation, returning a default value if
  it fails.
-}
tryd :: a -> Catch a -> a
tryd d = either (const d) id . runCatch

-- | Tests if the evaluation of a 'Catch' computation succeeds or not.
test' :: Catch a -> Bool
test' = either (const False) (const True) . runCatch

{-| 
  Returns a boolean value indicating whether a monadic computation succeeds or
  fails.  The '<|>' operator is used for branching.
-}
can :: MonadCatch m => (a -> m b) -> a -> m Bool
can f x = (f x >> return True) <|> return False

{-| 
  A version of 'can' that instead marks success or failure with a 'Maybe' value.
  Turns a potentially failing monadic computation into a guarded, always 
  successful monadic computation.
-}
can' :: MonadCatch m => (a -> m b) -> a -> m (Maybe b)
can' f x = (f x >>= \ x' -> return (Just x')) <|> return Nothing

{-| 
  The opposite of 'can'.  Functionally equivalent to 

  > \ f -> liftM not . can f
-}
canNot :: MonadCatch m => (a -> m b) -> a -> m Bool
canNot f x = (f x >> return False) <|> return True

{-| 
  Checks if a predicate succeeds for a provided value, returning that value
  guarded if so.
-} 
check :: MonadThrow m => (a -> Bool) -> a -> m a
check p x
  | p x = return x
  | otherwise = fail' "check"

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
repeatM :: MonadCatch m => (a -> m a) -> a -> m a
repeatM f x = (repeatM f =<< f x) <|> return x


-- | A guarded version of a list map for functions of arity 2.
map2 :: MonadThrow m => (a -> b -> c) -> [a] -> [b] -> m [c]
map2 _ [] [] = return []
map2 f (x:xs) (y:ys) = (:) (f x y) `fmap` map2 f xs ys
map2 _ _ _ = fail' "map2"

-- | A version of 'map2' that accepts monadic functions.
map2M :: MonadThrow m => (a -> b -> m c) -> [a] -> [b] -> m [c]
map2M _ [] [] = return []
map2M f (x:xs) (y:ys) =
  do h <- f x y
     t <- map2M f xs ys
     return (h : t)
map2M _ _ _ = fail' "map2M"

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
-- | The monadic version of 'foldr'.  A re-export of 'F.foldrM'.
foldrM :: (F.Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM = F.foldrM

-- | A version of 'foldrM' with its arguments flipped.
itlistM :: (F.Foldable t, Monad m) => (a -> b -> m b) -> t a -> b -> m b
itlistM f = flip (F.foldrM f)

-- | The monadic version of 'foldl'.  A re-export of 'F.foldlM'.
foldlM :: (F.Foldable t, Monad m) => (a -> b -> m a) -> a -> t b -> m a
foldlM = F.foldlM

-- | A version of 'foldlM' with its arguments flipped.
revItlistM :: (F.Foldable t, Monad m) => (b -> a -> m a) -> t b -> a -> m a
revItlistM f = flip (F.foldlM (flip f))

-- | A guarded version of 'foldr1'.
tryFoldr1 :: MonadThrow m => (a -> a -> a) -> [a] -> m a
tryFoldr1 _ [] = fail' "tryFoldr1"
tryFoldr1 _ [x] = return x
tryFoldr1 f (x:xs) = f x `fmap` tryFoldr1 f xs

-- | A version of 'foldr1' that accepts monadic functions.
foldr1M :: MonadThrow m => (a -> a -> m a) -> [a] -> m a
foldr1M _ [] = fail' "foldr1M"
foldr1M _ [x] = return x
foldr1M f (h:t) = f h =<< foldr1M f t

-- | A guarded version of a right, list fold for functions of arity 2.
foldr2 :: MonadThrow m => (a -> b -> c -> c) -> c -> [a] -> [b] -> m c
foldr2 _ b [] [] = return b
foldr2 f b (x:xs) (y:ys) = f x y `fmap` foldr2 f b xs ys
foldr2 _ _ _ _ = fail' "foldr2"

-- | A version of 'foldr2' that accepts monadic functions.
foldr2M :: MonadThrow m => (a -> b -> c -> m c) -> c -> [a] -> [b] -> m c
foldr2M _ b [] [] = return b
foldr2M f b (h1:t1) (h2:t2) = f h1 h2 =<< foldr2M f b t1 t2
foldr2M _ _ _ _ = fail' "foldr2M"

-- | A guarded version of a left, list fold for functions of arity 2.
foldl2 :: MonadThrow m => (c -> a -> b -> c) -> c -> [a] -> [b] -> m c
foldl2 _ b [] [] = return b
foldl2 f b (x:xs) (y:ys) =
  foldl2 f (f b x y) xs ys 
foldl2 _ _ _ _ = fail' "foldl2"

-- | A version of `foldl2` that accepts monadic functions.
foldl2M :: MonadThrow m => (c -> a -> b -> m c) -> c -> [a] -> [b] -> m c
foldl2M _ b [] [] = return b
foldl2M f b (h1:t1) (h2:t2) = 
    do b' <- f b h1 h2
       foldl2M f b' t1 t2
foldl2M _ _ _ _ = fail' "foldl2M"

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
        mergepairs [x] [] = x
        mergepairs xs [] = mergepairs [] xs
        mergepairs xs [y] = mergepairs (y:xs) []
        mergepairs xs (y1:y2:ys) = mergepairs (merge ord y1 y2 : xs) ys

-- iterative term splitting and stripping via destructor
{-|
  Repeatedly applies a binary destructor function to a term until failure.
  
  Application is forward, or left-associative, such that for a term of the form
  @x1 \`f\` x2 \`f\` b@ calling this function with a destructor for @f@ will
  produce the result @([x1, x2], b)@.
-}
splitList :: (b -> Catch (a, b)) -> b -> ([a], b)
splitList f = try' . splitListM f

-- | The monadic version of 'splitList'.
splitListM :: MonadCatch m => (b -> m (a, b)) -> b -> m ([a], b)
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
revSplitList :: (a -> Catch (a, b)) -> a -> (a, [b])
revSplitList f = try' . revSplitListM f

-- | The monadic version of 'revSplitList'.
revSplitListM :: forall m a b. MonadCatch m => (a -> m (a, b)) 
              -> a -> m (a, [b])
revSplitListM f = rsplist []
  where rsplist :: [b] -> a -> m (a, [b])
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
nsplit :: Monad m => (a -> m (a, a)) -> [b] -> a -> m ([a], a)
nsplit _ [] x = return ([], x)
nsplit dest (_:cs) x =
    do (l, r) <- dest x
       (ll, y) <- nsplit dest cs r
       return (l:ll, y)

{-|
  Repeatedly applies a binary destructor function to a term until failure.
  
  Application is forward, or left-associative, such that for a term of the form
  @x1 \`f\` x2 \`f\` x3@ calling this function with a destructor for @f@ will
  produce the result @[x1, x2, x3]@.
-}
stripList :: (a -> Catch (a, a)) -> a -> [a]
stripList dest = try' . stripListM dest

-- | The monadic version of 'stripList'.
stripListM :: forall m a. MonadCatch m => (a -> m (a, a)) -> a -> m [a]
stripListM dest x = strip x []
  where strip :: a -> [a] -> m [a]
        strip x' acc =
          (do (l, r) <- dest x'
              strip l =<< strip r acc)
          <|> return (x' : acc)


-- miscellaneous list methods

{-| 
  A version of 'all' for predicates of arity 2.  Iterates down two lists
  simultaneously with 'map2', using 'and' to combine the results.
-}
all2 :: MonadThrow m => (a -> b -> Bool) -> [a] -> [b] -> m Bool
all2 f xs ys = and `fmap` map2 f xs ys

{-| 
  Separates a list of elements using a predicate.  A re-export of 'L.partition'.
-}
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = L.partition

-- | Filter's a list of items using a `Catch` predicate.
mapFilter :: (a -> Catch b) -> [a] -> [b]
mapFilter f = try' . mapFilterM f

-- | The monadic version of 'mapFilter'.
mapFilterM :: MonadCatch m => (a -> m b) -> [a] -> m [b]
mapFilterM _ [] = return []
mapFilterM f (x:xs) =
    do xs' <- mapFilterM f xs
       (do x' <- f x
           return (x':xs'))
         <|> return xs'

-- | A guarded re-export of 'L.find'.
find :: MonadThrow m => (a -> Bool) -> [a] -> m a
find f = maybeToFail "find" . L.find f

{-| 
  The monadic version of 'find'.  Fails if the monadic predicate does.
-}
findM :: MonadCatch m => (a -> m Bool) -> [a] -> m a
findM _ [] = fail' "findM"
findM f (x:xs) =
    do b <- f x
       if b
          then return x
          else findM f xs

{-|
  An alternative monadic version of 'find' where the predicate is a monadic
  computation not necessarily of a boolean return type.  Returns the result of
  the first successful application of the predicate to an element of the list.
  Fails when called on an empty list. 
-}
tryFind :: (MonadCatch m, MonadThrow m) => (a -> m b) -> [a] -> m b
tryFind _ [] = fail' "tryFind"
tryFind f (x:xs) = f x <|> tryFind f xs

{-| 
  Drops elements from the end of a list while a predicate is true.  A re-export
  of 'L.dropWhileEnd'.
-}
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd = L.dropWhileEnd

{-| 
  Separates the first element of a list that satisfies a predicate.  Fails with
  'Nothing' if no such element is found.
-}
remove :: MonadThrow m => (a -> Bool) -> [a] -> m (a, [a])
remove _ [] = fail' "remove"
remove p (h:t)
  | p h = return (h, t)
  | otherwise =
      do (y, n) <- remove p t
         return (y, h:n)

{-|
  A guarded version of 'splitAt'.   
  Fails if a split is attempted at an index that doesn't exist.
-}
trySplitAt :: MonadThrow m => Int -> [a] -> m ([a], [a])
trySplitAt n l
    | n < 0 = fail' "trySplitAt"
    | n == 0 = return ([], l)
    | otherwise = 
        case l of
          [] -> fail' "trySplitAt"
          (x:xs) -> do (m, l') <- trySplitAt (n - 1) xs
                       return (x:m, l')

-- | Returns the first index where an element appears in list.
index :: (MonadThrow m, Eq a) => a -> [a] -> m Int
index x = maybeToFail "index" . L.elemIndex x

-- | A guarded versino of 'L.stripPrefix'.
stripPrefix :: (MonadThrow m, Eq a) => [a] -> [a] -> m [a]
stripPrefix xs = maybeToFail "stripPrefix" . L.stripPrefix xs

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
shareOut :: MonadThrow m => [[a]] -> [b] -> m [[b]]
shareOut [] _ = return []
shareOut (p:ps) bs = 
    do (l, r) <- trySplitAt (length p) bs
       ls <- shareOut ps r
       return (l : ls)

-- set operations on lists
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
intersect :: Eq a => [a] -> [a] -> [a]
intersect = L.intersect

-- | Removes an item from a list.  A re-export of 'L.delete'.
delete :: Eq a => a -> [a] -> [a]
delete = L.delete 

-- | Subtracts one list from the other.  A re-export of 'L.\\'.
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = (L.\\)

-- | Tests if the first list is a subset of the second.
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

-- | A test for set equality using 'subset'.
setEq :: Eq a => [a] -> [a] -> Bool
setEq l1 l2 = subset l1 l2 && subset l2 l1

-- | Converts a list to a set by removing duplicates.  A re-export of 'L.nub'.
nub :: Eq a => [a] -> [a]
nub = L.nub

-- | A version of 'nub' where the resultant list is sorted.
setify :: Ord a => [a] -> [a]
setify = L.sort . nub

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

-- | Constructs a 'Rational' from two 'Integer's.  A re-export of (R.%).
(%) :: Integer -> Integer -> Rational
(%) = (R.%)

{-| 
  Converts a real number to a rational representation.  
  An alias to 'toRational' for HOL users more familiar with this name.
-}
numdom :: Rational -> (Integer, Integer)
numdom r = 
    (R.numerator r, R.denominator r)

-- | Returns the numerator of a rational number.  A re-export of 'R.numerator'.
numerator :: Rational -> Integer
numerator = R.numerator

{-| 
  Returns the denominator of a rational number.  A re-export of 'R.denominator'.
-}
denominator :: Rational -> Integer
denominator = R.denominator

{-|
  Converts a 'String' representation of a number to an appropriate instance of
  the 'Num' class.  Fails with 'Nothing' if the conversion cannot be performed.

  Note:  The following prefixes are valid:

  * @0x@ - number read as a hexidecimal value

  * @0b@ - number read as a binary value

  * Any other prefix causes the number to be read as a decimal value
-}
numOfString :: forall m a. (MonadThrow m, Eq a, Num a) => String -> m a
numOfString s =
    case res of
      [(x, "")] -> return x
      _ -> fail' "numOfString"
   where res :: [(a, String)]
         res = case s of
                 ('0':'x':s') -> readHex s'
                 ('0':'b':s') -> readInt 2 (`elem` ("01"::String)) digitToInt s'
                 _ -> readDec s

-- Polymorphic, finite, partial functions via Patricia trees
{-| 'Func' is a Patricia Tree representation of polymorphic, finite, partial 
    functions.
-}
data Func a b 
    = Empty
    | Leaf !Int ![(a, b)]
    | Branch !Int !Int !(Func a b) !(Func a b) deriving Eq

-- | An empty 'Func' tree.
funcEmpty :: Func a b
funcEmpty = Empty

-- | The predicate for empty 'Func' trees.
isEmpty :: Func a b -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | A version of 'map' for 'Func' trees.
funcMap :: (b -> c) -> Func a b -> Func a c
funcMap _ Empty = Empty
funcMap f (Leaf h l) = Leaf h $ map (A.second f) l
funcMap f (Branch p b l r) = Branch p b (funcMap f l) $ funcMap f r

-- | A version of 'foldl' for 'Func' trees.
funcFoldl :: (c -> a -> b -> c) -> c -> Func a b -> c
funcFoldl _ a Empty = a
funcFoldl f a (Leaf _ l) =
    let (xs, ys) = unzip l
        Just res = foldl2 f a xs ys in res
funcFoldl f a (Branch _ _ l r) = funcFoldl f (funcFoldl f a l) r

-- | A version of 'foldr' for 'Func' trees.
funcFoldr :: (a -> b -> c -> c) -> c -> Func a b -> c
funcFoldr _ a Empty = a
funcFoldr f a (Leaf _ l) =
    let (xs, ys) = unzip l
        Just res = foldr2 f a xs ys in res
funcFoldr f a (Branch _ _ l r) = funcFoldr f (funcFoldr f a r) l

-- | Converts a 'Func' tree to a sorted association list.
graph :: (Ord a, Ord b) => Func a b -> [(a, b)]
graph f = setify $ funcFoldl (\ a x y -> ((x, y):a)) [] f
 
-- | Converts a 'Func' tree to a sorted list of domain elements.
dom :: Ord a => Func a b -> [a]
dom f = setify $ funcFoldl (\ a x _ -> x:a) [] f

-- | Converts a 'Func' tree to a sorted list of range elements.
ran :: Ord b => Func a b -> [b]
ran f = setify $ funcFoldl (\ a _ y -> y:a) [] f

-- | Application for 'Func' trees.
applyd :: forall m a b. (Monad m, Hashable a, Ord a) 
       => Func a b -> (a -> m b) -> a -> m b
applyd Empty d x = d x
applyd (Branch p b l r) d x
    | (k `xor` p) .&. (b - 1) == 0 =
        applyd (if k .&. b == 0 then l else r) d x
    | otherwise = d x
  where k = hash x
applyd (Leaf h l) d x
    | h == hash x = applydRec l
    | otherwise = d x
  where applydRec :: (Monad m, Ord a) => [(a, b)] -> m b
        applydRec [] = d x
        applydRec ((a, b):t)
            | x == a = return b
            | x > a = applydRec t
            | otherwise = d x

-- | Guarded application for 'Func' trees.
apply :: (MonadThrow m, Hashable a, Ord a) => Func a b -> a -> m b
apply f = applyd f (\_ -> fail' "apply")

-- | Application for 'Func' trees with a default value.
tryApplyd :: (Hashable a, Ord a) => Func a b -> a -> b -> b
tryApplyd f a d = 
    case runCatch $ applyd f (\ _ -> return d) a of
      Right res -> res
      _         -> d

-- | Predicate for testing if a value is defined in a 'Func' tree.
defined ::(Hashable a, Ord a) => Func a b -> a -> Bool
defined f x = 
    case runCatch $ apply f x of
      Right{} -> True
      _       -> False

-- | Undefine a value in a 'Func' tree.
undefine :: forall a b. (Hashable a, Ord a, Eq b) => a -> Func a b -> Func a b
undefine _ Empty = Empty
undefine x t@(Branch p b l r)
    | hash x .&. b == 0 = 
        let l' = undefine x l in
          if l' == l then t
          else case l' of
                 Empty -> r
                 _ -> Branch p b l' r
    | otherwise =
        let r' = undefine x r in
          if r' == r then t
          else case r' of
                 Empty -> l
                 _ -> Branch p b l r'
undefine x t@(Leaf h l)
    | h == hash x =
        let l' = undefineRec l in
          if l' == l then t
          else if null l' then Empty
               else Leaf h l'
    | otherwise = t
  where undefineRec :: [(a, b)] -> [(a, b)]
        undefineRec [] = []
        undefineRec l'@(ab@(a, _):xs)
            | x == a = xs
            | x < a = l'
            | otherwise =
                let xs' = undefineRec xs in
                  if xs' == xs then l' else ab:xs'

newBranch :: Int -> Func a b -> Int -> Func a b -> Func a b
newBranch p1 t1 p2 t2 =
    let zp = p1 `xor` p2
        b = zp .&. (-zp)
        p = p1 .&. (b - 1) in
      if p1 .&. b == 0 then Branch p b t1 t2
      else Branch p b t2 t1

-- | Insert or update an element in a 'Func' tree.
(|->) :: forall a b. (Hashable a, Ord a) => a -> b -> Func a b -> Func a b
(x |-> y) Empty = Leaf (hash x) [(x, y)]
(x |-> y) t@(Branch p b l r)
    | k .&. (b - 1) /= p =
        newBranch p t k $ Leaf k [(x, y)]
    | k .&. b == 0 = Branch p b ((x |-> y) l) r
    | otherwise = Branch p b l $ (x |-> y) r
  where k = hash x
(x |-> y) t@(Leaf h l)
    | h == k = Leaf h $ defineRec l
    | otherwise = 
        newBranch h t k $ Leaf k [(x, y)]
  where k = hash x
       
        defineRec :: [(a, b)] -> [(a, b)]
        defineRec [] = [(x, y)]
        defineRec l'@(ab@(a, _):xs)
            | x == a = (x, y):xs
            | x < a = (x, y):l'
            | otherwise = ab:defineRec xs

-- | Combines two 'Func' trees.
combine :: forall a b. Ord a => (b -> b -> b) -> (b -> Bool) 
        -> Func a b -> Func a b -> Func a b 
combine _ _ Empty t2 = t2
combine _ _ t1 Empty = t1
combine op z lf@(Leaf k _) br@(Branch p b l r)
    | k .&. (b - 1) == p =
        if k .&. b == 0
        then case combine op z lf l of
               Empty -> r
               l' -> Branch p b l' r
        else case combine op z lf r of
               Empty -> l
               r' -> Branch p b l r'
    | otherwise = newBranch k lf p br
combine op z br@(Branch p b l r) lf@(Leaf k _)
    | k .&. (b - 1) == p =
        if k .&. b == 0
        then case combine op z l lf of
               Empty -> r
               l' -> Branch p b l' r
        else case combine op z r lf of
               Empty -> l
               r' -> Branch p b l r'
    | otherwise = newBranch p br k lf
combine op z t1@(Branch p1 b1 l1 r1) t2@(Branch p2 b2 l2 r2)
    | b1 < b2 =
        if p2 .&. (b1 - 1) /= p1 then newBranch p1 t1 p2 t2
        else if p2 .&. b1 == 0
             then case combine op z l1 t2 of
                    Empty -> r1
                    l -> Branch p1 b1 l r1
             else case combine op z r1 t2 of
                    Empty -> l1
                    r -> Branch p1 b1 l1 r
    | b2 < b1 =
        if p1 .&. (b2 - 1) /= p2 then newBranch p1 t1 p2 t2
        else if p1 .&. b2 == 0
             then case combine op z t1 l2 of
                    Empty -> r2
                    l -> Branch p2 b2 l r2
             else case combine op z t1 r2 of
                    Empty -> l2
                    r -> Branch p2 b2 l2 r
    | p1 == p2 =
       case (combine op z l1 l2, combine op z r1 r2) of
         (Empty, r) -> r
         (l, Empty) -> l
         (l, r) -> Branch p1 b1 l r
    | otherwise = newBranch p1 t1 p2 t2
combine op z t1@(Leaf h1 l1) t2@(Leaf h2 l2)
    | h1 == h2 =
        let l = combineRec l1 l2 in
          if null l then Empty else Leaf h1 l
    | otherwise = newBranch h1 t1 h2 t2
  where combineRec :: [(a, b)] -> [(a, b)] -> [(a, b)]
        combineRec [] ys = ys
        combineRec xs [] = xs
        combineRec xs@(xy1@(x1, y1):xs') ys@(xy2@(x2, y2):ys')
            | x1 < x2 = xy1:combineRec xs' ys
            | x1 > x2 = xy2:combineRec xs ys'
            | otherwise =
                let y = op y1 y2
                    l = combineRec xs' ys' in
                  if z y then l else (x1, y):l

-- | Special case of '(|->)' applied to 'funcEmpty'.
(|=>) :: (Hashable a, Ord a) => a -> b -> Func a b
x |=> y = (x |-> y) funcEmpty

-- | Selects an arbitrary element from a 'Func' tree.
choose :: MonadThrow m => Func a b -> m (a, b)
choose Empty = fail' "choose"
choose (Leaf _ l) = tryHead l
choose (Branch _ _ t1 _) = choose t1

-- Maps
-- | A re-export of 'Map.empty'.
mapEmpty :: Map a b
mapEmpty = Map.empty

-- | A re-export of 'Map.insert'.
mapInsert :: Ord a => a -> b -> Map a b -> Map a b
mapInsert = Map.insert

-- | A re-export of 'Map.union'.
mapUnion :: Ord k => Map k a -> Map k a -> Map k a
mapUnion = Map.union

-- | A re-export of 'Map.delete'.
mapDelete :: Ord k => k -> Map k a -> Map k a
mapDelete = Map.delete

-- | A guarded re-export of 'Map.lookup'.
mapAssoc :: (MonadThrow m, Ord a) => a -> Map a b -> m b
mapAssoc x = maybeToFail "mapAssoc" . Map.lookup x

-- | A re-export of 'Map.elems'.
mapElems :: Map k a -> [a]
mapElems = Map.elems

-- | A re-export of 'Map.fromList'.
mapFromList :: Ord a => [(a, b)] -> Map a b
mapFromList = Map.fromList

-- | A re-export of 'Map.toList'.
mapToList :: Ord a => Map a b -> [(a, b)]
mapToList = Map.toList

-- | A re-export of 'Map.map'.
mapMap :: (a -> b) -> Map k a -> Map k b
mapMap = Map.map

-- | A re-export of 'Map.foldrWithKey'.
mapFoldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
mapFoldrWithKey = Map.foldrWithKey

-- | A version of 'remove' for 'Map's based on 'Map.updateLookupWithKey'.
mapRemove :: (MonadThrow m, Ord k) => k -> Map k a -> m (a, Map k a)
mapRemove x m = 
    let (child, map') = Map.updateLookupWithKey (\ _ _ -> Nothing) x m in
      case child of
        Nothing -> fail' "mapRemove"
        Just x' -> return (x', map')

-- | A re-export of 'Map.toAscList'.
mapToAscList :: Map k a -> [(k, a)]
mapToAscList = Map.toAscList

-- | A re-export of 'Text.append'.
append :: Text -> Text -> Text
append = Text.append

-- | A re-export of 'Text.cons'.
cons :: Char -> Text -> Text
cons = Text.cons

-- | A re-export of 'Text.snoc'.
snoc :: Text -> Char -> Text
snoc = Text.snoc

-- | A re-export of 'Text.pack'.
pack :: String -> Text
pack = Text.pack

-- | A re-export of 'Text.unpack'.
unpack :: Text -> String
unpack = Text.unpack

-- | A re-export of 'Text.head'.
textHead :: Text -> Char
textHead = Text.head

-- | A re-export of 'Text.tail'.
textTail :: Text -> Text
textTail = Text.tail

-- | A re-export of 'Text.null'.
textNull :: Text -> Bool
textNull = Text.null

-- | A re-export of 'Text.empty'.
textEmpty :: Text
textEmpty = Text.empty

-- | A re-export of 'Text.strip'.
textStrip :: Text -> Text
textStrip = Text.strip

-- | A re-export of 'Text.show'.
textShow :: Text.TextShow a => a -> Text
textShow = Text.fromStrict . Text.showt

-- Acid State re-exports

-- | A re-export of 'State.get' from @Control.Monad.State@.
get :: State.MonadState s m => m s
get = State.get

-- | A re-export of 'State.put' from @Control.Monad.State@.
put :: State.MonadState s m => s -> m ()
put = State.put

-- | A re-export of 'Reader.ask' from @Control.Monad.Reader@.
ask :: Reader.MonadReader r m => m r
ask = Reader.ask

-- | A re-export of 'Acid.makeAcidic' from @Data.Acid@.
makeAcidic :: Name -> [Name] -> Q [Dec]
makeAcidic = Acid.makeAcidic

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
    _NO = _FAIL "_NO"

    -- | A primitive language operation that always succeeds.
    _ALL :: a
    {-| 
      A language combinator for branching based on failure.  The language
      equivalent of the '<|>' operator.
    -}
    _ORELSE :: a -> a -> a

    -- | A language combinator that performs the first operation in a list.
    _FIRST :: [a] -> a
    _FIRST [] = _FAIL "_FIRST: empty list"
    _FIRST xs = foldr1 _ORELSE xs

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
    _TRY x = x `_ORELSE` _ALL

    {-|
      A language combinator that annotates the wrapped operation with a provided
      'String'.  The language equivalent of 'note''.
    -}
    _NOTE :: String -> a -> a


{-|
  The 'LangSeq' class defines common language operations and combinators based
  on sequencing.  See the note at the top of this section for more details as
  to why these are separated on their own.
-}
class Lang a => LangSeq a where
    -- | A language combinator that sequences operations.
    _THEN :: a -> a -> a
    {-| 
      A language combinator that repeatedly applies a language operation until 
      failure.
    -}
    _REPEAT :: a -> a
    _REPEAT x = (x `_THEN` _REPEAT x) `_ORELSE` _ALL
    {-| 
      A language combinator that performs every operation in a list  
      sequentially.
    -}
    _EVERY :: [a] -> a
    _EVERY = foldr _THEN _ALL
