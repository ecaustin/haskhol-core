{-# LANGUAGE ConstraintKinds, DataKinds, EmptyDataDecls, ScopedTypeVariables, 
             TypeFamilies, TypeOperators, UndecidableInstances #-}
{-|
  Module:    HaskHOL.Core.State.Monad
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module exports the primitive types and combinators for the 'HOL' 
  computational monad.  At a high level this monad is a newtype wrapper for a
  limited 'IO' monad.

  For higher level monadic combinators see the "HaskHOL.Core.State" and
  "HaskHOL.Core.Basics" modules.
-}
module HaskHOL.Core.State.Monad
    ( -- * The 'HOL' Monad
      HOL
    , Theory
    , Proof
    , runHOLProof
    , runHOL
      -- * Theory Contexts
    , TheoryPath
    , mkTheoryPath
    , destTheoryPath
    , BaseThry(..)
    , ExtThry(..)
    , CtxtName(..)
    , PolyTheory
    , BaseCtxt
    , ctxtBase
      -- * Text Output Methods
    , putStrHOL
    , putStrLnHOL
      -- * Exception Handling Methods
    , HOLException(..)
    , throwHOL
    , catchHOL
    , noteHOL
    , liftMaybe
    , liftEither
    , finallyHOL
      -- * Local Reference Methods
    , HOLRef
    , newHOLRef
    , readHOLRef
    , writeHOLRef
    , modifyHOLRef
      -- * Acid State Primitives
    , openLocalStateHOL
    , openLocalStateHOLBase
    , closeAcidStateHOL
    , createCheckpointAndCloseHOL
    , cleanArchives
    , updateHOL
    , updateHOLUnsafe
    , queryHOL
      -- * Benign Flag Methods
    , setBenignFlag
    , unsetBenignFlag
    , getBenignFlag
    , newFlag
      -- * Methods Related to Fresh Name Generation
    , tickTermCounter
    , tickTypeCounter
      -- * Proof Caching
    , cacheProof
    , cacheProofs
    , checkpointProofs
    , cleanArchiveProofs
      -- * Re-export for Extensible Exceptions
    , Exception
    ) where

import HaskHOL.Core.Lib hiding (combine)
import HaskHOL.Core.Kernel.Prims

-- HOL Monad imports
import Data.Typeable
import Control.Exception (Exception)
import qualified Control.Exception as E
import Data.IORef
import GHC.Prim (Constraint)
import qualified Data.HashMap.Strict as Hash
import Data.Hashable
import Data.Acid hiding (makeAcidic, Query, Update)

-- TH imports
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

-- Path Handling imports
import Prelude hiding (FilePath)
import Paths_haskhol_core
import Shelly hiding (put, get)
import System.FilePath (combine)

-- Messy Template Haskell stuff
-- Proofs
data Proofs = Proofs !(Hash.HashMap String HOLThm) deriving Typeable

instance (SafeCopy a, SafeCopy b, Hashable a, Eq a) => 
         SafeCopy (Hash.HashMap a b) where
    getCopy = contain $ fmap Hash.fromList safeGet
    putCopy = contain . safePut . Hash.toList

deriveSafeCopy 0 'base ''Proofs

insertProof :: String -> HOLThm -> Update Proofs ()
insertProof lbl thm =
    do (Proofs m) <- get
       put (Proofs (Hash.insert lbl thm m))

getProof :: String -> Query Proofs (Maybe HOLThm)
getProof lbl =
    do (Proofs m) <- ask
       return $! Hash.lookup lbl m

makeAcidic ''Proofs ['insertProof, 'getProof]
--
-- Types
data TyCounter = TyCounter !Integer deriving Typeable

deriveSafeCopy 0 'base ''TyCounter

updateTyCounter :: Update TyCounter Integer
updateTyCounter =
    do TyCounter n <- get
       let n' = succ n
       put (TyCounter n')
       return n'

queryTyCounter :: Query TyCounter Integer
queryTyCounter =
    do TyCounter n <- ask
       return n

makeAcidic ''TyCounter ['updateTyCounter, 'queryTyCounter]
--
-- Terms
data TmCounter = TmCounter !Integer deriving Typeable

deriveSafeCopy 0 'base ''TmCounter

updateTmCounter :: Update TmCounter Integer
updateTmCounter =
    do TmCounter n <- get
       let n' = succ n
       put (TmCounter n')
       return n'

queryTmCounter :: Query TmCounter Integer
queryTmCounter =
    do TmCounter n <- ask
       return n

makeAcidic ''TmCounter ['updateTmCounter, 'queryTmCounter]
--
-- Flags
data BenignFlags = BenignFlags !(Map String Bool) deriving Typeable

deriveSafeCopy 0 'base ''BenignFlags

insertFlag :: String -> Bool -> Update BenignFlags ()
insertFlag ty flag = 
    do BenignFlags m <- get
       put (BenignFlags (mapInsert ty flag m))

lookupFlag :: String -> Query BenignFlags (Maybe Bool)
lookupFlag ty =
    do BenignFlags m <- ask
       return (mapLookup ty m)
makeAcidic ''BenignFlags ['insertFlag, 'lookupFlag]
--
--

-- Monad
-- HOL method types
{-|
  The 'HOL' monad structures computations in the HaskHOL system at the stateful
  layer and above.  The type parameters are used as such:

  * @cls@ - 'HOL' computations are split into two classes, those that extend the
            current working theory and those that are \"pure\"-ly used for
            proof.  The @cls@ parameter is used to indicate the classification
            of a computation.  It is a phantom type variable that is inhabited
            by one of two empty data types, 'Theory' and 'Proof'.

  * @thry@ - Carries a tag indicating the most recent checkpoint of the current
             working theory, i.e. the last library loaded.  Again, it is a
             phantom
             type variable that is inhabited by an empty data type.  A unique
             tag is created for each library by linerearly extending the tag
             starting from a base value. For example, the tag 
             @ExtThry EqualThry BaseThry@ would indicate a current working
             theory consisting of the base and equality logic theories.

             Note that typically this value is left polymorphic and is
             constrained by a type class related to a library.  For example, the
             following type indicates a computation that can only be ran by
             using a theory context value that has the equality logic library
             loaded:  @EqualCtxt thry => HOL cls thry a@

  * @a@ - The return type of a 'HOL' computation.

  Note that the 'HOL' monad is essentially a newtype wrapper to a limited
  'IO' monad.  We say limited as we restrict the possible
  IO-like computations to the ones exported in this module, rather than allowing
  arbitrary computations through a mechanism like 'MonadIO'.  This prevents a
  number of soundness issues.
-}
newtype HOL cls thry a = 
    HOL { -- not exposed to the user
          runHOLUnsafe :: AcidState Proofs -> String -> IO a
        }

-- | The classification tag for theory extension computations.
data Theory
-- | The classification tag for proof computations.
data Proof

-- used internally by runHOLProof and runHOL
runHOLUnsafe' :: HOL cls thry a -> String -> IO a
runHOLUnsafe' m fp =
    do dir <- getDataDir 
       acid <- openLocalStateFrom (dir `combine` "Proofs") (Proofs Hash.empty)
       runHOLUnsafe m acid fp `E.finally` closeAcidState acid

{-| 
  Runs a 'HOL' 'Proof' computation using a provided 'TheoryPath' with access to
  the proof cache. Other state values, e.g. type and term constants, may be 
  accessed but not modified.
-}
runHOLProof :: HOL Proof thry a -> TheoryPath thry -> IO a
runHOLProof m (TheoryPath fp) = runHOLUnsafe' m fp

{-| 
  Evaluates a 'HOL' computation by copying the contents of a provided 
  'TheoryPath' to a new directory where destructive updates will occur.
  This is used primarily by 'extendTheory', but is also useful for testing 
  'Theory' computations in temporary directories.
-}
runHOL :: HOL cls thry a -> TheoryPath thry -> String -> IO a
runHOL m (TheoryPath old) new =
    do dir <- getDataDir
       let old' = mkFilePath $ dir `combine` old
           new' = mkFilePath $ dir `combine` new
       shelly $ do unlessM (test_d old') . fail $ 
                     "runHOL: acid-state directory, " ++
                     old ++ ", does not exist."
                   whenM (test_d new') . echo . pack $
                     "runHOL: acid-state directory, " ++
                     new ++ ", already exists.  Overwriting."
                   rm_rf new'
                   cp_r old' new'
       runHOLUnsafe' m new

instance Functor (HOL cls thry) where
    fmap = liftM
    
instance Monad (HOL cls thry) where
    return x = HOL $ \ _ _ -> return x
    m >>= k = HOL $ \ acid st -> 
        do b <- runHOLUnsafe m acid st
           runHOLUnsafe (k b) acid st
    fail = throwHOL . HOLException

instance MonadPlus (HOL cls thry) where
    mzero = fail "mzero - HOL"
    mplus = (<||>)

instance Applicative (HOL cls thry) where
    pure = return
    (<*>) = ap

instance Alternative (HOL cls thry) where
    empty = fail "empty - HOL"
    (<|>) = (<||>)

instance Note (HOL cls thry) where
   job <?> str = job <|> throwHOL (HOLException str)


-- Theory Contexts
{-| 
  A @newtype@ wrapper for the filepath to a theory context that uses a phantom
  type variable to capture the theory context type.  See 'HOL' for more 
  information.
-}
newtype TheoryPath thry = TheoryPath String

{-| Constructs a 'TheoryPath' for a provided theory context type using 
    'ctxtName'.
-}
mkTheoryPath :: forall thry. CtxtName thry => TheoryPath thry
mkTheoryPath = TheoryPath $ ctxtName (undefined :: thry)

-- | Destructs a 'TheoryPath', returning its internal 'String'.
destTheoryPath :: TheoryPath thry -> String
destTheoryPath (TheoryPath x) = x

-- converts TheoryPath strings to FilePaths for shelly
mkFilePath :: String -> FilePath
mkFilePath = fromText . pack


-- | The 'BaseThry' type is the type of the initial working theory.
data BaseThry = BaseThry
{-| 
  The 'ExtThry' type is the type of a linear theory extension, i.e. a cons-like
  operation for theory types.  See the module "HaskHOL.Lib.Equal.Context" for
  an example of how to correctly define theory types and contexts for a library.
-}
data ExtThry a b = ExtThry a b

{-|
  The 'CtxtName' class associates a 'String' representation of context names
  with context types.  It's used internally for most Template Haskell primitives
  defined in @HaskHOL.Core.Ext@ and its submodules.
-}
class CtxtName a where
    -- | Returns the name of a context type.
    ctxtName :: a -> String

{-| 
  The 'PolyTheory' type family is used to build a polymorphic theory context 
  constraint from a monomorphic theory context type.  For example: 

  > type instance PolyTheory BoolType b = BaseCtxt b

  Note:  This will be automated and appropriately hidden in the future, but for
  now it must be manually defined.
  Be extra careful not to define incorrect instances as they can be used to
  bypass the safety mechanisms of 'cacheProof' and 'cacheProofs'.
-}
type family PolyTheory a b :: Constraint

instance CtxtName BaseThry where
    ctxtName _ = "BaseCtxt"

instance CtxtName a => CtxtName (ExtThry a b) where
    ctxtName _ = ctxtName (undefined::a)

{-|
  The 'BaseCtxt' class is the context name associated with the 'BaseThry' type,
  i.e. the constraint to be used to guarantee that the stateful kernel has been
  loaded.  This should always be true.
-}
type family BaseCtxt a :: Constraint where
    BaseCtxt BaseThry = ()
    BaseCtxt (ExtThry a b) = BaseCtxt b

-- type family stuff for base contexts
type instance PolyTheory BaseThry b = BaseCtxt b

type instance BaseThry == BaseThry = 'True
type instance ExtThry a b == ExtThry a' b' = (a == a') && (b == b')

-- | The 'TheoryPath' for the base theory context.
ctxtBase :: TheoryPath BaseThry
ctxtBase = mkTheoryPath


-- define own versions of IO functions so they can be used external to kernel

-- | A version of 'putStr' lifted to the 'HOL' monad.
putStrHOL :: String -> HOL cls thry ()
putStrHOL x = HOL $ \ _ _ -> putStr x

-- | A version of 'putStrLn' lifted to the 'HOL' monad.
putStrLnHOL :: String -> HOL cls thry ()
putStrLnHOL x = HOL $ \ _ _ -> putStrLn x

-- Errors
-- the basic HOL exception type
-- | The data type for generic errors in HaskHOL.  Carries a 'String' message.
newtype HOLException = HOLException String deriving Typeable
instance Show HOLException where show (HOLException str) = str
instance Exception HOLException

{-| 
  A version of 'throwIO' lifted to the 'HOL' monad.  

  Note that the following functions for the 'HOL' type rely on 'throwHOL':
 
  * 'fail' - Equivalent to 

    > throwHOL . HOLException

  * 'mzero' - Equivalent to 

    > fail "mzero - HOL"

  * 'empty' - Equivalent to 

    > fail "empty - HOL"
-}
throwHOL :: Exception e => e -> HOL cls thry a
throwHOL x = HOL $ \ _ _ -> E.throwIO x

{-| 
  A version of 'E.catch' lifted to the 'HOL' monad.

  Note that 'mplus' and '<|>' are defined in terms of catching a 
  'E.SomeException' with 'catchHOL' and then ignoring it to run an alternative
  computation instead.
-}
catchHOL :: Exception e => HOL cls thry a -> (e -> HOL cls thry a) -> 
                           HOL cls thry a
catchHOL job errcase = HOL $ \ acid st -> 
    runHOLUnsafe job acid st `E.catch` \ e -> runHOLUnsafe (errcase e) acid st

-- Used to define mplus and (<|>) for the HOL monad.  Not exposed to the user.
(<||>) :: HOL cls thry a -> HOL cls thry a -> HOL cls thry a
job <||> alt = HOL $ \ acid st -> 
   runHOLUnsafe job acid st `E.catch` 
     \ (_ :: E.SomeException) -> runHOLUnsafe alt acid st

-- | A version of 'note' specific to 'HOL' computations.
noteHOL :: String -> HOL cls thry a -> HOL cls thry a
noteHOL str m = HOL $ \ acid st ->
    runHOLUnsafe m acid st `E.catch` \ (e :: E.SomeException) ->
        case E.fromException e of
          Just (HOLException str2) -> 
              E.throwIO . HOLException $ str ++ ": " ++ str2
          _ -> E.throwIO . HOLException $ str ++ ": " ++ show e

{-| 
  Lifts a 'Maybe' value into the 'HOL' monad mapping 'Just's to 'return's and
  'Nothing's to 'fail's with the provided 'String'.
-}
liftMaybe :: String -> Maybe a -> HOL cls thry a
liftMaybe _ (Just x) = return x
liftMaybe str _ = fail str 

{-|
  Lifts an 'Either' value into the 'HOL' monad mapping 'Right's to 'return's
  and 'Left's to 'fail's.  

  Note that the value inside the 'Left' must have an instance of the 'Show' 
  class such that 'show' can be used to construct a string to be used with
  'fail'.
-}
liftEither :: Show err => String -> Either err a -> HOL cls thry a
liftEither _ (Right res) = return res
liftEither str1 (Left str2) = fail $ str1 ++ ": " ++ show str2

-- | A version of 'E.finally' lifted to the 'HOL' monad.
finallyHOL :: HOL cls thry a -> HOL cls thry b -> HOL cls thry a
finallyHOL a b = HOL $ \ acid st ->
    runHOLUnsafe a acid st `E.finally` runHOLUnsafe b acid st

-- Local vars
-- | A type synonym for 'IORef'.
type HOLRef = IORef

{-| 
  Creates a new 'HOLRef' from a given starting value.  Functionally equivalent
  to 'newIORef' lifted to the 'HOL' monad.
-}
newHOLRef :: a -> HOL cls thry (HOLRef a)
newHOLRef ref = HOL $ \ _ _ -> newIORef ref

{-|
  Reads a 'HOLRef' returning the stored value.  Functionally equivalent to 
  'readIORef' lifted to the 'HOL' monad.
-}
readHOLRef :: IORef a -> HOL cls thry a
readHOLRef ref = HOL $ \ _ _ -> readIORef ref

{-|
  Writes a value to a 'HOLRef'.  Functionally equivalent to 'writeHOLRef' lifted
  to the 'HOL' monad.
-}
writeHOLRef :: IORef a -> a -> HOL cls thry ()
writeHOLRef ref x = HOL $ \ _ _ -> writeIORef ref x

{-|
  Applies a given function to a 'HOLRef', modifying the stored value.
  Functionally equivalent to 'modifyHOLRef' lifted to the 'HOL' monad.
-}
modifyHOLRef :: IORef a -> (a -> a) -> HOL cls thry ()
modifyHOLRef ref f = HOL $ \ _ _ -> modifyIORef ref f

-- acid primitives

-- used internally by openLocalStateHOL and openLocalStateHOLBase
openLocalState' :: (Typeable st, IsAcidic st) 
                => st -> String -> IO (AcidState st)
openLocalState' ast suf =
    do dir <- getDataDir
       let dir' = dir `combine` suf `combine` show (typeOf ast)
       openLocalStateFrom dir' ast


{-| 
  Creates an 'AcidState' value from a 'HOL' computation's theory context using
  a default value.
  This is a wrapper to 'openLocalStateFrom' using @HaskHOL.Core@'s 
  shared data directory appended with the 'ctxtName' of the theory context.
-}
openLocalStateHOL :: (Typeable st, IsAcidic st) => st 
                  -> HOL cls thry (AcidState st)
openLocalStateHOL ast = HOL $ \ _ st -> openLocalState' ast st

{-| 
  A version of 'openLocalStateHOL' that uses just @HaskHOL.Core@'s shared data
  directory.
-}
openLocalStateHOLBase :: (Typeable st, IsAcidic st) => st 
                      -> HOL cls thry (AcidState st)
openLocalStateHOLBase ast = HOL $ \ _ _ -> openLocalState' ast ""

-- | A wrapper to 'closeAcidState' for the 'HOL' monad.
closeAcidStateHOL :: (SafeCopy st, Typeable st) => AcidState st 
                  -> HOL cls thry ()
closeAcidStateHOL ast = HOL $ \ _ _ -> closeAcidState ast

{-| 
  A version of 'closeAcidStateHOL' that calls 'createCheckpoint' and 
  'createArchive' first. -}
createCheckpointAndCloseHOL :: (SafeCopy st, Typeable st) => AcidState st 
                            -> HOL cls thry ()
createCheckpointAndCloseHOL ast = HOL $ \ _ _ ->
    do createCheckpoint ast
       createArchive ast
       closeAcidState ast

{-| 
  The 'cleanArchives' method removes all of the "Archive" sub-directories 
  in a theory context that were created by 'createCheckpointAndCloseHOL'.
-}
cleanArchives :: HOL cls thry ()
cleanArchives = HOL $ \ _ st ->
    do dir <- getDataDir
       let dir' = mkFilePath $ dir `combine` st
       shelly $ do archvs <- findWhen (\ x -> return $! x == "Archive") dir'
                   mapM_ rm_rf archvs

{-|
  A wrapper to 'update' for the 'HOL' monad.  Note that the classification of
  the provided 'HOL' computation is unrestricted, such that it can be used in 
  'Proof' computations to update benign state values.

  If you want the state modification to be captured by the type, make sure to
  use 'updateHOL' instead.
-}
updateHOLUnsafe :: UpdateEvent event => AcidState (EventState event) -> event 
                -> HOL cls thry (EventResult event)
updateHOLUnsafe ast e = HOL $ \ _ _ -> update ast e

{-| 
  A version of 'updateHOLUnsafe' that restricts the classification of the 
  provided 'HOL' computation to be of a 'Theory' type.
-}
updateHOL :: UpdateEvent event => AcidState (EventState event) -> event 
          -> HOL Theory thry (EventResult event)
updateHOL = updateHOLUnsafe

-- | A wrapper to 'query' for the 'HOL' monad.
queryHOL :: QueryEvent event => AcidState (EventState event) -> event 
         -> HOL cls thry (EventResult event)
queryHOL ast e = HOL $ \ _ _ -> query ast e


-- Flag Methods

{-|
  HOL systems typically use a large number of boolean flags in order to direct
  system behavior, i.e. debug flags, warning flags, parser/printer flags, etc.
  These flags don't affect the underlying proof computations, hence their
  classification as benign, so we'd like to be able to toggle them on and off
  at will.  Unfortunately, if we store them in the extensible state and use 
  'putExt' or 'modifyExt' we're limited to only being able to change them in
  'Theory' computations.  

  Instead, we include them in a separate part of the theory context where we 
  can interact with them in any way we want without sacrificing the safety of 
  the extensible state portion of the context.

  The 'BenignFlag' class works very similarly to the 'ExtClass' class with the
  obvious exception that initial values are restricted to boolean values.

  See 'getBenignFlagCtxt' and 'setBenignFlag' for more details.
-}
class Typeable a => BenignFlag a where
    {-| 
      The intial value for a benign flag.  The value returned when attempting to
      retrieve a flag that is not yet defined in the context.
    -}
    initFlagValue :: a -> Bool

-- Benign Flag methods

-- Converts a flag's type to the string representation of its full name
tyToIndex :: forall a. Typeable a => a -> String
tyToIndex _ =
    let con = typeRepTyCon $ typeOf (undefined::a) in
      tyConPackage con ++ tyConModule con ++ tyConName con

-- used internally by set/unsetBenignFlag
modBenignFlag :: BenignFlag a => Bool -> a -> HOL cls thry ()
modBenignFlag val flag =
    do acid <- openLocalStateHOL (BenignFlags mapEmpty)
       updateHOLUnsafe acid (InsertFlag (tyToIndex flag) val)
       closeAcidStateHOL acid

{-|
  Adds a new, or modifies an existing, benign flag to be 'True'.  Benign flags 
  in the context are stored as a 'Map' of 'Bool' values with 'String' keys.
  The key is a term-level reificatino of a benign flag's type, 
  produced via a composition of 'show' and 'typeOf'.

  Numerous usage examples can be found in both the "HaskHOL.Core.Parser.Lib" and
  "HaskHOL.Core.Printer" modules where flags are used to direct the behavior
  of the parsers and printers accordingly.

  Note that since the retrieval and storage of benign flags are driven by types,
  it is in the best interest of library implementors to guarantee that the types
  of their flags are unique.  The easiest way to do this is to create a unique
  @data@ type for each flag.  The type doesn't need to carry a payload, but it
  does need to provide a witness to the flag type.  As such, it can either be
  a nullary, punned data declaration, i.e. @data X = X@, or an empty data 
  declaration with a type annotated instance of 'undefined' acting as the
  witness, i.e. @undefined :: X@.

  Example:

  > setBenignFlag FlagDebug

  would set the debugging flag equal to 'True'.
-}
setBenignFlag :: BenignFlag a => a -> HOL cls thry ()
setBenignFlag = modBenignFlag True

-- | Unsets a benign flag making it 'False'.
unsetBenignFlag :: BenignFlag a => a -> HOL cls thry ()
unsetBenignFlag = modBenignFlag False

{-|
  Retrieves the value of a benign flag from a theory context.
  Note that retrieval of the value requires a witness to the desired flag's
  type, i.e.

  > getBenignFlag FlagDebug

  or

  > getBenignFlag (undefined :: FlagDebug)

  In the event that the flag is not found then the 'initFlagValue' for that type
  is returned. Thus, this function never fails.
-}
getBenignFlag :: BenignFlag a => a -> HOL cls thry Bool
getBenignFlag flag =
    do acid <- openLocalStateHOL (BenignFlags mapEmpty)
       val <- queryHOL acid (LookupFlag (tyToIndex flag))
       closeAcidStateHOL acid
       return $! fromMaybe (initFlagValue flag) val

-- Fresh Name Generation

{-| 
  Increments the term counter stored in the context, returning the new value.
-}
tickTermCounter :: HOL cls thry Integer
tickTermCounter =
    do acid <- openLocalStateHOL (TmCounter 0)
       n <- updateHOLUnsafe acid UpdateTmCounter
       closeAcidStateHOL acid
       return n

{-|
  Increments the type counter stored in the context, returning the new value.
-}
tickTypeCounter :: HOL cls thry Integer
tickTypeCounter =
    do acid <- openLocalStateHOL (TyCounter 0)
       n <- updateHOLUnsafe acid UpdateTyCounter
       closeAcidStateHOL acid
       return n

{-|
  The 'newFlag' splice can be used to automatically construct a new benign flag
  given a name and an initial flag value.

  Example:

  > newFlag "FlagDebug" True

  will construct the following Haskell code:

  > data FlagDebug = FlagDebug deriving Typeable
  > instance BenignFlag FlagDebug where
  >     initFlagValue _ = True
-}
newFlag :: String -> Bool -> Q [Dec]
newFlag flag val =
    do val' <- lift val
       let name = mkName flag
           ty = DataD [] name [] [NormalC name []] [''Typeable]
           cls = InstanceD [] (AppT (ConT ''BenignFlag) (ConT name)) 
                   [FunD 'initFlagValue [Clause [WildP] (NormalB val') []]]
       return [ty, cls]

-- Proof Caching

{-|
  The 'cacheProof' method stores or retrieves a theorem from the proof
  cache.  For example:

  > thmTRUTH :: BoolCtxt thry => HOL cls thry HOLThm
  > thmTRUTH = cacheProof "thmTRUTH" ctxtBool $
  >     do tm <- toHTm [str| \p:bool. p |]
  >        tdef <- defT
  >        liftO . liftM1 primEQ_MP (ruleSYM tdef) $ primREFL tm

  Note that 'cacheProof' takes three arguments:

  * The label for the theorem.  Note that these labels must be unique, such that
    an attempt to store a theorem with a duplicate label will instead retrieve
    the existing theorem.  In the event that the existing theorem requires a
    theory context that is not satisfied by the provided context, the
    appropriate error will be thrown.

  * The theory context required to successfully evaluate the proof of the
    theorem.  Note that this context only needs to satisfy the ascribed type
    constraint, not "match" it.  For example, one could define a set of contexts
    that are identical modulo overload priorities.  Selecting from this set on a
    case by case basis can be used to eliminate a large number of type 
    annotations.

  * The proof computation itself.  Regardless of how the proof is provided as
    an argument, be sure that it's type matches that of the type ascribed to
    the call to 'cacheProof'.
-}
cacheProof :: PolyTheory thry thry' => String -> TheoryPath thry 
           -> HOL Proof thry HOLThm 
           -> HOL cls thry' HOLThm
cacheProof lbl (TheoryPath fp) prf = HOL $ \ acid _ ->
    do qth <- query acid (GetProof lbl)
       case qth of
         Just th -> 
             return th
         Nothing ->
           do putStrLn ("proving: " ++ lbl)
              th <- runHOLUnsafe prf acid fp
              putStrLn (lbl ++ " proved.")
              update acid (InsertProof lbl th)
              return th

{-|
  This is a version of 'cacheProof' that handles proof computations that return
  multiple theorems.  Essentially, it maps 'cacheProof' over
  the list of labels, where each resultant computation retrieves only a single
  theorem, but can store them all.
  
  The decision to return a list of computations was inherited from a previous 
  implementation technique. It can result in some messy code when you want to
  provide top-level names for each computation, but works fairly well otherwise.
-}
cacheProofs :: forall cls thry thry'. PolyTheory thry thry' => [String] 
            -> TheoryPath thry 
            -> HOL Proof thry [HOLThm] 
            -> [HOL cls thry' HOLThm]
cacheProofs lbls (TheoryPath fp) prf = map cacheProofs' lbls
  where cacheProofs' :: String -> HOL cls thry' HOLThm
        cacheProofs' lbl = HOL $ \ acid _ ->
            do qth <- query acid (GetProof lbl)
               case qth of
                 Just th -> 
                   return th
                 Nothing -> 
                   do qths <- liftM catMaybes $ 
                                mapM (query acid . GetProof) lbls
                      unless (null qths) . fail $
                         "cacheProofs: some provided labels clas with " ++
                         "existing theorems."
                      putStrLn ("proving: " ++ unwords lbls)
                      ths <- runHOLUnsafe prf acid fp
                      putStrLn (unwords lbls ++ " proved.")
                      when (length lbls /= length ths) . fail $
                         "cacheProofs: number of labels does not match " ++
                         "number of theorems."
                      mapM_ (\ (x, y) -> update acid (InsertProof x y)) $ 
                                           zip lbls ths
                      let n = fromJust $ elemIndex lbl lbls
                          th = ths !! n
                      return th

{-| 
  Similar to 'createCheckpointAndCloseHOL', this method creates a checkpoint
  and archive for the proofs cache specifically.
-}
checkpointProofs :: HOL cls thry ()
checkpointProofs = HOL $ \ acid _ ->
    do createCheckpoint acid
       createArchive acid

{-| 
  A version of 'cleanArchives' for the proofs cache specifically.
-}
cleanArchiveProofs :: HOL cls thry ()
cleanArchiveProofs = HOL $ \ _ _ ->
    do dir <- liftM mkFilePath getDataDir
       let dir' = dir </> ("Proofs" :: FilePath) </> ("Archive" :: FilePath)
       shelly $ rm_rf dir'
