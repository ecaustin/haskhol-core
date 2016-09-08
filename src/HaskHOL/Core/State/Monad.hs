{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds, DataKinds, DeriveLift, EmptyDataDecls, 
             ExistentialQuantification, FlexibleContexts, GADTs,
             ScopedTypeVariables, TypeFamilies, TypeApplications, 
             TypeOperators, UndecidableInstances 
#-}

{-|
  Module:    HaskHOL.Core.State.Monad
  Copyright: (c) Evan Austin 2015
  LICENSE:   BSD3

  Maintainer:  e.c.austin@gmail.com
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
    , mkProofGeneral
    , runHOLProof
    , runHOLTheory
    , runHOLHint
      -- * Theory Contexts
    , TheoryPath
    , Module
    , thisModule'
    , UnsafeThry(..)
    , BaseThry(..)
    , ExtThry(..)
    , CtxtName(..)
    , PolyTheory
    , BaseCtxt
    , ctxtBase
    , extendTheory
      -- * Parse Contexts
    , ParseContext
    , initParseContext
    , parseContext
    , parseContextCache
    , viewParseContext
    , overParseContext
    , testParseContext
    , binders
    , tyBinders
    , prefixes
    , infixes
    , typeAbbrevs
    , lefts
    , rights
    , hidden
    , interface
    , overloads
    , prebroken
    , termConstants
    , typeConstants
    , unspaced
      -- * Text Output Methods
    , putDocHOL
    , putStrHOL
    , putStrLnHOL
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
    , updateHOL
    , queryHOL
      -- * Benign Flag Methods
    , setBenignFlag
    , unsetBenignFlag
    , getBenignFlag
    , newFlag
      -- * Methods Related to Fresh Name Generation
    , tickTermCounter
    , tickTypeCounter
      -- * Value Caching and Serialization
    , cacheProof
    , unsafeCacheProof
    , cacheProofs
    , getProof
    , Conversion(..)
    , cacheConversion
    , cacheNet
    , cacheThms
    , cacheFlags
    , serializeValue
      -- * Re-export
    , Constraint
    ) where

import HaskHOL.Core.Lib hiding (combine, pack)
import HaskHOL.Core.Basics.Nets
import HaskHOL.Core.Kernel hiding ((:=), typeOf)

-- HOL Monad imports
import Data.Typeable
import qualified Control.Exception as E
import Data.IORef
import GHC.Exts (Constraint)
import qualified Data.HashMap.Strict as Hash
import Data.Acid hiding (makeAcidic, Query, Update)
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.ANSI.Leijen

-- TH imports
import Language.Haskell.TH hiding (OverloadedStrings, QuasiQuotes)
import Language.Haskell.TH.Syntax (Lift(..), Module(..), modString)

-- Path Handling imports
import Prelude hiding (FilePath)
import Paths_haskhol_core
import Shelly hiding (put, get)
import System.FilePath (combine)
import Data.Text (pack)
import System.IO (withBinaryFile, IOMode(..), Handle)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Cereal

-- Util Imports
import Control.Lens hiding (set)
import qualified Control.Lens as L

-- interpreter stuff
import Data.Coerce
import Language.Haskell.Interpreter hiding (get, lift, typeOf, name)
import qualified Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe


-- Monad
-- State Types
data Proofs = Proofs (Hash.HashMap Text HOLThm) deriving Eq
instance SafeCopy Proofs where
    getCopy = contain $ fmap (Proofs . Hash.fromList) safeGet
    putCopy (Proofs hm) = contain . safePut $ Hash.toList hm


data ProofState = ProofState
  { _proofs :: Proofs
  , _benignFlags :: !(Map String Bool)
  , _tyCounter :: !Integer
  , _tmCounter :: !Integer
  } deriving Eq
makeLenses ''ProofState

deriveSafeCopy 0 'base ''ProofState

putProofState :: ProofState -> Update ProofState ()
putProofState = put

getProofState :: Query ProofState ProofState
getProofState = ask

makeAcidic ''ProofState ['putProofState, 'getProofState]

initProofState :: ProofState
initProofState = ProofState (Proofs Hash.empty) mapEmpty 0 0

data Serials = Serials !(Map String String)
deriveSafeCopy 0 'base ''Serials

putSerials :: Serials -> Update Serials ()
putSerials = put

getSerials :: Query Serials Serials
getSerials = ask

makeAcidic ''Serials ['putSerials, 'getSerials]


-- Parse Context stored in HOL Monad for efficiency
data ParseContext = ParseContext
    { _typeConstants :: !(Map Text TypeOp)
    , _termConstants :: !(Map Text HOLTerm)
    , _typeAbbrevs :: !(Map Text HOLType)
    , _prefixes :: ![Text] 
    , _binders :: ![Text]
    , _tyBinders :: ![Text]
    , _infixes :: ![(Text, (Int, Text))]
    , _interface :: ![(Text, (Text, HOLType))]
    , _overloads :: !(Map Text HOLType)
    , _hidden :: ![Text]
    , _lefts     :: ![(Text, Int)]
    , _rights    :: ![(Text, Int)]
    , _unspaced  :: ![Text]
    , _prebroken :: ![Text]
    } deriving (Eq, Lift)
makeLenses ''ParseContext

deriveSafeCopy 0 'base ''ParseContext

putParseContext :: ParseContext -> Update ParseContext ()
putParseContext = put

getParseContext :: Query ParseContext ParseContext
getParseContext = ask

makeAcidic ''ParseContext ['putParseContext, 'getParseContext]

-- | The initial parser context.
initParseContext :: ParseContext
initParseContext = ParseContext 
    initTypeConstants initTermConstants mapEmpty [] 
    initBinderOps initTyBinderOps initInfixOps [] mapEmpty []
    (grabInfix "left" initInfixOps) (grabInfix "right" initInfixOps)
    initUnspaced initPrebroken
  where initBinderOps :: [Text]
        initBinderOps = ["\\"]

        initTyBinderOps :: [Text]
        initTyBinderOps = ["\\\\"]

        initInfixOps :: [(Text, (Int, Text))]
        initInfixOps = [("=", (12, "right"))]

        initUnspaced :: [Text]
        initUnspaced = [",", "..", "$"]

        initPrebroken :: [Text]
        initPrebroken = ["==>"]
        
        grabInfix :: Text -> [(Text, (Int, Text))] -> [(Text, Int)]
        grabInfix a = mapFilter $ \ (x, (n, a')) -> 
            if a == a' then return (x, n) else fail' "grabInfix"

-- Overall State
type GConversion cls thry = (Int, Conversion cls thry)

{-|
  The 'Conversion' type is a special class of derived rules that accepts a
  term and returns a theorem proving its equation with an equivalent term.
-}
data Conversion cls thry where
  Conv :: (HOLTerm -> HOL cls thry HOLThm) -> Conversion cls thry
  deriving Typeable

data HOLState thry = HOLState
  { _proofState :: ProofState
  , _parseContext' :: ParseContext
  , _convCache :: !(Hash.HashMap HOLTerm HOLThm)
  , _netCache :: !(Hash.HashMap [HOLThm] (Net (GConversion Proof thry)))
  , _thmCache :: !(Map Text [HOLThm])
  , _flagCache :: !(Map Text [Int])
  }

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
          runHOLUnsafe :: IORef (HOLState thry) -> String -> [String] -> IO a
        } deriving Typeable

-- | The classification tag for theory extension computations.
data Theory
-- | The classification tag for proof computations.
data Proof deriving Typeable
makeLenses ''HOLState

{-| 
  Can be used to make 'Proof' computations more general, e.g. more polymorphic.
-}
mkProofGeneral :: HOL Proof thry a -> HOL cls thry a
mkProofGeneral = coerce

runHOLInternal :: Bool -> Maybe ProofState -> HOL cls thry a -> TheoryPath thry 
               -> IO (a, ProofState)
runHOLInternal fl prfs m tp =
    do dir <- getDataDir 
       let new = newTP tp
           newFp = dir `combine` new
           mods = buildModList tp []
       shelly $
         do cond <- test_d $ mkFilePath newFp  
            unless cond $
              do echo . pack $ 
                   "Note: Rebuilding theory context for " ++ new
                 echo $ pack "...This may take a while."
                 liftIO $ 
                   runHOLTheory (loadTP tp) (oldTP tp) new
       runHOLUnsafe' fl prfs m newFp mods

runHOLUnsafe' :: Bool -> Maybe ProofState -> HOL cls thry a -> String 
              -> [String] -> IO (a, ProofState)
runHOLUnsafe' fl initPrfs m new mods =
    do dir <- getDataDir
       let openProof = openLocalState' initProofState dir
           openParse = openLocalState' initParseContext new
       prfs <- case initPrfs of
                 Just prfst -> return prfst
                 Nothing -> do acidProof <- openProof
                               prfst <- query acidProof GetProofState
                               closeAcidState acidProof
                               return prfst
       acidParse <- openParse
       parser <- query acidParse GetParseContext
       closeAcidState acidParse
       ref <- newIORef $ HOLState prfs parser Hash.empty Hash.empty mapEmpty mapEmpty
       (do res <- runHOLUnsafe m ref new mods
           st <- readIORef ref
           return (res, st ^. proofState)) `E.finally` when fl 
         (do st <- readIORef ref
             let prfs' = st ^. proofState
                 parser' = st ^. parseContext'
             when (prfs' /= prfs) $
               do acidProof' <- openProof
                  update acidProof' $ PutProofState prfs'
                  createCheckpoint acidProof'
                  createArchive acidProof'
                  closeAcidState acidProof'
             when (parser' /= parser) $
               do acidParse' <- openParse
                  update acidParse' $ PutParseContext parser'
                  createCheckpoint acidParse'
                  createArchive acidParse'
                  closeAcidState acidParse')

{-| 
  Runs a 'HOL' 'Proof' computation using a provided 'TheoryPath'. 
  The first argument is a flag indicating if you want access to the proof cache
  to be read-only (@False@) or read-write (@True@).
  Other state values, e.g. type and term constants, may be accessed but not 
  modified.
-}
runHOLProof :: Bool -> HOL Proof thry a -> TheoryPath thry -> IO a
runHOLProof fl prf tp = fst `fmap` runHOLInternal fl Nothing prf tp

{-| 
  Evaluates a 'HOL' computation by copying the contents of a provided 
  'TheoryPath' to a new directory where destructive updates will occur.
  This is used primarily by 'extendTheory', but is also useful for testing 
  'Theory' computations in temporary directories.
-}
runHOLTheory :: HOL cls thry a -> Maybe (TheoryPath thry) -> String -> IO a
runHOLTheory m (Just old) new =
    do dir <- getDataDir 
       let newFP = dir `combine` new
           old' = mkFilePath $ dir `combine` newTP old
           new' = mkFilePath newFP
           mods = buildModList old []
       shelly . print_stderr False $ 
         do unlessM (test_d old') . liftIO $ fst `fmap`
              runHOLInternal True Nothing (return ()) old
            whenM (test_d new') . echo . pack $
              "runHOL: acid-state directory, " ++
              new ++ ", already exists.  Overwriting."
            rm_rf new'
            cp_r old' new'
       fst `fmap` runHOLUnsafe' True Nothing m newFP mods
-- really only used for creating BaseCtxt to prevent needing dummy data files
runHOLTheory m Nothing new =
    do dir <- getDataDir
       let newFP = dir `combine` new
           new' = mkFilePath newFP
       shelly . print_stderr False . unlessM (test_d new') $
         do echo . pack $ "runHOL: acid-state director, " ++ new ++
                          " does not exist.  Using empty directory."
            mkdir new'
       fst `fmap` runHOLUnsafe' True Nothing m newFP 
                    ["HaskHOL.Core.State.Monad"]

{-|
  Used to dynamically evaluate a 'HOL' computation using the 'interpret'
  method from @Language.Haskell.Interpreter@.

  The first provided argument is the 'String' representation of the computation
  to evaluate.  Note that the type of this computation must be 
  @HOL Proof thry a@, otherwise a run-time exception will be thrown.

  The second provided argument is the list of 'String' names for the modules
  that must be imported for the computation to succeed.
-}
runHOLHint :: String -> [String] -> HOL cls thry (Conversion cls thry)
runHOLHint m mods = HOL $ \ ref tp thryMods -> 
    do r <- runInterpreter $
                do setImports $ ["Prelude", "HaskHOL.Core"] ++ mods -- ++ thryMods
                   set [languageExtensions := [OverloadedStrings, QuasiQuotes]]
                   unsafeSetGhcOption "-fcontext-stack=200"
                   -- We can check the type here for more safety, 
                   -- should we want to.
                   interpret m (as :: Conversion Proof UnsafeThry)
       case r of
         Left err -> fail $ show err
         Right res -> return $! coerce res

instance Functor (HOL cls thry) where
    fmap = liftM
    
instance Monad (HOL cls thry) where
    return x = HOL $ \ _ _ _ -> return x
    m >>= k = HOL $ \ ref st mods -> 
        do b <- runHOLUnsafe m ref st mods
           runHOLUnsafe (k b) ref st mods
    fail = fail'

instance Applicative (HOL cls thry) where
    pure = return
    (<*>) = ap

instance MonadThrow (HOL cls thry) where
    throwM x = HOL $ \ _ _ _ -> E.throwIO x
instance MonadCatch (HOL cls thry) where
    catch job err = HOL $ \ ref st mods ->
        runHOLUnsafe job ref st mods `E.catch`
        \ e -> runHOLUnsafe (err e) ref st mods
instance MonadMask (HOL cls thry) where
    mask m = HOL $ \ ref st mods -> E.mask $ \ restore ->
        runHOLUnsafe (m $ lft restore) ref st mods
    uninterruptibleMask m = HOL $ \ ref st mods -> E.uninterruptibleMask $ 
        \ restore -> runHOLUnsafe (m $ lft restore) ref st mods

lft :: (IO a -> IO a) -> HOL cls thry a -> HOL cls thry a
lft rst (HOL x) = HOL $ \ ref st mods -> rst $ x ref st mods

-- Theory Contexts
data InnerThry

{-| 
  The 'TheoryPath' data type contains the information necessary to both
  retrieve and reconstruct theory contexts.  They are safely constructed via the
  'extendTheory' method which builds new 'TheoryPath' values from old ones.

  The 'ctxtBase' value can be used as a starting point, providing a linear 
  ordering of theory contexts.
-}
data TheoryPath thry = TheoryPath
    { newTP :: String
    , oldTP :: Maybe (TheoryPath InnerThry)
    , modTP :: String
    , loadTP :: HOL Theory InnerThry ()
    } deriving Typeable

instance Show (TheoryPath thry) where
    show = newTP

-- Used to construct the imports modules necessary for run-time interpretation
buildModList :: TheoryPath thry -> [String] -> [String]
buildModList x acc =
    case oldTP x of
      Nothing -> acc
      Just tp -> buildModList tp (modTP x : acc)

-- converts TheoryPath strings to FilePaths for shelly
mkFilePath :: String -> FilePath
mkFilePath = fromText . pack


data UnsafeThry = UnsafeThry deriving Typeable

-- | The 'BaseThry' type is the type of the initial working theory.
data BaseThry = BaseThry deriving Typeable
{-| 
  The 'ExtThry' type is the type of a linear theory extension, i.e. a cons-like
  operation for theory types.  See the module "HaskHOL.Lib.Equal.Context" for
  an example of how to correctly define theory types and contexts for a library.
-}
data ExtThry a b = ExtThry a b deriving Typeable

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
    BaseCtxt UnsafeThry = ()
    BaseCtxt BaseThry = ()
    BaseCtxt (ExtThry a b) = BaseCtxt b

-- type family stuff for base contexts
type instance PolyTheory BaseThry b = BaseCtxt b

type instance BaseThry == BaseThry = 'True
type instance ExtThry a b == ExtThry a' b' = (a == a') && (b == b')

-- | The 'TheoryPath' for the base theory context.
ctxtBase :: TheoryPath BaseThry
ctxtBase = 
    TheoryPath (ctxtName (undefined :: BaseThry)) Nothing 
      "HaskHOL.Core.State.Monad" (return ()) 
      
{-| 
  Returns the name of the module this splice is called from. Used in conjunction
  with 'extendTheory'.
-}
thisModule' :: Q Exp
thisModule' = 
    do (Module _ modname) <- thisModule
       litE . stringL $ modString modname

{-|
  Constructs a new 'TheoryPath' value by extending an old value with a provided 
  'HOL' computation.  This formulation of theory paths allows the reconstruction
  of theory contexts without having to recompile libraries.
-}
extendTheory :: forall new old. CtxtName new => TheoryPath old 
             -> String -> HOL Theory old () -> TheoryPath new
extendTheory old modname m = 
    let old' :: TheoryPath InnerThry
        old' = coerce old
        m' :: HOL cls InnerThry ()
        m' = coerce m in
      TheoryPath (ctxtName (undefined :: new)) (Just old') modname m'

-- Printer Context Helpers
parseContext :: HOL cls thry ParseContext
parseContext = HOL $ \ ref _ _ ->
    do st <- readIORef ref
       return $! st ^. parseContext'

parseContextCache :: HOL cls thry ParseContext
parseContextCache =
    do acid <- openLocalStateHOL initParseContext
       ctxt <- queryHOL acid GetParseContext
       closeAcidStateHOL acid
       return ctxt

viewParseContext :: Getting a ParseContext a -> HOL cls thry a
viewParseContext f = view f `fmap` parseContext

overParseContext :: Setting (->) ParseContext ParseContext a a -> (a -> a) 
                 -> HOL Theory thry ()
overParseContext f p = HOL $ \ ref _ _ ->
    atomicModifyIORef' ref (\ st -> (over (parseContext' . f) p st, ()))

testParseContext :: Optical (->) (->) (Const Bool) ParseContext ParseContext a a
                 -> (a -> Bool) -> HOL cls thry Bool
testParseContext f p = views f p `fmap` parseContext

-- define own versions of IO functions so they can be used external to kernel
{-| 
  A version of 'putDoc' lifted to the 'HOL' monad for use with pretty printers.
-}
putDocHOL :: Doc -> HOL cls thry ()
putDocHOL x = HOL $ \ _ _ _ -> putDoc x >> putStr "\n"

-- | A version of 'putStr' lifted to the 'HOL' monad.
putStrHOL :: String -> HOL cls thry ()
putStrHOL x = HOL $ \ _ _ _ -> putStr x

-- | A version of 'putStrLn' lifted to the 'HOL' monad.
putStrLnHOL :: String -> HOL cls thry ()
putStrLnHOL x = HOL $ \ _ _ _ -> putStrLn x

-- Local vars
-- | A type synonym for 'IORef'.
type HOLRef = IORef

{-| 
  Creates a new 'HOLRef' from a given starting value.  Functionally equivalent
  to 'newIORef' lifted to the 'HOL' monad.
-}
newHOLRef :: a -> HOL cls thry (HOLRef a)
newHOLRef ref = HOL $ \ _ _ _ -> newIORef ref

{-|
  Reads a 'HOLRef' returning the stored value.  Functionally equivalent to 
  'readIORef' lifted to the 'HOL' monad.
-}
readHOLRef :: IORef a -> HOL cls thry a
readHOLRef ref = HOL $ \ _ _ _ -> readIORef ref

{-|
  Writes a value to a 'HOLRef'.  Functionally equivalent to 'writeHOLRef' lifted
  to the 'HOL' monad.
-}
writeHOLRef :: IORef a -> a -> HOL cls thry ()
writeHOLRef ref x = HOL $ \ _ _ _ -> writeIORef ref x

{-|
  Applies a given function to a 'HOLRef', modifying the stored value.
  Functionally equivalent to 'modifyIORef' lifted to the 'HOL' monad.
-}
modifyHOLRef :: IORef a -> (a -> a) -> HOL cls thry ()
modifyHOLRef ref f = HOL $ \ _ _ _ -> modifyIORef ref f

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
openLocalStateHOL ast = HOL $ \ _ st _ -> openLocalState' ast st

{-| 
  A version of 'openLocalStateHOL' that uses just @HaskHOL.Core@'s shared data
  directory.
-}
openLocalStateHOLBase :: (Typeable st, IsAcidic st) => st 
                      -> HOL cls thry (AcidState st)
openLocalStateHOLBase ast = HOL $ \ _ _ _ -> openLocalState' ast ""

-- | A wrapper to 'closeAcidState' for the 'HOL' monad.
closeAcidStateHOL :: AcidState st -> HOL cls thry ()
closeAcidStateHOL ast = HOL $ \ _ _ _ -> closeAcidState ast


{-|
  A wrapper to 'update' for the 'HOL' monad.  Note that the classification of
  the provided 'HOL' computation is unrestricted, such that it can be used in 
  'Proof' computations to update benign state values.

  If you want the state modification to be captured by the type, make sure to
  use 'updateHOL' instead.
-}
updateHOLUnsafe :: UpdateEvent event => AcidState (EventState event) -> event 
                -> HOL cls thry (EventResult event)
updateHOLUnsafe ast e = HOL $ \ _ _ _ -> update ast e

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
queryHOL ast e = HOL $ \ _ _ _ -> query ast e


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
modBenignFlag val flag = HOL $ \ ref _ _ ->
    atomicModifyIORef' ref (\ st ->
      (over (proofState . benignFlags) (mapInsert (tyToIndex flag) val) st, ()))

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
getBenignFlag flag = HOL $ \ ref _ _ ->
    do st <- readIORef ref
       let val = mapAssoc (tyToIndex flag) $ st ^. proofState . benignFlags
       return $! tryd (initFlagValue flag) val

-- Fresh Name Generation

{-| 
  Increments the term counter stored in the context, returning the new value.
-}
tickTermCounter :: HOL cls thry Integer
tickTermCounter = HOL $ \ ref _ _ ->
    do st <- readIORef ref
       let n = st ^. proofState . tmCounter
       atomicModifyIORef' ref 
         (\ st' -> (over (proofState . tmCounter) succ st', n))

{-|
  Increments the type counter stored in the context, returning the new value.
-}
tickTypeCounter :: HOL cls thry Integer
tickTypeCounter = HOL $ \ ref _ _ ->
    do st <- readIORef ref
       let n = st ^. proofState . tyCounter
       atomicModifyIORef' ref 
         (\ st' -> (over (proofState . tyCounter) succ st', n))

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
           ty = DataD [] name [] Nothing [NormalC name []] [ConT ''Typeable]
           cls = InstanceD Nothing [] (AppT (ConT ''BenignFlag) (ConT name)) 
                   [FunD 'initFlagValue [Clause [WildP] (NormalB val') []]]
       return [ty, cls]

-- Proof Caching
getProofInternal :: Text -> IORef (HOLState thry) -> IO HOLThm -> IO HOLThm
getProofInternal lbl ref job =
    do st <- readIORef ref
       let Proofs sthm = st ^. proofState . proofs
       case Hash.lookup lbl sthm of
         Just th -> return th
         Nothing -> job

{-| 
  An "unsafe" version of 'cacheProof' that uses the current working theory
  to evaluate a proof instead of a provided theory context.  Useful for
  cacheing proofs that are used in the construction of theories.
-}
unsafeCacheProof :: Text -> HOL Proof thry HOLThm -> HOL cls thry HOLThm
unsafeCacheProof lbl prf = HOL $ \ ref tp mods ->
    getProofInternal lbl ref $
      let lbl' = unpack lbl in
        do putStrLn ("proving: " ++ lbl')
           th <- runHOLUnsafe prf ref tp mods
           putStrLn (lbl' ++ " proved.")
           atomicModifyIORef' ref $ \ st ->
             (over (proofState . proofs) 
               (\ (Proofs prfs) -> Proofs $ Hash.insert lbl th prfs) st, th)

{-|
  The 'cacheProof' method stores or retrieves a theorem from the proof
  cache.  For example:

  > thmTRUTH :: BoolCtxt thry => HOL cls thry HOLThm
  > thmTRUTH = cacheProof "thmTRUTH" ctxtBool $
  >   primEQ_MP (ruleSYM defT) $ primREFL [txt| \ p:bool. p |]

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
cacheProof :: PolyTheory thry thry' => Text -> TheoryPath thry 
           -> HOL Proof thry HOLThm 
           -> HOL cls thry' HOLThm
cacheProof lbl tp prf = HOL $ \ ref _ _ ->
    getProofInternal lbl ref $
      let lbl' = unpack lbl in
        do putStrLn ("proving: " ++ lbl')
           st <- readIORef ref
           (th, prfs) <- runHOLInternal False (Just $ st ^. proofState) prf tp
           putStrLn (lbl' ++ " proved.")
           atomicModifyIORef' ref $ \ st' ->
             let prfs' = over proofs (\ (Proofs ps) -> 
                           Proofs $ Hash.insert lbl th ps) prfs in
               (L.set proofState prfs' st', th)

{-| 
  Retrieves a proof from the cache given its label.  
  Can be useful in combination with 'cacheProofs'.
-} 
getProof :: Text -> HOL cls thry HOLThm
getProof lbl = HOL $ \ ref _ _ ->
    getProofInternal lbl ref (fail $ "getProof: proof " ++ unpack lbl ++ 
                                     " not found.")


{-|
  This is a version of 'cacheProof' that handles proof computations that return
  multiple theorems.  Essentially, it maps 'cacheProof' over
  the list of labels, where each resultant computation retrieves only a single
  theorem, but can store them all.
  
  The decision to return a list of computations was inherited from a previous 
  implementation technique. It can result in some messy code when you want to
  provide top-level names for each computation, but works fairly well otherwise.
-}
cacheProofs :: forall cls thry thry'. PolyTheory thry thry' => [Text] 
            -> TheoryPath thry 
            -> HOL Proof thry [HOLThm] 
            -> [HOL cls thry' HOLThm]
cacheProofs lbls tp prf = map cacheProofs' lbls
  where cacheProofs' :: Text -> HOL cls thry' HOLThm
        cacheProofs' lbl = HOL $ \ ref _ _ ->
            getProofInternal lbl ref $
              do st <- readIORef ref
                 let prfs = st ^. proofState
                     Proofs sthm = prfs ^. proofs
                     qths = mapFilter (maybeToFail "cacheProofs" . 
                                       (`Hash.lookup` sthm)) lbls
                 unless (null qths) . fail $
                   "cacheProofs: some provided labels clash with " ++
                   "existing theorems."
                 let lbls' = unpack $ T.unwords lbls
                 putStrLn ("proving: " ++ lbls')
                 (ths, prfs') <- runHOLInternal False (Just prfs) prf tp
                 putStrLn (lbls' ++ " proved.")
                 when (length lbls /= length ths) . fail $
                   "cacheProofs: number of labels does not match " ++
                   "number of theorems."
                 let hm' = Hash.fromList $ zip lbls ths
                 atomicModifyIORef' ref (\ st' -> 
                   let Proofs sthm' = prfs' ^. proofs
                       hm'' = Hash.union sthm' hm'
                       Just th = Hash.lookup lbl hm''
                       prfs'' = L.set proofs (Proofs hm'') prfs' in
                     (L.set proofState prfs'' st', th))

cacheConversion :: Conversion cls thry -> Conversion cls thry
cacheConversion (Conv cnv) = Conv $ \ tm -> HOL $ \ ref state mods ->
    do st <- readIORef ref
       case Hash.lookup tm $ st ^. convCache of
         Just res -> return res
         Nothing ->
           do res <- runHOLUnsafe (cnv tm) ref state mods
              atomicModifyIORef' ref $ \ st' ->
                (over convCache (Hash.insert tm res) st', res)

cacheNet :: [HOLThm] 
         -> ([HOLThm] -> HOL Proof thry (Net (GConversion Proof thry)))
         -> HOL cls thry (Net (GConversion Proof thry))
cacheNet ths buildNet = HOL $ \ ref state mods ->
    do st <- readIORef ref
       case Hash.lookup ths $ st ^. netCache of
         Just res -> return res
         Nothing ->
           do res <- runHOLUnsafe (buildNet ths) ref state mods
              atomicModifyIORef' ref $ \ st' ->
                (over netCache (Hash.insert ths res) st', res)

cacheThms :: Text -> HOL cls thry [HOLThm] -> HOL cls thry [HOLThm]
cacheThms lbl m = HOL $ \ ref state mods ->
    do st <- readIORef ref
       case mapAssoc lbl $ st ^. thmCache of
         Just res -> return res
         Nothing ->
           do res <- runHOLUnsafe m ref state mods
              atomicModifyIORef' ref $ \ st' ->
                (over thmCache (mapInsert lbl res) st', res)

cacheFlags :: Text -> HOL cls thry [Int] -> HOL cls thry [Int]
cacheFlags lbl m = HOL $ \ ref state mods ->
    do st <- readIORef ref
       case mapAssoc lbl $ st ^. flagCache of
         Just res -> return res
         Nothing ->
           do res <- runHOLUnsafe m ref state mods
              atomicModifyIORef' ref $ \ st' ->
                (over flagCache (mapInsert lbl res) st', res)

serializeValue :: forall a cls thry thry'. 
                  (SafeCopy a, Typeable a, PolyTheory thry thry') 
               => String -> TheoryPath thry 
               -> HOL Proof thry a 
               -> HOL cls thry' a
serializeValue fname tp m = HOL $ \ ref _ _ ->
    do dir <- getDataDir
       let dir' = dir `combine` "Serials" 
           fname' = dir' `combine` fname
           resTy = tyToIndex (undefined :: a)
       acid <- openLocalState' (Serials mapEmpty) dir'
       (Serials serials) <- query acid GetSerials
       closeAcidState acid
       cond <- shelly $ let dir'' = mkFilePath dir' in
                          do unlessM (test_d dir'') $ mkdir dir''
                             test_f $ mkFilePath fname'
       if cond
          then case mapAssoc fname serials of
                 Nothing -> 
                     fail $ "serializeValue: file present, but " ++
                            "not recorded in list of known serializations."
                 Just ty
                     | ty == resTy ->
                         withBinaryFile fname' ReadMode $ \ inH -> getter inH
                     | otherwise -> 
                         fail $ "serializeValue: file present, but " ++
                                "of wrong type for serialization."
         else do st <- readIORef ref
                 (res, prfs) <- runHOLInternal False (Just $ st ^. proofState) 
                                  m tp
                 withBinaryFile fname' WriteMode $ \ outH -> putter res outH
                 acid' <- openLocalState' (Serials mapEmpty) dir'
                 update acid' . PutSerials . Serials $ 
                   mapInsert fname resTy serials
                 createCheckpoint acid'
                 createArchive acid'
                 closeAcidState acid'
                 atomicModifyIORef' ref $ \ st' ->
                   (L.set proofState prfs st', res)
  where putter :: a -> Handle -> IO ()
        putter x outH = sourcePut (safePut x) $$ sinkHandle outH

        getter :: Handle -> IO a
        getter inH = sourceHandle inH $$ sinkGet (safeGet @a)
