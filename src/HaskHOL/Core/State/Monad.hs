{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ExistentialQuantification, 
             MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}

{-|
  Module:    HaskHOL.Core.State.Monad
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module exports the primitive types and combinators for the 'HOL' 
  computational monad.  At a high level this monad is a flattened stack of a
  'State' monad transformer and a limited 'IO' monad.

  For higher level monadic combinators see the "HaskHOL.Core.State" and
  "HaskHOL.Core.Basics" modules.
-}
module HaskHOL.Core.State.Monad
    ( -- * The HOL Monad
      HOL
    , Theory
    , Proof
    , runHOLCtxt  -- :: HOLContext thry -> IO (a, HOLContext thry) 
    , evalHOLCtxt -- :: HOL cls thry a -> HOLContext thry -> IO a
    , execHOLCtxt -- :: HOL cls thry a -> HOLContext thry -> 
                  --    IO (HOLContext thry)
      -- * State Methods
    , get  -- :: HOL cls thry (HOLContext thry)
    , gets -- :: (HOLContext thry -> a) -> HOL cls thry a
      -- * Text Output Methods
    , putStrHOL   -- :: String -> HOL cls thry ()
    , putStrLnHOL -- :: String -> HOL cls thry ()
      -- * Exception Handling Methods
    , HOLException(..)
    , throwHOL    -- :: Exception e => e -> HOL cls thry a
    , catchHOL    -- :: Exception e => HOL cls thry a -> (e -> HOL cls thry a) 
                  --    -> HOL cls thry a
    , liftMaybe   -- :: String -> Maybe a -> HOL cls thry a
    , liftEither  -- :: Show err => String -> Either err a -> HOL cls thry a
      -- * Local Reference Methods
    , HOLRef
    , newHOLRef    -- :: a -> HOL cls thry (HOLRef a)
    , readHOLRef   -- :: IORef a -> HOL cls thry a
    , writeHOLRef  -- :: IORef a -> a -> HOL cls thry ()
    , modifyHOLRef -- :: IORef a -> (a -> a) -> HOL cls thry ()
      -- * Benign Flag Methods
    , BenignFlag(..)
    , setBenignFlag
    , unsetBenignFlag
    , getBenignFlagCtxt
    , getBenignFlag
      -- * Methods Related to Fresh Name Generation
    , tickTermCounter -- :: HOL cls thry Int
    , tickTypeCounter -- :: HOL cls thry Int
      -- * Extensible State Methods
       -- $ExtState
    , ExtClass(..)
    , ExtState
    , putExt     -- :: ExtClass a => a -> HOL Theory thry ()
    , getExtCtxt -- :: forall a thry. ExtClass a => HOLContext thry -> Maybe a
    , getExt     -- :: forall cls thry a. ExtClass a => HOL cls thry a
    , modifyExt  -- :: ExtClass a => (a -> a) -> HOL Theory thry ()
      -- * Implementation of Theory Contexts
    , HOLContext
    , ctxtBase -- :: HOLContext BaseThry
    , ExtThry(..)
    , BaseThry(..)
    , BaseCtxt
      -- * Template Haskell Assistance for Flags/Extensions
    , newFlag      -- :: String -> Bool -> Q [Dec]
    , newExtension -- :: String -> ExpQ -> Q [Dec]
      -- * Re-export for Extensible Exceptions
    , Exception
    ) where

import HaskHOL.Core.Lib

import Control.Exception (Exception)
import qualified Control.Exception as E

import Data.IORef
 
import Data.Typeable (cast, typeOf)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))


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
             working theory, i.e. the last library loaded.  Again, it is phantom
             type variable that is inhabited by an empty data type.  A unique
             tag is created for each library by linerearly extending the tag
             starting from a base value. For example, the tag 
             @ExtThry EqualThry BaseThry@ would indicate a current working
             theory consisting of the base and equality logic theories.

             Note that typically this value is left polymorphic and is
             constrained by a class related to a library.  For example, the
             following type indicates a computation that can only be ran by
             using a theory context value that has the equality logic library
             loaded:  @EqualCtxt thry => HOL cls thry a@

  * @a@ - The return type of a 'HOL' computation.

  Note that the 'HOL' monad is effectively a flattened stack of a limited
  'IO' monad and a 'State' monad.  We say limited as we restrict the possible
  IO-like computations to the ones shown in this module, rather than allowing
  arbitrary computations through a mechanism like 'MonadIO'.  This prevents a
  number of soundness issues.

  For more information regarding the contents of a theory context see the
  documentation for 'HOLContext'.
-}

newtype HOL cls thry a = 
    HOL { {-| 
            Evaluates a 'HOL' computation with a provided theory context.
            Returns the result paired with an updated theory context.
          -}
          runHOLCtxt :: HOLContext thry -> IO (a, HOLContext thry) 
        }

-- | The classification tag for theory extension computations.
data Theory
-- | The classification tag for proof computations.
data Proof

instance Functor (HOL cls thry) where
    fmap = liftM
    
instance Monad (HOL cls thry) where
    return x = HOL $ \ s -> 
        return (x, s)
    {-# INLINEABLE (>>=) #-}
    m >>= k = HOL $ \ s ->
        do (b, s') <- runHOLCtxt m s
           runHOLCtxt (k b) s'
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

-- | A version of 'runHOLCtxt' that returns only the resultant value.
evalHOLCtxt :: HOL cls thry a -> HOLContext thry -> IO a
evalHOLCtxt m ctxt = return fst <*> runHOLCtxt m ctxt

-- | A version of 'runHOLCtxt' that returns only the theory context.
execHOLCtxt :: HOL cls thry a -> HOLContext thry -> IO (HOLContext thry)
execHOLCtxt m ctxt = return snd <*> runHOLCtxt m ctxt

{- 
  We define our own versions of state functions instead of deriving MonadState 
  so that we can control where they are exported.  Note that put is not expose
  to the user.
-}
put :: HOLContext thry -> HOL cls thry ()
put s = HOL $ \ _ -> return ((), s)

{-| 
  Equivalent to 'Control.Monad.State.get' for the 'HOL' monad.  Note that we
  define our own version of this function, rather than define an instance of
  'MonadState' so that we can control where the morphisms are exported.

  This is done in the name of soundness given that a user can inject an unsound
  theory context into a proof using a @put@ morphism.  This is analogous to the
  issue behind defining an instance of 'MonadIO' given 'liftIO' can be used to
  inject arbitrary computations into the 'HOL' monad, including ones containing
  unsound contexts.
-}
get :: HOL cls thry (HOLContext thry)
get = HOL $ \ s -> return (s, s)

{-| 
  A version of 'get' that applies a function to the state before returning the
  result.
-}
gets :: (HOLContext thry -> a) -> HOL cls thry a
gets f = liftM f get

-- See the above notes.  Not exported to the user.
modify :: (HOLContext thry -> HOLContext thry) -> HOL cls thry ()
modify = put <=< gets

-- define own versions of IO functions so they can be used external to kernel
-- | A version of 'putStr' lifted to the 'HOL' monad.
putStrHOL :: String -> HOL cls thry ()
putStrHOL str = HOL $ \ s -> putStr str >> return ((), s)

-- | A version of 'putStrLn' lifted to the 'HOL' monad.
putStrLnHOL :: String -> HOL cls thry ()
putStrLnHOL str = HOL $ \ s -> putStrLn str >> return ((), s)

-- Errors

-- the basic HOL exception type
-- | The data type for generic errors in HaskHOL.  Carries a 'String' message.
newtype HOLException = HOLException String deriving (Show, Typeable)
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
throwHOL e = HOL $ \ _ -> E.throwIO e

{-| 
  A version of 'E.catch' lifted to the 'HOL' monad.

  Note that 'mplus' and '<|>' are defined in terms of catching a 
  'E.SomeException' with 'catchHOL' and then ignoring it to run an alternative
  computation instead.
-}
catchHOL :: Exception e => HOL cls thry a -> (e -> HOL cls thry a) -> 
                           HOL cls thry a
catchHOL job errcase = HOL $ \ s ->
    runHOLCtxt job s `E.catch` \ e -> runHOLCtxt (errcase e) s

-- Used to define mplus and (<|>) for the HOL monad.  Not exposed to the user.
(<||>) :: HOL cls thry a -> HOL cls thry a -> HOL cls thry a
job <||> alt = HOL $ \ s ->
   runHOLCtxt job s `E.catch` \ (_ :: E.SomeException) -> runHOLCtxt alt s

{-| 
  Lifts a 'Maybe' value into the 'HOL' monad mapping 'Just's to 'return's and
  'Nothing's to 'fail's with the provided 'String'.
-}
{-# INLINEABLE liftMaybe #-}
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
{-# INLINEABLE liftEither #-}
liftEither :: Show err => String -> Either err a -> HOL cls thry a
liftEither _ (Right res) = return res
liftEither str1 (Left str2) = fail $ str1 ++ " - " ++ show str2

-- Local vars
-- | A type synonym for 'IORef'.
type HOLRef = IORef

{-| 
  Creates a new 'HOLRef' from a given starting value.  Functionally equivalent
  to 'newIORef' lifted to the 'HOL' monad.
-}
newHOLRef :: a -> HOL cls thry (HOLRef a)
newHOLRef x = HOL $ \ s ->
    do ref <- newIORef x
       return (ref, s)

{-|
  Reads a 'HOLRef' returning the stored value.  Functionally equivalent to 
  'readIORef' lifted to the 'HOL' monad.
-}
readHOLRef :: IORef a -> HOL cls thry a
readHOLRef ref = HOL $ \ s ->
    do res <- readIORef ref
       return (res, s)

{-|
  Writes a value to a 'HOLRef'.  Functionally equivalent to 'writeHOLRef' lifted
  to the 'HOL' monad.
-}
writeHOLRef :: IORef a -> a -> HOL cls thry ()
writeHOLRef ref x = HOL $ \ s -> writeIORef ref x >> return ((), s)

{-|
  Applies a given function to a 'HOLRef', modifying the stored value.
  Functionally equivalent to 'modifyHOLRef' lifted to the 'HOL' monad.
-}
modifyHOLRef :: IORef a -> (a -> a) -> HOL cls thry ()
modifyHOLRef ref f = HOL $ \ s -> modifyIORef ref f >> return ((), s)


-- Context
{-|
  The 'ExtClass' type class is the heart of HaskHOL's extensible state
  mechanism.  It serves a number of purposes:

  * It provides the polymorphic type for heterogenous structures of type 
    'ExtState'.

  * It introduces the 'Typeable' constraint that enables the mechanism for
    selecting specific state extensions based on their type.  See 'getExt' for
    more details.

  * It defines an initial value for state extensions to use if they have not 
    been introduced to the context by a computation yet.

  For more information see the documentation for 'HOLContext', 'getExtCtxt', and
  'putExt'.
-}
class (Lift a, Typeable a) => ExtClass a where
    {-| 
      The intial value for an extensible state type.  The value returned when
      attempting to retrieve a type that is not yet defined in the context.
    -}
    initValue :: a

{-| 
  Used to build heterogenous structures that hold state extensions.  See
  'ExtClass' for more details.
-}
data ExtState = forall a. ExtClass a => ExtState a

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

  See 'HOLContext', 'getBenignFlagCtxt', and 'setBenignFlag' for more details.
-}
class Typeable a => BenignFlag a where
    {-| 
      The intial value for a benign flag.  The value returned when attempting to
      retrieve a flag that is not yet defined in the context.
    -}
    initFlagValue :: a -> Bool

{-|
  The state type for the 'HOL' monad.  A newtype wrapper to the following quad:

  * An association 'List' of @('String', 'Bool')@ pairs that models HaskHOL's
    extensible benign flag system.  The first field is a 'String' representation
    of the type of a benign flag and the second field is that flag's current
    value.

  * An 'Int' counter that is used for fresh name generation for type variables.

  * An 'Int' counter that is used for fresh name generation for term variables.

  * An association 'List' of @('String', 'ExtState')@ pairs that models 
    HaskHOL's extensible state. The first field is a 'String' representation of 
    the type of a state extension and the second field is a wrapping of that 
    type that has an instance of the 'ExtClass' class.

  See 'putExt' and 'getExtCtxt' for more details on how to interact with the
  extensible state and see 'setBenignFlag' and 'getBenignFlag' for more details
  on how to interact with benign flags.
-}
newtype HOLContext thry = 
    HCtxt ([(String, Bool)], Int, Int, [(String, ExtState)]) 
  deriving Typeable

-- manually derived to avoid needing lift instance for phantoms
instance Lift (HOLContext thry) where
  lift (HCtxt x) = conE 'HCtxt `appE` lift x

instance Show (HOLContext thry) where
    show (HCtxt (_, _, _, xs)) = show $ map fst xs

-- Benign Flag methods
-- used internally by set/unsetBenignFlag
modBenignFlag :: BenignFlag a => Bool -> a -> HOL cls thry ()
modBenignFlag val flag =
    modify (\ (HCtxt (flags, tm, ty, m)) ->
               HCtxt (insertMap (show $ typeOf flag) val flags, tm, ty, m))

{-|
  Adds a new, or modifies an existing, benign flag to be 'True'.  Benign flags 
  in the context are stored as a list of @('String', 'Bool')@ pairs.  The first 
  field in this pair is a term-level reificatino of a benign flag's type, 
  produced via a composition of 'show' and 'typeOf'.  The second field is simply
  the current boolean value of the flag.

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
  ness, i.e. @undefined :: X@.

  Example:

  > setBenignFlag FlagDebug

  would set the debugging flag equal to 'True'.

  Alternatively, the 'newFlag' splice can be used to automatically construct a 
  new extension given a name and initial value.  See that function's 
  documentation for more information.
-}
setBenignFlag :: BenignFlag a => a -> HOL cls thry ()
setBenignFlag = modBenignFlag True

-- | Unsets a benign flag making it 'False'.
unsetBenignFlag :: BenignFlag a => a -> HOL cls thry ()
unsetBenignFlag = modBenignFlag False

{-|
  Retrieves the value of a benign flag from a theory context.  This function is
  typically used external to 'HOL' computations, such as in the parser and 
  printer.

  Note that retrieval of the value requires a witness to the desired flag's
  type, i.e.

  > getBenignFlag FlagDebug

  or

  > getBenignFlag (undefined :: FlagDebug)

  In the event that the flag is not found then the 'initFlagValue' for that type
  is returned. Thus, this function never fails.
-}
getBenignFlagCtxt :: forall a thry. BenignFlag a => 
                     a -> HOLContext thry -> Bool
getBenignFlagCtxt flag (HCtxt (flags, _, _, _)) =
    fromMaybe (initFlagValue flag) $ 
      lookup (show $ typeOf flag) flags

{-|
  A version of 'getBenignFlagCtxt' that can be used with theory contexts passed
  implicitly as part of a 'HOL' computation.
  
  Never fails.
-}
getBenignFlag :: BenignFlag a => a -> HOL cls thry Bool
getBenignFlag = gets . getBenignFlagCtxt

-- Fresh Name Generation
{-| 
  Increments the term counter stored in the context, returning the new value.
  Can be used to guarantee the freshness of term names within a single 
  computation.
-}
tickTermCounter :: HOL cls thry Int
tickTermCounter =
    do (HCtxt (f, tm, ty, s)) <- get
       let tm' = succ tm
       put $ HCtxt (f, tm', ty, s)
       return tm'

{-|
  Increments the type counter stored in the context, returning the new value.
  Can be used to gurantee the freshness of type names within a single
  computation.
-}
tickTypeCounter :: HOL cls thry Int
tickTypeCounter =
    do (HCtxt (f, tm, ty, s)) <- get
       let ty' = succ ty
       put $ HCtxt (f, tm, ty', s)
       return ty'

-- Context: Extensible State
{- $ExtState
  HaskHOL's extensible state mechanism is based closely on the implementation 
  of extensible state found in XMonad.

  In the event that the relevant documentation from 'ExtClass', 'putExt', and
  'getExtCtxt' is confusing or not sufficient, it may be helpful to review the
  documentation contained in the "XMonad.Util.ExtensibleState" module.
-}

{-|
  Adds a new, or modifies an existing, state extension.  State extensions in the
  context are stored as a list of @('String', 'ExtState')@ pairs.  The first 
  field in this pair is a term-level reification of a state extension's type, 
  produced via a composition of 'show' and 'typeOf'.  The second field is simply
  a wrapping of the extension's value with 'ExtState' to facilitate 
  heterogeneous structures.

  Numerous usage examples can be found in the "HaskHOL.Core.Parser.Lib" module
  where extensible state is used to store the list of operators, as well as
  other information, required by the parser.

  Note that since the retrieval and storage of state extensions are driven by 
  types, it is in the best interest of library implementors to guarantee that
  the type of their extensions are unique.  The easiest way to do this is to
  create a @newtype@ wrapper for your extension and hide the internal
  constructor to prevent unintended modification.  Again, see 
  "HaskHOL.Core.Parser.Lib" for usage examples.

  Alternatively, the 'newExtension' splice can be used to automatically
  construct a new extension given a name and initial value.  See that function's
  documentation for more information.
-}
putExt :: ExtClass a => a -> HOL Theory thry ()
putExt val = 
    modify (\ (HCtxt (b, tm, ty, m)) -> 
            HCtxt (b, tm, ty, insertMap (show . typeOf $ val) (ExtState val) m))

{-|
  Retrives a state extension from a theory context.  This function is typically 
  used external to 'HOL' computations, such as in the parser, where
  a theory context is passed explicitly as a value.

  Note that the selection of the extension is driven by the return type of this 
  function.  Thus when binding the result of this function, the type must be 
  fixed either via explicit type annotation or through the presence of a unique 
  constructor.

  In order to provide the correct result type, this function relies on the
  type-safe 'cast' operation.  In the event that either this cast fails or the 
  state extension is not found then the 'initValue' for that type is returned.
  Thus, this function never fails.
-}
getExtCtxt :: forall a thry. ExtClass a => HOLContext thry -> a
getExtCtxt (HCtxt (_, _, _, ctxt)) =
    fromMaybe initValue $
      do (ExtState val) <- lookup (show $ typeOf (undefined :: a)) ctxt
         cast val
   
{-|
  A version of 'getExtCtxt' that can be used with theory contexts passed
  implicitly as part of a 'HOL' computation.

  Never fails.
-} 
getExt :: ExtClass a => HOL cls thry a
getExt = gets getExtCtxt
                             
{-| 
  Modifies the value of a state extension.  Functionally equivalent to the
  composition 

  > \ f -> putExt . f =<< getExt
-}
modifyExt :: ExtClass a => (a -> a) -> HOL Theory thry ()
modifyExt f = putExt . f =<< getExt

-- Initial Context
-- | The 'BaseThry' type is the type of the initial working theory.
data BaseThry = BaseThry deriving Typeable
{-| 
  The 'ExtThry' type is the type of a linear theory extension, i.e. a cons-like
  operation for theory types.  See the module "HaskHOL.Lib.Equal.Context" for
  an example of how to correctly define theory types and contexts for a library.
-}
data ExtThry a b = ExtThry a b deriving Typeable

{-|
  The 'BaseCtxt' class is the context name associated with the 'BaseThry' type,
  i.e. the constraint to be used to guarantee that the stateful kernel has been
  loaded.  This should always be true.
-}
class BaseCtxt a
instance BaseCtxt BaseThry
instance BaseCtxt b => BaseCtxt (ExtThry a b)

{-| 
  The initial working theory value:  debugging is on, the counters are at zero 
  and the extensible state is empty.
-}
ctxtBase :: HOLContext BaseThry
ctxtBase = HCtxt ([], 0, 0, [])

-- Some TH wizardry
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

{-|
  The 'newExtension' splice can be used to automatically construct a new state
  extension given a name and a quoted, type annotated, initial value.  The type
  annotation is required as many initial values, such as an empty list, are too
  polymorphic to infer the correct type on its own.

  Example:

  > newExtension "TheCoreDefinitions" [| [] :: [HOLThm] |]

  will construct the following Haskell code:

  > newtype TheCoreDefinitions = TheCoreDefinitions [HOLThm] deriving Typeable
  > instance ExtClass TheCoreDefinitions where
  >     initValue = TheCoreDefinitions []

  Note that, due to limitations with the current version of Template Haskell,
  'Lift' instances should be derived external to this splice via 'deriveLift' or
  'deriveLiftMany'.
-}
newExtension :: String -> ExpQ -> Q [Dec]
newExtension ext val =
    do val' <- val
       case val' of
         SigE e eTy -> 
             let name = mkName ext
                 ty = NewtypeD [] name [] 
                        (NormalC name [(NotStrict, eTy)]) [''Typeable]
                 extCls = InstanceD [] (ConT ''ExtClass `AppT` ConT name)
                            [ValD (VarP 'initValue) (NormalB $
                              ConE name `AppE` e) []] in
               return [ty, extCls]
         _ -> fail "newExtension: provided value must be annotated with a type."

-- lift derivations
deriveLift ''ExtState
