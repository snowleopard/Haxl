-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{- TODO

- do EVENTLOG stuff, track the data fetch numbers for performFetch

- timing: we should be using clock_gettime(CLOCK_MONOTONIC) instead of
  getCurrentTime, which will be affected by NTP and leap seconds.

- write different scheduling policies

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

-- |
-- The implementation of the 'Haxl' monad.  Most users should
-- import "Haxl.Core" instead of importing this module directly.
--
module Haxl.Core.Monad
  (
    -- * The monad
    GenHaxl(..)
  , Result(..)

    -- * Cont
  , Cont(..)
  , toHaxl

    -- * Selective functors
  , Selective (..)
  , branch
  , ifS
  , (<||>)
  , (<&&>)
  , pOr
  , pAnd
  , pOrOld
  , pAndOld

    -- * IVar
  , IVar(..)
  , IVarContents(..)
  , newIVar
  , newFullIVar
  , getIVar
  , putIVar

    -- * ResultVal
  , ResultVal(..)
  , done
  , eitherToResult
  , eitherToResultThrowIO

    -- * CompleteReq
  , CompleteReq(..)

    -- * Env
  , Env(..)
  , Caches
  , caches
  , initEnvWithData
  , initEnv
  , emptyEnv
  , env, withEnv
  , speculate
  , imperative

    -- * JobList
  , JobList(..)
  , appendJobList
  , lengthJobList
  , addJob

    -- * Exceptions
  , throw
  , raise
  , catch
  , catchIf
  , try
  , tryToHaxlException

    -- * Dumping the cache
  , dumpCacheAsHaskell
  , dumpCacheAsHaskellFn

    -- * Unsafe operations
  ,  unsafeLiftIO, unsafeToHaxlException
  ) where

import Haxl.Core.Flags
import Haxl.Core.Selective
import Haxl.Core.Stats
import Haxl.Core.StateStore
import Haxl.Core.Exception
import Haxl.Core.RequestStore as RequestStore
import Haxl.Core.DataCache as DataCache

import Control.Arrow (left)
import Control.Concurrent.STM
import qualified Data.Text as Text
import qualified Control.Monad.Catch as Catch
import Control.Exception (Exception(..), SomeException, throwIO)
import Control.Monad
import qualified Control.Exception as Exception
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const)
#endif
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Data.IORef
import Data.Int
import GHC.Exts (IsString(..))
import Text.PrettyPrint hiding ((<>))
import Text.Printf
#ifdef EVENTLOG
import Control.Exception (bracket_)
import Debug.Trace (traceEventIO)
#endif

#ifdef PROFILING
import GHC.Stack
#endif
-- import qualified Debug.Trace


trace_ :: String -> a -> a
trace_ _ = id
-- trace_ = Debug.Trace.trace

-- -----------------------------------------------------------------------------
-- The environment

-- | The data we carry around in the Haxl monad.
data Env u = Env
  { cacheRef     :: {-# UNPACK #-} !(IORef (DataCache (IVar u)))
      -- ^ cached data fetches

  , memoRef      :: {-# UNPACK #-} !(IORef (DataCache (IVar u)))
      -- ^ memoized computations

  , flags        :: !Flags
      -- conservatively not unpacking, because this is passed
      -- to 'fetch' and would need to be rebuilt.

  , userEnv      :: u
      -- ^ user-supplied data, retrievable with 'env'

  , statsRef     :: {-# UNPACK #-} !(IORef Stats)
      -- ^ statistics, collected according to the 'report' level in 'flags'.

  , profLabel    :: ProfileLabel
      -- ^ current profiling label, see 'withLabel'

  , profRef      :: {-# UNPACK #-} !(IORef Profile)
      -- ^ profiling data, collected according to the 'report' level in 'flags'.

  , states       :: StateStore
      -- ^ Data sources and other components can store their state in
      -- here. Items in this store must be instances of 'StateKey'.

  , reqStoreRef :: {-# UNPACK #-} !(IORef (RequestStore u))
       -- ^ The set of requests that we have not submitted to data sources yet.
       -- Owned by the scheduler.

  , runQueueRef :: {-# UNPACK #-} !(IORef (JobList u))
       -- ^ runnable computations. Things get added to here when we wake up
       -- a computation that was waiting for something.  When the list is
       -- empty, either we're finished, or we're waiting for some data fetch
       -- to return.

  , completions :: {-# UNPACK #-} !(TVar [CompleteReq u])
       -- ^ Requests that have completed.  Modified by data sources
       -- (via putResult) and the scheduler.  Waiting for this list to
       -- become non-empty is how the scheduler blocks waiting for
       -- data fetches to return.

  , pendingWaits :: [IO ()]
       -- ^ this is a list of IO actions returned by 'FutureFetch'
       -- data sources.  These do a blocking wait for the results of
       -- some data fetch.

  , speculative :: {-# UNPACK #-} !Int
  }

type Caches u = (IORef (DataCache (IVar u)), IORef (DataCache (IVar u)))

caches :: Env u -> Caches u
caches env = (cacheRef env, memoRef env)

-- | Initialize an environment with a 'StateStore', an input map, a
-- preexisting 'DataCache', and a seed for the random number generator.
initEnvWithData :: StateStore -> u -> Caches u -> IO (Env u)
initEnvWithData states e (cref, mref) = do
  sref <- newIORef emptyStats
  pref <- newIORef emptyProfile
  rs <- newIORef noRequests          -- RequestStore
  rq <- newIORef JobNil
  comps <- newTVarIO []              -- completion queue
  return Env
    { cacheRef = cref
    , memoRef = mref
    , flags = defaultFlags
    , userEnv = e
    , states = states
    , statsRef = sref
    , profLabel = "MAIN"
    , profRef = pref
    , reqStoreRef = rs
    , runQueueRef = rq
    , completions = comps
    , pendingWaits = []
    , speculative = 0
    }

-- | Initializes an environment with 'StateStore' and an input map.
initEnv :: StateStore -> u -> IO (Env u)
initEnv states e = do
  cref <- newIORef emptyDataCache
  mref <- newIORef emptyDataCache
  initEnvWithData states e (cref,mref)

-- | A new, empty environment.
emptyEnv :: u -> IO (Env u)
emptyEnv = initEnv stateEmpty

speculate :: Env u -> Env u
speculate env@Env{..}
  | speculative == 0 = env { speculative = 1 }
  | otherwise = env

imperative :: Env u -> Env u
imperative env@Env{..}
  | speculative == 1 = env { speculative = 0 }
  | otherwise = env

-- -----------------------------------------------------------------------------
-- | The Haxl monad, which does several things:
--
--  * It is a reader monad for 'Env', which contains the current state
--    of the scheduler, including unfetched requests and the run queue
--    of computations.
--
--  * It is a concurrency, or resumption, monad. A computation may run
--    partially and return 'Blocked', in which case the framework should
--    perform the outstanding requests in the 'RequestStore', and then
--    resume the computation.
--
--  * The Applicative combinator '<*>' explores /both/ branches in the
--    event that the left branch is 'Blocked', so that we can collect
--    multiple requests and submit them as a batch.
--
--  * It contains IO, so that we can perform real data fetching.
--
newtype GenHaxl u a = GenHaxl
  { unHaxl :: Env u -> IO (Result u a) }


instance IsString a => IsString (GenHaxl u a) where
  fromString s = return (fromString s)

-- -----------------------------------------------------------------------------
-- JobList

-- | A list of computations together with the IVar into which they
-- should put their result.
--
-- This could be an ordinary list, but the optimised representation
-- saves space and time.
--
data JobList u
 = JobNil
 | forall a . JobCons
     (Env u)          -- See Note [make withEnv work] below.
     (GenHaxl u a)
     {-# UNPACK #-} !(IVar u a)
     (JobList u)

-- Note [make withEnv work]
--
-- The withEnv operation supplies a new Env for the scope of a GenHaxl
-- computation.  The problem is that the computation might be split
-- into pieces and put onto various JobLists, so we have to be sure to
-- use the correct Env when we execute the pieces. Furthermore, if one
-- of these pieces blocks and gets run again later, we must ensure to
-- restart it with the correct Env.  So we stash the Env along with
-- the continuation in the JobList.

appendJobList :: JobList u -> JobList u -> JobList u
appendJobList JobNil c = c
appendJobList c JobNil = c
appendJobList (JobCons a b c d) e = JobCons a b c $! appendJobList d e

lengthJobList :: JobList u -> Int
lengthJobList JobNil = 0
lengthJobList (JobCons _ _ _ j) = 1 + lengthJobList j


-- -----------------------------------------------------------------------------
-- IVar

-- | A synchronisation point.  It either contains a value, or a list
-- of computations waiting for the value.
newtype IVar u a = IVar (IORef (IVarContents u a))

data IVarContents u a
  = IVarFull (ResultVal a)
  | IVarEmpty (JobList u)
    -- morally this is a list of @a -> GenHaxl u ()@, but instead of
    -- using a function, each computation begins with `getIVar` to grab
    -- the value it is waiting for.  This is less type safe but a little
    -- faster (benchmarked with tests/MonadBench.hs).

newIVar :: IO (IVar u a)
newIVar = IVar <$> newIORef (IVarEmpty JobNil)

newFullIVar :: ResultVal a -> IO (IVar u a)
newFullIVar r = IVar <$> newIORef (IVarFull r)

getIVar :: IVar u a -> GenHaxl u a
getIVar (IVar !ref) = GenHaxl $ \_env -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok a) -> return (Done a)
    IVarFull (ThrowHaxl e) -> return (Throw e)
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty _ -> return (Blocked (IVar ref) (Cont (getIVar (IVar ref))))

-- Just a specialised version of getIVar, for efficiency in <*>
getIVarApply :: IVar u (a -> b) -> a -> GenHaxl u b
getIVarApply (IVar !ref) a = GenHaxl $ \_env -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok f) -> return (Done (f a))
    IVarFull (ThrowHaxl e) -> return (Throw e)
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty _ ->
      return (Blocked (IVar ref) (Cont (getIVarApply (IVar ref) a)))

putIVar :: IVar u a -> ResultVal a -> Env u -> IO ()
putIVar (IVar ref) a Env{..} = do
  e <- readIORef ref
  case e of
    IVarEmpty jobs -> do
      writeIORef ref (IVarFull a)
      modifyIORef' runQueueRef (appendJobList jobs)
    IVarFull{} -> error "putIVar: multiple put"

{-# INLINE addJob #-}
addJob :: Env u -> GenHaxl u b -> IVar u b -> IVar u a -> IO ()
addJob env !haxl !resultIVar (IVar !ref) =
  modifyIORef' ref $ \contents ->
    case contents of
      IVarEmpty list -> IVarEmpty (JobCons env haxl resultIVar list)
      _ -> addJobPanic

addJobPanic :: forall a . a
addJobPanic = error "addJob: not empty"


-- -----------------------------------------------------------------------------
-- ResultVal

-- | The contents of a full IVar.  We have to distinguish exceptions
-- thrown in the IO monad from exceptions thrown in the Haxl monad, so
-- that when the result is fetched using getIVar, we can throw the
-- exception in the right way.
data ResultVal a
  = Ok a
  | ThrowHaxl SomeException
  | ThrowIO SomeException

done :: ResultVal a -> IO (Result u a)
done (Ok a) = return (Done a)
done (ThrowHaxl e) = return (Throw e)
done (ThrowIO e) = throwIO e

eitherToResultThrowIO :: Either SomeException a -> ResultVal a
eitherToResultThrowIO (Right a) = Ok a
eitherToResultThrowIO (Left e)
  | Just HaxlException{} <- fromException e = ThrowHaxl e
  | otherwise = ThrowIO e

eitherToResult :: Either SomeException a -> ResultVal a
eitherToResult (Right a) = Ok a
eitherToResult (Left e) = ThrowHaxl e


-- -----------------------------------------------------------------------------
-- CompleteReq

-- | A completed request from a data source, containing the result,
-- and the 'IVar' representing the blocked computations.  The job of a
-- data source is just to add these to a queue ('completions') using
-- 'putResult'; the scheduler collects them from the queue and unblocks
-- the relevant computations.
data CompleteReq u
  = forall a . CompleteReq
      (Either SomeException a)
      !(IVar u a)  -- IVar because the result is cached
      {-# UNPACK #-} !Int64 -- see Note [tracking allocation in child threads]


{- Note [tracking allocation in child threads]

For a BackgroundFetch, we might be doing some of the work in a
separate thread, but we want to make sure that the parent thread gets
charged for the allocation, so that allocation limits still work.

The design is a bit tricky here.  We want to track the allocation
accurately but without adding much overhead.

The best way to propagate the allocation back from the child thread is
through putResult.  If we had some other method, we would also need a
way to synchronise it with the main runHaxl loop; the advantage of
putResult is that this is already a synchronisation method, because
runHaxl is waiting for the result of the dataFetch.

(slight wrinkle here: runHaxl might not wait for the result of the
dataFetch in the case where we do some speculative execution in
pAnd/pOr)

We need a special version of putResult for child threads
(putResultFromChildThread), because we don't want to propagate any
allocation from the runHaxl thread back to itself and count it twice.

We also want to capture the allocation as late as possible, so that we
count everything.  For that reason, we pass a Bool down from putResult
into the function in the ResultVar, and it reads the allocation
counter as the last thing before adding the result to the completions
TVar.

The other problem to consider is how to capture the allocation when
the child thread is doing multiple putResults.  Our solution here is
to ensure that the *last* one is a putResultFromChildThread, so it
captures all the allocation from everything leading up to it.

Why not reset the counter each time, so we could do multiple
putResultFromChildThreads?  Because the child thread might be using an
allocation limit itself, and changing the counter would mess it up.
-}

-- -----------------------------------------------------------------------------
-- Result

-- | The result of a computation is either 'Done' with a value, 'Throw'
-- with an exception, or 'Blocked' on the result of a data fetch with
-- a continuation.
data Result u a
  = Done a
  | Throw SomeException
  | forall b . Blocked -- Add an Int field with the cost
      {-# UNPACK #-} !(IVar u b)
      (Cont u a)
         -- ^ The 'IVar' is what we are blocked on; 'Cont' is the
         -- continuation.  This might be wrapped further if we're
         -- nested inside multiple '>>=', before finally being added
         -- to the 'IVar'.  Morally @b -> GenHaxl u a@, but see
         -- 'IVar',

instance (Show a) => Show (Result u a) where
  show (Done a) = printf "Done(%s)" $ show a
  show (Throw e) = printf "Throw(%s)" $ show e
  show Blocked{} = "Blocked"

{- Note [Exception]

How do we want to represent Haxl exceptions (those that are thrown by
"throw" in the Haxl monad)?

1) Explicitly via a Throw constructor in the Result type
2) Using throwIO in the IO monad

If we did (2), we would have to use an exception handler in <*>,
because an exception in the right-hand argument of <*> should not
necessarily be thrown by the whole computation - an exception on the
left should get priority, and the left might currently be Blocked.

We must be careful about turning IO monad exceptions into Haxl
exceptions.  An IO monad exception will normally propagate right
out of runHaxl and terminate the whole computation, whereas a Haxl
exception can get dropped on the floor, if it is on the right of
<*> and the left side also throws, for example.  So turning an IO
monad exception into a Haxl exception is a dangerous thing to do.
In particular, we never want to do it for an asynchronous exception
(AllocationLimitExceeded, ThreadKilled, etc.), because these are
supposed to unconditionally terminate the computation.

There are three places where we take an arbitrary IO monad exception and
turn it into a Haxl exception:

 * wrapFetchInCatch.  Here we want to propagate a failure of the
   data source to the callers of the data source, but if the
   failure came from elsewhere (an asynchronous exception), then we
   should just propagate it

 * cacheResult (cache the results of IO operations): again,
   failures of the IO operation should be visible to the caller as
   a Haxl exception, but we exclude asynchronous exceptions from
   this.

 * unsafeToHaxlException: assume the caller knows what they're
   doing, and just wrap all exceptions.
-}


-- -----------------------------------------------------------------------------
-- Cont

-- | A data representation of a Haxl continuation.  This is to avoid
-- repeatedly traversing a left-biased tree in a continuation, leading
-- O(n^2) complexity for some pathalogical cases - see the "seql" benchmark
-- in tests/MonadBench.hs.
-- See "A Smart View on Datatypes", Jaskelioff/Rivas, ICFP'15
data Cont u a
  = Cont (GenHaxl u a)
  | forall b. Cont u b :>>= (b -> GenHaxl u a)
  | forall b. (b -> a) :<$> (Cont u b)

toHaxl :: Cont u a -> GenHaxl u a
toHaxl (Cont haxl) = haxl
toHaxl (m :>>= k) = toHaxlBind m k
toHaxl (f :<$> x) = toHaxlFmap f x

toHaxlBind :: Cont u b -> (b -> GenHaxl u a) -> GenHaxl u a
toHaxlBind (m :>>= k) k2 = toHaxlBind m (k >=> k2)
toHaxlBind (Cont haxl) k = haxl >>= k
toHaxlBind (f :<$> x) k = toHaxlBind x (k . f)

toHaxlFmap :: (a -> b) -> Cont u a -> GenHaxl u b
toHaxlFmap f (m :>>= k) = toHaxlBind m (k >=> return . f)
toHaxlFmap f (Cont haxl) = f <$> haxl
toHaxlFmap f (g :<$> x) = toHaxlFmap (f . g) x

-- -----------------------------------------------------------------------------
-- Monad/Applicative instances

instance Monad (GenHaxl u) where
  return a = GenHaxl $ \_env -> return (Done a)
  GenHaxl m >>= k = GenHaxl $ \env -> do
    e <- m env
    case e of
      Done a -> unHaxl (k a) env
      Throw e -> return (Throw e)
      Blocked ivar cont -> trace_ ">>= Blocked" $
        return (Blocked ivar (cont :>>= k))
  fail msg = GenHaxl $ \_env ->
    return $ Throw $ toException $ MonadFail $ Text.pack msg

  -- We really want the Applicative version of >>
  (>>) = (*>)

instance Functor (GenHaxl u) where
  fmap = fmapGenHaxl

fmapGenHaxl :: (a -> b) -> GenHaxl t a -> GenHaxl t b
fmapGenHaxl f (GenHaxl m) = GenHaxl $ \env -> do
  r <- m env
  case r of
    Done a -> return (Done (f a))
    Throw e -> return (Throw e)
    Blocked ivar cont -> trace_ "fmap Blocked" $
      return (Blocked ivar (f :<$> cont))
{-# INLINE [1] fmapGenHaxl #-}
{-# RULES
"fmapGenHaxl/id"
  fmapGenHaxl (\x -> x) = id
 #-}

instance Applicative (GenHaxl u) where
  pure = return
  GenHaxl ff <*> GenHaxl aa = GenHaxl $ \env -> do
    rf <- ff env
    case rf of
      Done f -> do
        ra <- aa env
        case ra of
          Done a -> trace_ "A/Done/Done" $ return (Done (f a))
          Throw e -> trace_ "A/Done/Throw" $ return (Throw e)
          Blocked ivar fcont -> trace_ "A/Done/Blocked" $
            return (Blocked ivar (f :<$> fcont))
      Throw e -> trace_ "A/Throw" $ return (Throw e)
      Blocked ivar1 fcont -> do
         ra <- aa env
         case ra of
           Done a -> trace_ "A/Blocked/Done" $
             return (Blocked ivar1 (($ a) :<$> fcont))
           Throw e -> trace_ "A/Blocked/Throw" $
             return (Blocked ivar1 (fcont :>>= (\_ -> throw e)))
           Blocked ivar2 acont -> trace_ "A/Blocked/Blocked" $ do
              -- Note [Blocked/Blocked]
              if speculative env /= 0
                then
                  return (Blocked ivar1
                            (Cont (toHaxl fcont <*> toHaxl acont)))
                else do
                  i <- newIVar
                  addJob env (toHaxl fcont) i ivar1
                  let cont = acont :>>= \a -> getIVarApply i a
                  return (Blocked ivar2 cont)

-- Note [Blocked/Blocked]
--
-- This is the tricky case: we're blocked on both sides of the <*>.
-- We need to divide the computation into two pieces that may continue
-- independently when the resources they are blocked on become
-- available.  Moreover, the computation as a whole depends on the two
-- pieces.  It works like this:
--
--   ff <*> aa
--
-- becomes
--
--   (ff >>= putIVar i) <*> (do a <- aa; f <- getIVar i; return (f a))
--
-- where the IVar i is a new synchronisation point.  If the right side
-- gets to the `getIVar` first, it will block until the left side has
-- called 'putIVar'.
--
-- We can also do it the other way around:
--
--   (do ff <- f; getIVar i; return (ff a)) <*> (aa >>= putIVar i)
--
-- The first was slightly faster according to tests/MonadBench.hs.

instance Selective (GenHaxl u) where
  {-# INLINEABLE biselect #-}
  biselect f g x y = GenHaxl (biselectInner f g x y)

biselectInner :: (t -> Either a1 (b -> a1)) -> (a -> Either a1 b) -> GenHaxl u t -> GenHaxl u a -> Env u -> IO (Result u a1)
biselectInner f g = go
  where
    go (GenHaxl x) (GenHaxl y) env@Env{..} = do
      let !senv = speculate env
      rx <- x env -- non speculative
      case rx of
        Done a -> case f a of
          Left   c -> return (Done c)
          Right dc -> unHaxl ((either id dc . g) <$> GenHaxl y) env
        Throw e -> return (Throw e)

        Blocked ix x' -> do
          ry <- y senv -- speculative
          case ry of
            Done b -> case g b of
              Left  c -> return (Done c)
              Right d -> unHaxl ((either id ($d) . f) <$> GenHaxl x) env
            Throw e -> return (Throw e)
            Blocked _ y' -> return (Blocked ix (Cont (GenHaxl (go (toHaxl x') (toHaxl y')))))
            -- Note [biselect Blocked/Blocked]
            -- This will only wake up when ia is filled, which
            -- is whatever the left side was waiting for.  This is
            -- suboptimal because the right side might wake up first,
            -- but handling this non-determinism would involve a much
            -- more complicated implementation here.
{-# INLINE biselectInner #-}

-- -----------------------------------------------------------------------------
-- Env utils

-- | Extracts data from the 'Env'.
env :: (Env u -> a) -> GenHaxl u a
env f = GenHaxl $ \env -> return (Done (f env))

-- | Returns a version of the Haxl computation which always uses the
-- provided 'Env', ignoring the one specified by 'runHaxl'.
withEnv :: Env u -> GenHaxl u a -> GenHaxl u a
withEnv newEnv (GenHaxl m) = GenHaxl $ \_env -> do
  r <- m newEnv
  case r of
    Done a -> return (Done a)
    Throw e -> return (Throw e)
    Blocked ivar k ->
      return (Blocked ivar (Cont (withEnv newEnv (toHaxl k))))


-- -----------------------------------------------------------------------------
-- Exceptions

-- | Throw an exception in the Haxl monad
throw :: (Exception e) => e -> GenHaxl u a
throw e = GenHaxl $ \_env -> raise e

raise :: (Exception e) => e -> IO (Result u a)
raise e
#ifdef PROFILING
  | Just (HaxlException Nothing h) <- fromException somex = do
    stk <- currentCallStack
    return (Throw (toException (HaxlException (Just stk) h)))
  | otherwise
#endif
    = return (Throw somex)
  where
    somex = toException e

-- | Catch an exception in the Haxl monad
catch :: Exception e => GenHaxl u a -> (e -> GenHaxl u a) -> GenHaxl u a
catch (GenHaxl m) h = GenHaxl $ \env -> do
   r <- m env
   case r of
     Done a    -> return (Done a)
     Throw e | Just e' <- fromException e -> unHaxl (h e') env
             | otherwise -> return (Throw e)
     Blocked ivar k -> return (Blocked ivar (Cont (catch (toHaxl k) h)))

-- | Catch exceptions that satisfy a predicate
catchIf
  :: Exception e => (e -> Bool) -> GenHaxl u a -> (e -> GenHaxl u a)
  -> GenHaxl u a
catchIf cond haxl handler =
  catch haxl $ \e -> if cond e then handler e else throw e

-- | Returns @'Left' e@ if the computation throws an exception @e@, or
-- @'Right' a@ if it returns a result @a@.
try :: Exception e => GenHaxl u a -> GenHaxl u (Either e a)
try haxl = (Right <$> haxl) `catch` (return . Left)

-- | @since 0.3.1.0
instance Catch.MonadThrow (GenHaxl u) where throwM = Haxl.Core.Monad.throw
-- | @since 0.3.1.0
instance Catch.MonadCatch (GenHaxl u) where catch = Haxl.Core.Monad.catch


-- -----------------------------------------------------------------------------
-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Haxl
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenHaxl u a
unsafeLiftIO m = GenHaxl $ \_env -> Done <$> m

-- | Convert exceptions in the underlying IO monad to exceptions in
-- the Haxl monad.  This is morally unsafe, because you could then
-- catch those exceptions in Haxl and observe the underlying execution
-- order.  Not to be exposed to user code.
unsafeToHaxlException :: GenHaxl u a -> GenHaxl u a
unsafeToHaxlException (GenHaxl m) = GenHaxl $ \env -> do
  r <- m env `Exception.catch` \e -> return (Throw e)
  case r of
    Blocked cvar c ->
      return (Blocked cvar (Cont (unsafeToHaxlException (toHaxl c))))
    other -> return other

-- | Like 'try', but lifts all exceptions into the 'HaxlException'
-- hierarchy.  Uses 'unsafeToHaxlException' internally.  Typically
-- this is used at the top level of a Haxl computation, to ensure that
-- all exceptions are caught.
tryToHaxlException :: GenHaxl u a -> GenHaxl u (Either HaxlException a)
tryToHaxlException h = left asHaxlException <$> try (unsafeToHaxlException h)


-- -----------------------------------------------------------------------------

-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.  For
-- example, the generated code looks something like this:
--
-- > loadCache :: GenHaxl u ()
-- > loadCache = do
-- >   cacheRequest (ListWombats 3) (Right ([1,2,3]))
-- >   cacheRequest (CountAardvarks "abcabc") (Right (2))
--
dumpCacheAsHaskell :: GenHaxl u String
dumpCacheAsHaskell = dumpCacheAsHaskellFn "loadCache" "GenHaxl u ()"

-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.
--
-- Takes the name and type for the resulting function as arguments.
dumpCacheAsHaskellFn :: String -> String -> GenHaxl u String
dumpCacheAsHaskellFn fnName fnType = do
  ref <- env cacheRef  -- NB. cacheRef, not memoRef.  We ignore memoized
                       -- results when dumping the cache.
  let
    readIVar (IVar ref) = do
      r <- readIORef ref
      case r of
        IVarFull (Ok a) -> return (Just (Right a))
        IVarFull (ThrowHaxl e) -> return (Just (Left e))
        IVarFull (ThrowIO e) -> return (Just (Left e))
        IVarEmpty _ -> return Nothing

    mk_cr (req, res) =
      text "cacheRequest" <+> parens (text req) <+> parens (result res)
    result (Left e) = text "except" <+> parens (text (show e))
    result (Right s) = text "Right" <+> parens (text s)

  entries <- unsafeLiftIO $ do
    cache <- readIORef ref
    showCache cache readIVar

  let
    body = if null entries
      then text "return ()"
      else vcat (map mk_cr (concatMap snd entries))

  return $ show $
    text (fnName ++ " :: " ++ fnType) $$
    text (fnName ++ " = do") $$
      nest 2 body $$
    text "" -- final newline

-- -----------------------------------------------------------------------------
-- Parallel operations

-- Bind more tightly than .&&, .||
infixr 5 `pAnd`
infixr 4 `pOr`

-- | Parallel version of '(.||)'.  Both arguments are evaluated in
-- parallel, and if either returns 'True' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pOr'.  If one
-- argument returns 'True' before the other completes, then 'pOr'
-- returns 'True' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pOr :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
pOr x y = x <||> y


-- | Parallel version of '(.&&)'.  Both arguments are evaluated in
-- parallel, and if either returns 'False' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pAnd'.  If one
-- argument returns 'False' before the other completes, then 'pAnd'
-- returns 'False' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pAnd :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
pAnd x y = x <&&> y

-- Old implementations of pOr and pAnd, for performance analysis
pOrOld :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
GenHaxl a `pOrOld` GenHaxl b = GenHaxl $ \env@Env{..} -> do
  let !senv = speculate env
  ra <- a senv
  case ra of
    Done True -> return (Done True)
    Done False -> b env  -- not speculative
    Throw _ -> return ra
    Blocked ia a' -> do
      rb <- b senv
      case rb of
        Done True -> return rb
        Done False -> return ra
        Throw _ -> return rb
        Blocked _ b' -> return (Blocked ia (Cont (toHaxl a' `pOrOld` toHaxl b')))
          -- Note [pOr Blocked/Blocked]
          -- This will only wake up when ia is filled, which
          -- is whatever the left side was waiting for.  This is
          -- suboptimal because the right side might wake up first,
          -- but handling this non-determinism would involve a much
          -- more complicated implementation here.

pAndOld :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
GenHaxl a `pAndOld` GenHaxl b = GenHaxl $ \env@Env{..} -> do
  let !senv = speculate env
  ra <- a senv
  case ra of
    Done False -> return (Done False)
    Done True -> b env
    Throw _ -> return ra
    Blocked ia a' -> do
      rb <- b senv
      case rb of
        Done False -> return rb
        Done True -> return ra
        Throw _ -> return rb
        Blocked _ b' -> return (Blocked ia (Cont (toHaxl a' `pAndOld` toHaxl b')))
         -- See Note [pOr Blocked/Blocked]
