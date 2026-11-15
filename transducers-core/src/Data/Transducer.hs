{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- t'Reducer's are generalized data processing steps that can be reused across multiple collections.
-- They process elements one by one, can carry state, and have ability to short-circuit.
--
-- Transducers are functions that take one t'Reducer' and return another.
--
-- This module is intended to be imported qualified or used without implicit @Prelude@.
-- Function names collide with @base@ functions that operate on lists and other collections though
-- output types are sometimes modified for our t'Reducer's to remain total.
module Data.Transducer where

import Control.Applicative (Applicative (liftA2, pure, (*>), (<*), (<*>)))
import Control.Monad (Functor (fmap))
import Data.Bool (Bool (False, True), not, (&&), (||))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable)
import Data.Foldable qualified as Foldable
import Data.Function (const, id, ($))
import Data.Int (Int)
import Data.Kind (Constraint, Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord (compare, max, min, (<=), (>)), Ordering (EQ, GT, LT))
import Data.Semigroup (Semigroup, (<>))
import Prelude (Fractional (fromRational, (/)), Num ((*), (+), (-)), fromIntegral, seq)

-- for haddock
import Prelude qualified as Preldue ()

-- * Basic Definitions

-- |
-- @
-- data Reducer init a r
--              ^    ^ ^
--              |    | |
--              |    | +-- Accumulator that will be returned after reduction
--              |    |
--              |    +-- Elements that the reducer is processing
--              |
--              +-- Is sequence required to be non-empty
-- @
--
-- @since 1.0.0
type role Reducer nominal nominal representational

type Reducer :: Initial -> Type -> Type -> Type
data Reducer init a r = forall s. Reducer
  { state :: InitialState init a s
  -- ^ Current state of the reducer.
  , finalize :: s -> r
  -- ^ Convert the state to the final value.
  , step :: s -> a -> Reduced s
  -- ^ Consume next element of the sequence and update the state.
  }

-- | Change the final accumulator value.
--
-- Note that 'fmap' is different than 'map';
-- 'fmap' maps the result of the reduction while 'map' maps the input elements
--
-- @
-- 'fmap' :: (r -> x) -> t'Reducer' init a r -> t'Reducer' init a x
--  'map' :: (b -> a) -> t'Reducer' init a r -> t'Reducer' init b r
-- @
--
-- @since 1.0.0
instance Functor (Reducer init a) where
  {-# INLINE fmap #-}
  fmap f (Reducer state finalize step) = Reducer state (\s -> f (finalize s)) step

-- | 'liftA2' runs two reducers in parallel, together with @ApplicativeDo@ it allows to
-- ergonomically define new reducers that run multiple operations with a single input traversal:
--
-- ===== __Examples__
--
-- @
-- {\-# LANGUAGE ApplicativeDo #-\}
--
-- mean :: 'IsInitial' init => t'Reducer' init 'Int' ('Maybe' 'Int')
-- mean = do
--   total <- 'sum'
--   len <- 'length'
--   'pure' (case len of 0 -> 'Nothing'; _ -> 'Just' (total \`'Preldue.div'\` len))
-- @
--
-- @since 1.0.0
instance (IsInitial init) => Applicative (Reducer init a) where
  {-# INLINE pure #-}
  pure :: forall (r :: Type). r -> Reducer init a r
  pure r =
    Reducer
      { state = case initial @init of SZero -> (); SOne -> const (Reduced ())
      , finalize = const r
      , step = \_ _ -> Reduced ()
      }

  {-# INLINE liftA2 #-}
  liftA2 :: (x -> y -> r) -> Reducer init a x -> Reducer init a y -> Reducer init a r
  liftA2 f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
    Reducer
      { state = case initial @init of
          SZero -> (ZipFinishedNone, state1, state2)
          SOne -> \a -> zipInitial (state1 a) (state2 a)
      , finalize = \(_, s1, s2) -> f (finalize1 s1) (finalize2 s2)
      , step = \s a -> zipStepBoth step1 step2 s a a
      }

  {-# INLINE (<*>) #-}
  (<*>) :: Reducer init a (x -> r) -> Reducer init a x -> Reducer init a r
  (<*>) = liftA2 id

  {-# INLINE (*>) #-}
  (*>) :: Reducer init a x -> Reducer init a r -> Reducer init a r
  (*>) = liftA2 (\_ r -> r)

  {-# INLINE (<*) #-}
  (<*) :: Reducer init a r -> Reducer init a x -> Reducer init a r
  (<*) = liftA2 const

-- | '<>' will run both reducers on the same input and then apply '<>' to the resulting values
--
-- @('<>') = 'liftA2' ('<>')@
--
-- ===== __Examples__
--
-- >>> reduceList (intoList <> (take 3 |> intoList)) [1..5]
-- [1,2,3,4,5,1,2,3]
--
-- @since 1.0.0
instance (IsInitial init, Semigroup r) => Semigroup (Reducer init a r) where
  (<>) = liftA2 (<>)

-- | @'mempty' = 'pure' 'mempty'@
--
-- ===== __Examples__
--
-- >>> reduceList (pure 42) [1..5]
-- 42
--
-- @since 1.0.0
instance (IsInitial init, Monoid r) => Monoid (Reducer init a r) where
  mempty = pure mempty

-- | Result of the reduction process.
--
-- @since 1.0.0
type role Reduced representational

type Reduced :: Type -> Type
data Reduced a
  = -- | The reduction finished and the reducer step should not be called anymore.
    Reduced a
  | -- | The reduction is still in progress and the reducer step should be called again.
    Continue a

-- | @since 1.0.0
instance Functor Reduced where
  {-# INLINE fmap #-}
  fmap f = \case
    Reduced a -> Reduced (f a)
    Continue a -> Continue (f a)

-- | Extract the result of the reduction.
--
-- ===== __Examples__
--
-- >>> unReduced (Reduced 42)
-- 42
--
-- >>> unReduced (Continue 42)
-- 42
--
-- @since 1.0.0
{-# INLINE unReduced #-}
unReduced :: forall (a :: Type). Reduced a -> a
unReduced = \case
  Reduced a -> a
  Continue a -> a

-- | Whether a reducer can start without input or needs its first element.
--
-- @since 1.0.0
type Initial :: Type
data Initial
  = -- | Can reduce an empty sequence.
    Zero
  | -- | Needs a non-empty sequence.
    One

-- | A ready state for Zero, or an initializer consuming the first element for One.
--
-- @since 1.0.0
type InitialState :: Initial -> Type -> Type -> Type
type family InitialState (init :: Initial) (a :: Type) (s :: Type) :: Type where
  InitialState 'Zero _a s = s
  InitialState 'One a s = a -> Reduced s

-- | Singleton witness for a reducer's initialization requirement.
--
-- @since 1.0.0
type role SInitial nominal

type SInitial :: Initial -> Type
data SInitial init where
  SZero :: SInitial 'Zero
  SOne :: SInitial 'One

-- | Supported initialization modes.
--
-- This typeclass is always satisfied.
--
-- @since 1.0.0
type IsInitial :: Initial -> Constraint
class IsInitial init where
  initial :: SInitial init

-- @since 1.0.0
instance IsInitial 'Zero where
  initial = SZero

-- @since 1.0.0
instance IsInitial 'One where
  initial = SOne

-- * Runners

-- $runners
-- To run a t'Reducer' it must be fed input elements until it finishes the process.
-- All of @reduce*@ functions take a t'Reducer' and a source.

-- | Run a reducer on all list elements.
--
-- ===== __Examples__
--
-- >>> reduceList sum [1,2,3]
-- 6
--
-- @since 1.0.0
{-# INLINE reduceList #-}
reduceList :: forall (a :: Type) (r :: Type). Reducer 'Zero a r -> [a] -> r
reduceList = reduceFoldr

-- | Run a reducer on all elements of a non-empty list.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty @'Zero sum (1 :| [2,3])
-- 6
--
-- >>> reduceNonEmpty maximum1 (2 :| [1,4,3])
-- 4
--
-- @since 1.0.0
{-# INLINE reduceNonEmpty #-}
reduceNonEmpty :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => Reducer init a r -> NonEmpty a -> r
reduceNonEmpty (Reducer state finalize step) (a :| as) =
  case startInitial (initial @init) step state a of
    Reduced s -> finalize s
    Continue s -> reduceList (Reducer s finalize step) as

-- | Run a reducer on a single element.
--
-- ===== __Examples__
--
-- >>> reduceSingleton @'Zero head 1
-- Just 1
--
-- >>> reduceSingleton @'Zero (drop 1 |> head) 1
-- Nothing
--
-- @since 1.0.0
{-# INLINE reduceSingleton #-}
reduceSingleton :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => Reducer init a r -> a -> r
reduceSingleton reducer a = reduceList (feed1 a reducer) []

-- | Run a reducer on an infinite sequence of repeated function applications.
--
-- ===== __Examples__
--
-- >>> reduceIterate @'Zero (take 10 |> intoList) (+3) 42
-- [42,45,48,51,54,57,60,63,66,69]
--
-- @since 1.0.0
{-# INLINE reduceIterate #-}
reduceIterate :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => Reducer init a r -> (a -> a) -> a -> r
reduceIterate (Reducer state finalize step) f a =
  case startInitial (initial @init) step state a of
    Reduced s -> finalize s
    Continue s -> finalize (go s (f a))
  where
    go s a = case step s a of
      Reduced s' -> s'
      Continue s' -> go s' (f a)

-- | Run a reducer on an infinite sequence of the same value.
--
-- ===== __Examples__
--
-- >>> reduceRepeat @'Zero (take 10 |> intoList) 42
-- [42,42,42,42,42,42,42,42,42,42]
--
-- @since 1.0.0
{-# INLINE reduceRepeat #-}
reduceRepeat :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => Reducer init a r -> a -> r
reduceRepeat (Reducer state finalize step) a =
  case startInitial (initial @init) step state a of
    Reduced s -> finalize s
    Continue s -> finalize (go s)
  where
    go s = case step s a of
      Reduced r' -> r'
      Continue r' -> go r'

-- | Run a reducer on a finite sequence of the same value.
--
-- ===== __Examples__
--
-- >>> reduceReplicate intoList 10 42
-- [42,42,42,42,42,42,42,42,42,42]
--
-- @since 1.0.0
{-# INLINE reduceReplicate #-}
reduceReplicate :: forall (a :: Type) (r :: Type). Reducer 'Zero a r -> Int -> a -> r
reduceReplicate (Reducer state finalize step) n a = finalize (go state n)
  where
    go s n =
      if n <= 0 then
        s
      else case step s a of
        Reduced s' -> s'
        Continue s' -> go s' (n - 1)

-- | Run a reducer on any 'Foldable' by feeding elements with 'Foldable.foldr'.
--
-- ===== __Examples__
--
-- >>> import Data.Complex (Complex ((:+)))
-- >>> reduceFoldr head (1 :+ 2)
-- Just 1
--
-- @since 1.0.0
{-# INLINE reduceFoldr #-}
reduceFoldr ::
  forall (a :: Type) (r :: Type) (f :: Type -> Type).
  (Foldable f) =>
  Reducer 'Zero a r ->
  f a ->
  r
reduceFoldr (Reducer state finalize step) f = finalize (Foldable.foldr go id f state)
  where
    go a k s =
      case step s a of
        Reduced s' -> s'
        Continue s' -> k s'

-- * Reducers

-- | Get the sum of the elements in the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList sum [1..10]
-- 55
--
-- >>> reduceList sum ([] :: [Int])
-- 0
--
-- @since 1.0.0
{-# INLINE sum #-}
sum :: forall (init :: Initial) (r :: Type). (Num r, IsInitial init) => Reducer init r r
sum = initializeReducer $ linearReducer 0 (+)

-- | Get the product of the elements in the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList product [1..5]
-- 120
--
-- >>> reduceList product ([] :: [Int])
-- 1
--
-- @since 1.0.0
{-# INLINE product #-}
product :: forall (init :: Initial) (r :: Type). (Num r, IsInitial init) => Reducer init r r
product = initializeReducer $ linearReducer 1 (*)

-- | Get the largest element of the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList maximum [1,3,2]
-- Just 3
--
-- >>> reduceList maximum []
-- Nothing
--
-- @since 1.0.0
{-# INLINE maximum #-}
maximum :: forall (init :: Initial) (a :: Type). (Ord a, IsInitial init) => Reducer init a (Maybe a)
maximum = initializeReducer $ linearReducer Nothing (\r a -> max (Just a) r)

-- | Get the largest element of the non-empty sequence.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty maximum1 (2 :| [1,4,3])
-- 4
--
-- @since 1.0.0
{-# INLINE maximum1 #-}
maximum1 :: forall (a :: Type). (Ord a) => Reducer 'One a a
maximum1 = Reducer Continue id (\s a -> let !r = max s a in Continue r)

-- | Get the smallest element of the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList minimum [1,3,2]
-- Just 1
--
-- >>> reduceList minimum []
-- Nothing
--
-- @since 1.0.0
{-# INLINE minimum #-}
minimum :: forall (init :: Initial) (a :: Type). (Ord a, IsInitial init) => Reducer init a (Maybe a)
minimum = initializeReducer $ linearReducer Nothing $ \r a ->
  case r of
    Nothing -> Just a
    Just r' -> let !m = min r' a in Just m

-- | Get the largest element of the non-empty sequence.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty minimum1 (2 :| [1,4,3])
-- 1
--
-- @since 1.0.0
{-# INLINE minimum1 #-}
minimum1 :: forall (a :: Type). (Ord a) => Reducer 'One a a
minimum1 = Reducer Continue id (\s a -> let !r = min s a in Continue r)

-- | Get the length of the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList length [1,3,2]
-- 3
--
-- @since 1.0.0
{-# INLINE length #-}
length :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a Int
length = initializeReducer $ linearReducer 0 (\acc _ -> acc + 1)

-- | Compare length of the sequence against a constant. It short-circuits upon reaching 'GT'
-- thus terminates even on infinite sequences.
--
-- ===== __Examples__
--
-- >>> reduceList (compareLength 0) []
-- EQ
--
-- >>> reduceList (compareLength 1) []
-- LT
--
-- >>> reduceList (compareLength 1) ['a']
-- EQ
--
-- >>> reduceList (compareLength 1) ['a','b']
-- GT
--
-- >>> reduceList (compareLength 100) [0..]
-- GT
--
-- @since 1.0.0
{-# INLINE compareLength #-}
compareLength :: forall (init :: Initial) (a :: Type). (IsInitial init) => Int -> Reducer init a Ordering
compareLength n =
  initializeReducer $
    Reducer
      { state = (compare 0 n, 0)
      , finalize = \(r, _) -> r
      , step = \(_, s) _ ->
          case compare (s + 1) n of
            LT -> Continue (LT, s + 1)
            EQ -> Continue (EQ, s + 1)
            GT -> Reduced (GT, s + 1)
      }

-- | Get the mean (average) of the elements.
--
-- ===== __Examples__
--
-- >>> reduceList mean [1..5]
-- Just 3.0
--
-- >>> reduceList mean []
-- Nothing
--
-- @since 1.0.0
{-# INLINE mean #-}
mean :: forall (init :: Initial) (a :: Type). (IsInitial init, Fractional a) => Reducer init a (Maybe a)
mean =
  initializeReducer $
    Reducer
      { state = (0 :: Int, 0 :: a)
      , finalize = \(len, sum) -> if len == 0 then Nothing else Just (sum / fromRational (fromIntegral len))
      , step = \(!len, !sum) a -> Continue (len + 1, sum + a)
      }

-- | Get the mean (average) of non-empty sequence of elements.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty mean1 (1:|[2..5])
-- 3.0
--
-- @since 1.0.0
{-# INLINE mean1 #-}
mean1 :: forall (a :: Type). (Fractional a) => Reducer 'One a a
mean1 =
  Reducer
    { state = \a -> Continue (1 :: Int, a)
    , finalize = \(len, sum) -> sum / fromRational (fromIntegral len)
    , step = \(!len, !sum) a -> Continue (len + 1, sum + a)
    }

-- | Get the variance (population) of the elements.
--
-- ===== __Examples__
--
-- >>> reduceList variance [1,5,3,9,2]
-- Just 8.0
--
-- >>> reduceList variance []
-- Nothing
--
-- @since 1.0.0
{-# INLINE variance #-}
variance :: forall (init :: Initial) (a :: Type). (Fractional a, IsInitial init) => Reducer init a (Maybe a)
variance =
  initializeReducer $
    Reducer
      { state = (0, 0, 0 :: Int)
      , finalize = \(total, totalSq, len) ->
          if len == 0 then
            Nothing
          else
            let m = total / fromRational (fromIntegral len)
             in Just ((totalSq / fromRational (fromIntegral len)) - (m * m))
      , step = \(!total, !totalSq, !n) a -> Continue (total + a, totalSq + (a * a), n + 1)
      }

-- | Get the variance (population) of non-empty sequence of elements.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty variance1 (1 :| [5,3,9,2])
-- 8.0
--
-- @since 1.0.0
{-# INLINE variance1 #-}
variance1 :: forall (a :: Type). (Fractional a) => Reducer 'One a a
variance1 =
  Reducer
    { state = \a -> Continue (a, a * a, 1 :: Int)
    , finalize = \(total, totalSq, len) ->
        let m = total / fromRational (fromIntegral len)
         in (totalSq / fromRational (fromIntegral len)) - (m * m)
    , step = \(!total, !totalSq, !n) a -> Continue (total + a, totalSq + (a * a), n + 1)
    }

-- | Get the disjunction of 'Bool's.
--
-- ===== __Examples__
--
-- >>> reduceList or [True,False,True]
-- True
--
-- >>> reduceList or [False,False]
-- False
--
-- 'or' short-circuits on first 'True' value
--
-- >>> reduceRepeat @'Zero or True
-- True
--
-- @since 1.0.0
{-# INLINE or #-}
or :: forall (init :: Initial). (IsInitial init) => Reducer init Bool Bool
or = initializeReducer $ any id

-- | Get the conjunction of 'Bool's.
--
-- ===== __Examples__
--
-- >>> reduceList and [True,False,True]
-- False
--
-- >>> reduceList and [True,True]
-- True
--
-- 'and' short-circuits on first 'False' value
--
-- >>> reduceRepeat @'Zero and False
-- False
--
-- @since 1.0.0
{-# INLINE and #-}
and :: forall (init :: Initial). (IsInitial init) => Reducer init Bool Bool
and = initializeReducer $ all id

-- | Get the conjunction of the results of passed predicates.
--
-- ===== __Examples__
--
-- >>> reduceList (any even) [1,2,3]
-- True
--
-- >>> reduceList (any even) [1,3]
-- False
--
-- 'any' short-circuits on first 'True' value
--
-- >>> reduceRepeat @'Zero (any even) 2
-- True
--
-- @since 1.0.0
{-# INLINE any #-}
any :: forall (init :: Initial) (a :: Type). (IsInitial init) => (a -> Bool) -> Reducer init a Bool
any pred =
  initializeReducer $
    Reducer
      { state = False
      , finalize = id
      , step = \_ a ->
          if pred a then
            Reduced True
          else
            Continue False
      }

-- | Get the conjunction of the results of passed predicates.
--
-- ===== __Examples__
--
-- >>> reduceList (all even) [4,2,6]
-- True
--
-- >>> reduceList (all even) [1,2,3]
-- False
--
-- 'and' short-circuits on first 'False' value
--
-- >>> reduceList (all even) [1..]
-- False
--
-- @since 1.0.0
{-# INLINE all #-}
all :: forall (init :: Initial) (a :: Type). (IsInitial init) => (a -> Bool) -> Reducer init a Bool
all pred =
  initializeReducer $
    Reducer
      { state = True
      , finalize = id
      , step = \_ a ->
          if pred a then
            Continue True
          else
            Reduced False
      }

-- | Check if sequence is empty.
--
-- ===== __Examples__
--
-- >>> reduceList null []
-- True
--
-- >>> reduceList null [1..]
-- False
--
-- @since 1.0.0
{-# INLINE null #-}
null :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a Bool
null =
  initializeReducer $
    Reducer
      { state = True
      , finalize = id
      , step = \_ _ -> Reduced False
      }

-- | Extract the first element if exists.
--
-- ===== __Examples__
--
-- >>> reduceList head [1,2,3]
-- Just 1
--
-- >>> reduceList head [1..]
-- Just 1
--
-- >>> reduceList head []
-- Nothing
--
-- @since 1.0.0
{-# INLINE head #-}
head :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a (Maybe a)
head =
  initializeReducer $
    Reducer
      { state = Nothing
      , finalize = id
      , step = \_ a -> Reduced (Just a)
      }

-- | Extract the first element from non-empty sequence.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty head1 (2 :| [1,4,3])
-- 2
--
-- @since 1.0.0
{-# INLINE head1 #-}
head1 :: forall (a :: Type). Reducer 'One a a
head1 = Reducer Reduced id (\s _ -> Reduced s)

-- | Extract the last element if exists.
--
-- ===== __Examples__
--
-- >>> reduceList last [1,2,3]
-- Just 3
--
-- >>> reduceList last []
-- Nothing
--
-- @since 1.0.0
{-# INLINE last #-}
last :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a (Maybe a)
last = initializeReducer $ linearReducer Nothing (const Just)

-- | Extract the last element from non-empty sequence.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty last1 (2 :| [1,4,3])
-- 3
--
-- @since 1.0.0
{-# INLINE last1 #-}
last1 :: forall (a :: Type). Reducer 'One a a
last1 = Reducer Continue id (\_ !a -> Continue a)

-- | Get the first element for which the passed predicate returns 'True', if exists.
--
-- ===== __Examples__
--
-- >>> reduceList (find even) [1,2,3]
-- Just 2
--
-- >>> reduceList (find even) [1,3,5]
-- Nothing
--
-- @since 1.0.0
{-# INLINE find #-}
find :: forall (init :: Initial) (a :: Type). (IsInitial init) => (a -> Bool) -> Reducer init a (Maybe a)
find pred =
  initializeReducer $
    Reducer
      { state = Nothing
      , finalize = id
      , step = \_ a ->
          if pred a then
            Reduced (Just a)
          else
            Continue Nothing
      }

-- | Check if any elements is equal (using '==') to the one passed.
--
-- ===== __Examples__
--
-- >>> reduceList (elem 2) [1,2,3]
-- True
--
-- >>> reduceList (elem 2) [1,3,5]
-- False
--
-- @since 1.0.0
{-# INLINE elem #-}
elem :: forall (init :: Initial) (a :: Type). (Eq a, IsInitial init) => a -> Reducer init a Bool
elem a = initializeReducer $ elemBy (a ==)

-- | Check if for any elements the passed predicate returns 'True'.
--
-- ===== __Examples__
--
-- >>> reduceList (elemBy even) [1,2,3]
-- True
--
-- >>> reduceList (elemBy even) [1,3,5]
-- False
--
-- @since 1.0.0
{-# INLINE elemBy #-}
elemBy :: forall (init :: Initial) (a :: Type). (IsInitial init) => (a -> Bool) -> Reducer init a Bool
elemBy pred =
  initializeReducer $
    Reducer
      { state = False
      , finalize = id
      , step = \_ a ->
          if pred a then
            Reduced True
          else
            Continue False
      }

-- | Apply '<>' to all elements in the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList sconcat ["hello"," ","world"]
-- Just "hello world"
--
-- >>> reduceList sconcat []
-- Nothing
--
-- @since 1.0.0
{-# INLINE sconcat #-}
sconcat :: forall (init :: Initial) (a :: Type). (Semigroup a, IsInitial init) => Reducer init a (Maybe a)
sconcat = initializeReducer $ linearReducer Nothing $ \acc a -> case acc of
  Nothing -> Just a
  Just acc -> Just (acc <> a)

-- | Apply '<>' to all elements in the sequence.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty sconcat1 ("hello" :| [" ","world"])
-- "hello world"
--
-- >>> reduceNonEmpty sconcat1 ("hello" :| [])
-- "hello"
--
-- @since 1.0.0
{-# INLINE sconcat1 #-}
sconcat1 :: forall (a :: Type). (Semigroup a) => Reducer 'One a a
sconcat1 = Reducer Continue id (\s a -> let !r = s <> a in Continue r)

-- | Apply '<>' to all elements in the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList mconcat ["hello"," ","world"]
-- "hello world"
--
-- >>> reduceList mconcat ([] :: [String])
-- ""
--
-- @since 1.0.0
{-# INLINE mconcat #-}
mconcat :: forall (init :: Initial) (a :: Type). (Monoid a, IsInitial init) => Reducer init a a
mconcat = initializeReducer $ linearReducer mempty (<>)

-- | Collect all elements into a list.
--
-- ===== __Examples__
--
-- >>> reduceReplicate intoList 10 42
-- [42,42,42,42,42,42,42,42,42,42]
--
-- @since 1.0.0
{-# INLINE intoList #-}
intoList :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a [a]
intoList =
  initializeReducer $
    Reducer
      { state = []
      , finalize = List.reverse
      , step = \as a -> Continue (a : as)
      }

-- | Collect all elements into a non-empty list or @Nothing@ if it does not contain any elements.
--
-- ===== __Examples__
--
-- >>> reduceList intoNonEmpty [1,2,3]
-- Just (1 :| [2,3])
--
-- >>> reduceList intoNonEmpty []
-- Nothing
--
-- @since 1.0.0
{-# INLINE intoNonEmpty #-}
intoNonEmpty :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a (Maybe (NonEmpty a))
intoNonEmpty =
  initializeReducer $
    Reducer
      { state = Nothing
      , finalize = fmap NonEmpty.reverse
      , step = \as a ->
          case as of
            Nothing -> Continue (Just (a :| []))
            Just (b :| bs) -> Continue (Just (a :| b : bs))
      }

-- | Collect all elements from non-empty sequence into a non-empty list.
--
-- ===== __Examples__
--
-- >>> reduceNonEmpty intoNonEmpty1 (1 :| [2,3])
-- 1 :| [2,3]
--
-- @since 1.0.0
{-# INLINE intoNonEmpty1 #-}
intoNonEmpty1 :: forall (a :: Type). Reducer 'One a (NonEmpty a)
intoNonEmpty1 =
  Reducer
    { state = \a -> Continue (a :| [])
    , finalize = NonEmpty.reverse
    , step = \(b :| bs) a -> Continue (a :| b : bs)
    }

-- | Discard the rest of the elements. It works on infinite sequences.
--
-- It seems pointless but can be useful when a transducer takes multiple reducers
-- and you care only about some of them.
--
-- ===== __Examples__
--
-- >>> reduceList discard [0..]
-- ()
--
-- @since 1.0.0
{-# INLINE discard #-}
discard :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a ()
discard =
  initializeReducer $
    Reducer
      { state = ()
      , finalize = const ()
      , step = \_ _ -> Reduced ()
      }

-- * Transducers

-- | Transducer composition operator.
--
-- Transducers are executed left to right which may get quite counter-intuitive to read comparing
-- to @base@ list operators so this operator can be used to clearly indicate the flow of elements.
--
-- Note that this is just like '$' but specialized to t'Reducer's.
--
-- ===== __Examples__
--
-- >>> reduceList (drop 2 |> take 3 |> intoList) [1..]
-- [3,4,5]
--
-- Note that second '|>' is technically redundant but makes code more readable.
--
-- >>> reduceList (drop 2 |> take 3 intoList) [1..]
-- [3,4,5]
--
-- Code that uses '|>' is equivalent to
--
-- >>> reduceList (drop 2 (take 3 intoList)) [1..]
-- [3,4,5]
--
-- @since 1.0.0
{-# INLINE (|>) #-}
(|>) ::
  forall (i :: Initial) (j :: Initial) (a :: Type) (b :: Type) (r :: Type).
  (Reducer i a r -> Reducer j b r) ->
  Reducer i a r ->
  Reducer j b r
(|>) transducer = transducer

infixr 5 |>

-- | Apply passed function to every element in the sequence
--
-- ===== __Examples__
--
-- >>> reduceList (map even |> intoList) [2,6,4,5,8]
-- [True,True,True,False,True]
--
-- @since 1.0.0
{-# INLINE map #-}
map ::
  forall (init :: Initial) (a :: Type) (b :: Type) (r :: Type).
  (IsInitial init) =>
  (b -> a) ->
  Reducer init a r ->
  Reducer init b r
map f (Reducer state finalize step) =
  Reducer
    { state = case initial @init of
        SZero -> state
        SOne -> \b -> state (f b)
    , finalize = finalize
    , step = \s b -> step s (f b)
    }

-- | Take first @n@ elements from the beginning of the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList (take 3 |> intoList) [1..]
-- [1,2,3]
--
-- @since 1.0.0
{-# INLINE take #-}
take ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Int ->
  Reducer 'Zero a r ->
  Reducer init a r
take n (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = (n, state)
      , finalize = \(_, state) -> finalize state
      , step = \(currN, s) a ->
          if currN <= 0 then
            Reduced (currN, s)
          else
            let currN' = currN - 1
             in case step s a of
                  Reduced s' -> Reduced (currN', s')
                  Continue s' ->
                    if currN' <= 0 then
                      Reduced (currN', s')
                    else
                      Continue (currN', s')
      }

-- | Keep taking elements from the sequence as long as the passed predicate returns 'True'.
-- Note that it short-circuits upon first value that fails the predicate even if some further value
-- would pass it.
--
-- ===== __Examples__
--
-- >>> reduceList (takeWhile even |> intoList) [2,6,4,5,8]
-- [2,6,4]
--
-- @since 1.0.0
{-# INLINE takeWhile #-}
takeWhile ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  (a -> Bool) ->
  Reducer 'Zero a r ->
  Reducer init a r
takeWhile pred (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = state
      , finalize = finalize
      , step = \s a ->
          if pred a then
            step s a
          else
            Reduced s
      }

-- | Drop (skip/remove) first @n@ elements from the beginning of the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList (drop 3 |> intoList) [1..5]
-- [4,5]
--
-- @since 1.0.0
{-# INLINE drop #-}
drop ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Int ->
  Reducer 'Zero a r ->
  Reducer init a r
drop n (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = (n, state)
      , finalize = \(_, state) -> finalize state
      , step = \(currN, s) a ->
          if currN <= 0 then
            fmap (currN,) (step s a)
          else
            Continue (currN - 1, s)
      }

-- | Keep dropping elements from the sequence as long as the passed predicate returns 'True'.
-- Note that upon first value that fails the predicate the whole remaining sequence will be forwarded
-- even if some further value would pass it.
--
-- ===== __Examples__
--
-- >>> reduceList (dropWhile even |> intoList) [2,6,4,5,8]
-- [5,8]
--
-- @since 1.0.0
{-# INLINE dropWhile #-}
dropWhile ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  (a -> Bool) ->
  Reducer 'Zero a r ->
  Reducer init a r
dropWhile pred (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = (False, state)
      , finalize = \(_, state) -> finalize state
      , step = \(finishedDropping, s) a ->
          if finishedDropping then
            fmap (True,) (step s a)
          else
            if pred a then
              Continue (False, s)
            else
              fmap (True,) (step s a)
      }

-- | Keep only elements for which the passed predicate returns 'True'.
--
-- ===== __Examples__
--
-- >>> reduceList (filter even |> intoList) [1..10]
-- [2,4,6,8,10]
--
-- @since 1.0.0
{-# INLINE filter #-}
filter ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  (a -> Bool) ->
  Reducer 'Zero a r ->
  Reducer init a r
filter pred (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = state
      , finalize = finalize
      , step = \s a ->
          if pred a then
            step s a
          else
            Continue s
      }

-- | Keep only elements that are mapped to 'Just'.
--
-- ===== __Examples__
--
-- >>> import Text.Read (readMaybe)
-- >>> reduceList (mapMaybe (readMaybe @Int) |> intoList) ["1","foo","2","bar"]
-- [1,2]
--
-- @since 1.0.0
{-# INLINE mapMaybe #-}
mapMaybe ::
  forall (init :: Initial) (a :: Type) (b :: Type) (r :: Type).
  (IsInitial init) =>
  (b -> Maybe a) ->
  Reducer 'Zero a r ->
  Reducer init b r
mapMaybe f (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = state
      , finalize = finalize
      , step = \s a ->
          case f a of
            Nothing -> Continue s
            Just a' -> step s a'
      }

-- | Keep only 'Just' elements.
--
-- ===== __Examples__
--
-- >>> reduceList (catMaybes |> intoList) [Just 1,Nothing,Just 2,Just 3,Nothing]
-- [1,2,3]
--
-- @since 1.0.0
{-# INLINE catMaybes #-}
catMaybes ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Reducer 'Zero a r ->
  Reducer init (Maybe a) r
catMaybes (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = state
      , finalize = finalize
      , step = \s -> \case
          Nothing -> Continue s
          Just a -> step s a
      }

-- | Insert an element in between every element in the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList (intersperse 0 |> intoList) [1,2,3]
-- [1,0,2,0,3]
--
-- @since 1.0.0
{-# INLINE intersperse #-}
intersperse ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  a ->
  Reducer init a r ->
  Reducer init a r
intersperse middle (Reducer state finalize step) =
  Reducer
    { state = case initial @init of
        SZero -> (False, state)
        SOne -> \a -> fmap (True,) (state a)
    , finalize = \(_, state) -> finalize state
    , step = \(acc, s) a ->
        if acc then case step s middle of
          Reduced s2 -> Reduced (True, s2)
          Continue s2 -> fmap (True,) (step s2 a)
        else
          fmap (True,) (step s a)
    }

-- | Flatten a sequence of @[a]@ into a sequence of @a@.
--
-- ===== __Examples__
--
-- >>> reduceList (concatList |> intoList) [[1,2],[3,4,5]]
-- [1,2,3,4,5]
--
-- @since 1.0.0
{-# INLINE concatList #-}
concatList ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Reducer 'Zero a r ->
  Reducer init [a] r
concatList (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = state
      , finalize = finalize
      , step = step'
      }
  where
    step' s = \case
      [] -> Continue s
      (a : as) -> case step s a of
        Reduced s' -> Reduced s'
        Continue s' -> step' s' as

-- | Flatten a sequence of @NonEmpty a@ into a sequence of @a@.
--
-- ===== __Examples__
--
-- >>> reduceList (concatNonEmpty |> intoList) [1:|[2],3:|[4,5]]
-- [1,2,3,4,5]
--
-- @since 1.0.0
{-# INLINE concatNonEmpty #-}
concatNonEmpty ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Reducer init a r ->
  Reducer init (NonEmpty a) r
concatNonEmpty (Reducer state finalize step) =
  Reducer
    { state = case initial @init of
        SZero -> state
        SOne -> \(a :| as) -> go (state a) as
    , finalize = finalize
    , step = \s (a :| as) -> go (step s a) as
    }
  where
    go (Reduced s) _ = Reduced s
    go s [] = s
    go (Continue s) (a : as) = go (step s a) as

-- | Remove duplicate elements and keep only the first occurrence that are equal.
--
-- Note that it has the same performance problems as 'List.nub'.
--
-- ===== __Examples__
--
-- >>> reduceList (nub |> intoList) [1,2,1,3,3,2,2]
-- [1,2,3]
--
-- @since 1.0.0
{-# INLINE nub #-}
nub ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init, Eq a) =>
  Reducer init a r ->
  Reducer init a r
nub = nubBy (==)

-- | Remove duplicate elements and keep only the first occurrence that are equal
-- accordingly to passed predicate.
--
-- Note that it has the same performance problems as 'List.nubBy'.
--
-- ===== __Examples__
--
-- Get elements that are not equal to each other modulo @5@.
--
-- >>> reduceList (nubBy (\lhs rhs -> lhs `mod` 5 == rhs `mod` 5) |> intoList) [5,10,1,6,7,12]
-- [5,1,7]
--
-- @since 1.0.0
{-# INLINE nubBy #-}
nubBy ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  (a -> a -> Bool) ->
  Reducer init a r ->
  Reducer init a r
nubBy eq (Reducer state finalize step) =
  Reducer
    { state = case initial @init of
        SZero -> ([], state)
        SOne -> \a -> fmap ([a],) (state a)
    , finalize = \(_, state) -> finalize state
    , step = \(seen, s) a ->
        if list_elemBy a seen then
          Continue (seen, s)
        else
          fmap (a : seen,) (step s a)
    }
  where
    list_elemBy :: a -> [a] -> Bool
    list_elemBy _ [] = False
    list_elemBy y (x : xs) = x `eq` y || list_elemBy y xs

-- | Preserve the first element and run the passed reducer on the remaining sequence.
-- 'Nothing' if the sequence has no elements.
--
-- ===== __Examples__
--
-- >>> reduceList (uncons sum) [1,2,3]
-- Just (1,5)
--
-- @since 1.0.0
{-# INLINE uncons #-}
uncons ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Reducer 'Zero a r ->
  Reducer init a (Maybe (a, r))
uncons (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = (Nothing, state)
      , finalize = \(r, s) -> case r of
          Nothing -> Nothing
          Just fst -> Just (fst, finalize s)
      , step = \(mr, s) a -> case mr of
          Nothing -> Continue (Just a, s)
          Just fst -> fmap (Just fst,) (step s a)
      }

-- | Preserve the last element and run the passed reducer on the initial sequence.
-- 'Nothing' if the sequence has no elements.
--
-- ===== __Examples__
--
-- >>> reduceList (unsnoc sum) [1,2,3,4]
-- Just (6,4)
--
-- @since 1.0.0
{-# INLINE unsnoc #-}
unsnoc ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Reducer 'Zero a r ->
  Reducer init a (Maybe (r, a))
unsnoc (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = (Nothing, state)
      , finalize = \(r, s) -> case r of
          Nothing -> Nothing
          Just lst -> Just (finalize s, lst)
      , step = \(mr, s) a -> case mr of
          Nothing -> Continue (Just a, s)
          Just lst -> fmap (Just a,) (step s lst)
      }

-- | Apply the passed function with every element of the input and add it to the input stream.
--
-- ===== __Examples__
--
-- Running sums
--
-- >>> reduceList (scan (+) 0 |> intoList) [1..5]
-- [(0,1),(1,2),(3,3),(6,4),(10,5)]
--
-- @since 1.0.0
{-# INLINE scan #-}
scan ::
  forall (init :: Initial) (a :: Type) (r :: Type) (acc :: Type).
  (IsInitial init) =>
  (acc -> a -> acc) ->
  acc ->
  Reducer init (acc, a) r ->
  Reducer init a r
scan scanStep scanInit (Reducer state finalize step) =
  Reducer
    { state = case initial @init of
        SZero -> (scanInit, state)
        SOne -> \a -> fmap (scanInit `seq` scanStep scanInit a,) (state (scanInit, a))
    , finalize = \(_, state) -> finalize state
    , step = \(scanAcc, s) a ->
        case step s (scanAcc, a) of
          Reduced s' -> Reduced (scanAcc, s')
          Continue s' -> Continue (scanAcc `seq` scanStep scanAcc a, s')
    }

-- | Associate index with each element, starting at zero.
--
-- ===== __Examples__
--
-- >>> reduceList (enumerate |> intoList) ["foo","bar"]
-- [(0,"foo"),(1,"bar")]
--
-- @since 1.0.0
{-# INLINE enumerate #-}
enumerate ::
  forall (init :: Initial) (a :: Type) (r :: Type).
  (IsInitial init) =>
  Reducer init (Int, a) r ->
  Reducer init a r
enumerate = scan (\acc _ -> acc + 1) 0

-- | Run two reducers on the same input.
--
-- ===== __Examples__
--
-- >>> reduceList (zip sum product) [1,2,3,4]
-- (10,24)
--
-- @since 1.0.0
{-# INLINE zip #-}
zip ::
  forall (init :: Initial) (a :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  Reducer init a r1 ->
  Reducer init a r2 ->
  Reducer init a (r1, r2)
zip = liftA2 (,)

-- | Run first reducer on first element of the tuple and second reducer on the second.
--
-- ===== __Examples__
--
-- Collect @fst@ into a list and sums the @snd@
--
-- >>> reduceList (zipTuple intoList sum) [(1,2),(3,4)]
-- ([1,3],6)
--
-- @since 1.0.0
{-# INLINE zipTuple #-}
zipTuple ::
  forall (init :: Initial) (a1 :: Type) (a2 :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  Reducer init a1 r1 ->
  Reducer init a2 r2 ->
  Reducer init (a1, a2) (r1, r2)
zipTuple (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = case initial @init of
        SZero -> (ZipFinishedNone, state1, state2)
        SOne -> \(a1, a2) -> zipInitial (state1 a1) (state2 a2)
    , finalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
    , step = \s (a1, a2) -> zipStepBoth step1 step2 s a1 a2
    }

-- | Run first reducer on @Left@ elements and second reducer on @Right@s.
--
-- ===== __Examples__
--
-- Collect @fst@ into a list and sums the @snd@
--
-- >>> reduceList (zipEither intoList sum) [Left 1,Right 2,Left 3,Left 4,Right 5]
-- ([1,3,4],7)
--
-- @since 1.0.0
{-# INLINE zipEither #-}
zipEither ::
  forall (init :: Initial) (a1 :: Type) (a2 :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  Reducer 'Zero a1 r1 ->
  Reducer 'Zero a2 r2 ->
  Reducer init (Either a1 a2) (r1, r2)
zipEither (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (ZipFinishedNone, state1, state2)
      , finalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
      , step = \s a ->
          case a of
            Left a1 -> zipStepLeft step1 s a1
            Right a2 -> zipStepRight step2 s a2
      }

-- | Run first reducer if on elements that satisfy the predicate and second otherwise
--
-- ===== __Examples__
--
-- >>> reduceList (partition even sum intoList) [1,2,3,4,5]
-- (6,[1,3,5])
--
-- @since 1.0.0
{-# INLINE partition #-}
partition ::
  forall (init :: Initial) (a :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  (a -> Bool) ->
  Reducer 'Zero a r1 ->
  Reducer 'Zero a r2 ->
  Reducer init a (r1, r2)
partition f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (ZipFinishedNone, state1, state2)
      , finalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
      , step = \s a -> if f a then zipStepLeft step1 s a else zipStepRight step2 s a
      }

-- | Run first reducer on the longest prefix that satisfies the predicate, then second.
--
-- ===== __Examples__
--
-- >>> reduceList (span even intoList intoList) [2,6,4,1,2,3,4]
-- ([2,6,4],[1,2,3,4])
--
-- @since 1.0.0
{-# INLINE span #-}
span ::
  forall (init :: Initial) (a :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  (a -> Bool) ->
  Reducer 'Zero a r1 ->
  Reducer 'Zero a r2 ->
  Reducer init a (r1, r2)
span f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (True, (ZipFinishedNone, state1, state2))
      , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
      , step = \(onFirst, s) a ->
          if onFirst && f a then
            fmap (True,) (zipStepLeft step1 s a)
          else
            fmap (False,) (zipStepRight step2 s a)
      }

-- | Run first reducer on the longest prefix that does not satisfy the predicate, then second.
--
-- @'break' p = 'span' (not . p)@
--
-- ===== __Examples__
--
-- >>> reduceList (break odd intoList intoList) [2,6,4,1,2,3,4]
-- ([2,6,4],[1,2,3,4])
--
-- @since 1.0.0
{-# INLINE break #-}
break ::
  forall (init :: Initial) (a :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  (a -> Bool) ->
  Reducer 'Zero a r1 ->
  Reducer 'Zero a r2 ->
  Reducer init a (r1, r2)
break f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (True, (ZipFinishedNone, state1, state2))
      , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
      , step = \(onFirst, s) a ->
          if onFirst && not (f a) then
            fmap (True,) (zipStepLeft step1 s a)
          else
            fmap (False,) (zipStepRight step2 s a)
      }

-- | Run first reducer on the first @n@ elements,then second.
--
-- ===== __Examples__
--
-- >>> reduceList (splitAt 3 intoList intoList) [2,6,4,1,2,3,4]
-- ([2,6,4],[1,2,3,4])
--
-- @since 1.0.0
{-# INLINE splitAt #-}
splitAt ::
  forall (init :: Initial) (a :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  Int ->
  Reducer 'Zero a r1 ->
  Reducer 'Zero a r2 ->
  Reducer init a (r1, r2)
splitAt n (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (n, (ZipFinishedNone, state1, state2))
      , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
      , step = \(n, s) a ->
          if n > 0 then
            fmap (n - 1,) (zipStepLeft step1 s a)
          else
            fmap (n,) (zipStepRight step2 s a)
      }

-- | Split sequence into two alternating sequences.
--
-- ===== __Examples__
--
-- >>> reduceList (uninterleave intoList intoList) [1..5]
-- ([1,3,5],[2,4])
--
-- @since 1.0.0
{-# INLINE uninterleave #-}
uninterleave ::
  forall (init :: Initial) (a :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  Reducer 'Zero a r1 ->
  Reducer 'Zero a r2 ->
  Reducer init a (r1, r2)
uninterleave (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (True, (ZipFinishedNone, state1, state2))
      , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
      , step = \(first, s) a ->
          if first then
            fmap (False,) (zipStepLeft step1 s a)
          else
            fmap (True,) (zipStepRight step2 s a)
      }

-- | Split off every @n@th element into the first reducer and rest to the second
--
-- ===== __Examples__
--
-- >>> reduceList (takeNth 3 intoList intoList) [1..10]
-- ([1,5,9],[2,3,4,6,7,8,10])
--
-- @since 1.0.0
{-# INLINE takeNth #-}
takeNth ::
  forall (init :: Initial) (a :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  Int ->
  Reducer 'Zero a r1 ->
  Reducer 'Zero a r2 ->
  Reducer init a (r1, r2)
takeNth n (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (0, (ZipFinishedNone, state1, state2))
      , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
      , step = \(k, s) a ->
          if k <= 0 then
            fmap (n,) (zipStepLeft step1 s a)
          else
            fmap (k - 1,) (zipStepRight step2 s a)
      }

-- | Feed one element to the reducer before the rest of the sequence.
--
-- ===== __Examples__
--
-- >>> reduceList (feed1 @'Zero 1 |> intoList) [2,3]
-- [1,2,3]
--
-- @since 1.0.0
{-# INLINE feed1 #-}
feed1 :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => a -> Reducer init a r -> Reducer 'Zero a r
feed1 a (Reducer state finalize step) =
  case startInitial (initial @init) step state a of
    Continue state' -> Reducer state' finalize step
    Reduced state' -> Reducer state' finalize (\s _ -> Reduced s)

-- ** Grouping

-- | Split the sequence into groups of consecutive equal elements.
--
-- The inner reducer accepts either initialization mode, independently of the downstream
-- reducer. Every group is non-empty, and non-empty input produces at least one group,
-- so the result has the same initialization requirement as the downstream reducer.
--
-- ===== __Examples__
--
-- >>> reduceList (group @'Zero intoList |> intoList) [1,2,2,3,3,1]
-- [[1],[2,2],[3,3],[1]]
--
-- Run-length encoding with a non-empty inner reducer:
--
-- >>> reduceList (group (liftA2 (,) head1 length) |> intoList) "aaabccdddd"
-- [('a',3),('b',1),('c',2),('d',4)]
--
-- @since 1.0.0
{-# INLINE group #-}
group ::
  forall (inner :: Initial) (init :: Initial) (a :: Type) (p :: Type) (r :: Type).
  (IsInitial inner, IsInitial init, Eq a) =>
  Reducer inner a p ->
  Reducer init p r ->
  Reducer init a r
group = groupBy (==)

-- | Split the sequence into groups of consecutive elements for which the passed predicate
-- returns 'True' when applied to the first element of the group and the current element.
--
-- ===== __Examples__
--
-- Get non-decreasing sub-sequences
--
-- >>> reduceList (groupBy @'Zero (\x y -> x <= y) intoList |> intoList) [3,4,5,1,2,6,0]
-- [[3,4,5],[1,2,6],[0]]
--
-- >>> reduceList (groupBy (\a b -> (a `mod` 3) == (b `mod` 3)) maximum1 |> intoList) [1,4,5,9,6]
-- [4,5,9]
--
-- @since 1.0.0
{-# INLINE groupBy #-}
groupBy ::
  forall (inner :: Initial) (init :: Initial) (a :: Type) (p :: Type) (r :: Type).
  (IsInitial inner, IsInitial init) =>
  (a -> a -> Bool) ->
  Reducer inner a p ->
  Reducer init p r ->
  Reducer init a r
groupBy eq = groupByKey id eq

-- | Split the sequence into groups of consecutive elements that map to the same key.
--
-- ===== __Examples__
--
-- >>> reduceList (groupOn @'Zero even intoList |> intoList) [1,2,4,5,7,9,6,8]
-- [[1],[2,4],[5,7,9],[6,8]]
--
-- >>> reduceList (groupOn even head1 |> intoList) [1,2,4,5,7,9,6,8]
-- [1,2,5,6]
--
-- @since 1.0.0
{-# INLINE groupOn #-}
groupOn ::
  forall (inner :: Initial) (init :: Initial) (k :: Type) (a :: Type) (p :: Type) (r :: Type).
  (IsInitial inner, IsInitial init, Eq k) =>
  (a -> k) ->
  Reducer inner a p ->
  Reducer init p r ->
  Reducer init a r
groupOn key = groupByKey key (\k a -> k == key a)

-- | Split the sequence into groups of consecutive elements.
--
-- ===== __Examples__
--
-- Group elements that are at most 2 greater than the first element of the group.
--
-- >>> reduceList (groupByKey @'Zero (+ 2) (\bound x -> x <= bound) intoList |> intoList) [1,2,3,4,5,10,11,12,13]
-- [[1,2,3],[4,5],[10,11,12],[13]]
--
-- Compute the mean of each such group with a non-empty inner reducer:
--
-- >>> reduceList (groupByKey (+ 2) (\bound x -> x <= bound) mean1 intoList) [1,2,3,4,5,10,11,12,13]
-- [2.0,4.5,11.0,13.0]
--
-- @since 1.0.0
{-# INLINE groupByKey #-}
groupByKey ::
  forall (inner :: Initial) (init :: Initial) (k :: Type) (a :: Type) (p :: Type) (r :: Type).
  (IsInitial inner, IsInitial init) =>
  (a -> k) ->
  (k -> a -> Bool) ->
  Reducer inner a p ->
  Reducer init p r ->
  Reducer init a r
groupByKey key sameGroup (Reducer groupState groupFinalize groupStep) =
  groupByKeyWith key sameGroup (startInitial (initial @inner) groupStep groupState) groupFinalize groupStep

-- * Building Blocks

-- | Building block for @group*@ functions.
--
-- A non-empty downstream reducer is initialized only when the first group is complete.
--
-- @since 1.0.0
{-# INLINE groupByKeyWith #-}
groupByKeyWith ::
  forall (init :: Initial) (k :: Type) (a :: Type) (g :: Type) (p :: Type) (r :: Type).
  (IsInitial init) =>
  -- | Function that turns first element of a group into the group key.
  (a -> k) ->
  -- | Function that checks if an element belongs to a given group by its key.
  (k -> a -> Bool) ->
  -- | Function that creates the reducer for the group based on its first value.
  (a -> Reduced g) ->
  -- | Finalizer function for the group.
  (g -> p) ->
  -- | Step function for the group.
  (g -> a -> Reduced g) ->
  Reducer init p r ->
  Reducer init a r
groupByKeyWith key sameGroup groupSeed groupFinalize groupStep (Reducer state finalize step) =
  let startGroup a downstream = Right ((key a, groupSeed a), downstream)
      emit downstream g =
        case downstream of
          Left seed -> seed (groupFinalize (unReduced g))
          Right s -> step s (groupFinalize (unReduced g))
   in Reducer
        { state = case initial @init of
            SZero -> Left state
            SOne -> \a -> Continue (startGroup a (Left state))
        , finalize = \current ->
            case current of
              Left s -> finalize s
              Right ((_, g), downstream) -> finalize (unReduced (emit downstream g))
        , step = \current a ->
            case current of
              Left s -> Continue (startGroup a (Right s))
              Right ((k, g), downstream) ->
                if sameGroup k a then case g of
                  Reduced _ -> Continue current
                  Continue gs -> Continue (Right ((k, groupStep gs a), downstream))
                else case emit downstream g of
                  Reduced s' -> Reduced (Left s')
                  Continue s' -> Continue (startGroup a (Right s'))
        }

-- | Construct a reducer that consumes whole input.
--
-- ===== __Examples__
--
-- @sum = linearReducer 0 (+)@
--
-- >>> reduceList (linearReducer 0 (+)) [1,2,3]
-- 6
--
-- @since 1.0.0
{-# INLINE linearReducer #-}
linearReducer :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => r -> (r -> a -> r) -> Reducer init a r
linearReducer acc f =
  initializeReducer $
    Reducer
      { state = acc
      , finalize = id
      , step = \r !a -> let !r' = f r a in Continue r'
      }

-- ** Zipping

-- | Internal state of @zip*@ functions.
--
-- It is used to track if some reducer already short circuted to not call step function on it anymore
-- but the overall combinator must still return @Continue@ to give the opportunity to both reducers
-- to finish.
--
-- @since 1.0.0
type ZipFinished :: Type
data ZipFinished
  = -- | No reducer finished.
    ZipFinishedNone
  | -- | First (left) reducer finished but second (right) did not.
    ZipFinished1
  | -- | Second (right) reducer finished but first (left) did not.
    ZipFinished2
  | -- | Both reducers finished.
    ZipFinishedBoth

-- | Combine initialization results, keeping finished branches fixed.
{-# INLINE zipInitial #-}
zipInitial :: Reduced s1 -> Reduced s2 -> Reduced (ZipFinished, s1, s2)
zipInitial (Continue s1) (Continue s2) = Continue (ZipFinishedNone, s1, s2)
zipInitial (Reduced s1) (Continue s2) = Continue (ZipFinished1, s1, s2)
zipInitial (Continue s1) (Reduced s2) = Continue (ZipFinished2, s1, s2)
zipInitial (Reduced s1) (Reduced s2) = Reduced (ZipFinishedBoth, s1, s2)

-- | @since 1.0.0
{-# INLINE zipStepLeft #-}
zipStepLeft ::
  forall (a1 :: Type) (s1 :: Type) (s2 :: Type).
  (s1 -> a1 -> Reduced s1) ->
  (ZipFinished, s1, s2) ->
  a1 ->
  Reduced (ZipFinished, s1, s2)
zipStepLeft step1 (finished, s1, s2) a1 =
  case finished of
    ZipFinishedNone -> case step1 s1 a1 of
      Continue s1' -> Continue (ZipFinishedNone, s1', s2)
      Reduced s1' -> Continue (ZipFinished1, s1', s2)
    ZipFinished1 -> Continue (ZipFinished1, s1, s2)
    ZipFinished2 -> case step1 s1 a1 of
      Continue s1' -> Continue (ZipFinished2, s1', s2)
      Reduced s1' -> Reduced (ZipFinishedBoth, s1', s2)
    ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)

-- | @since 1.0.0
{-# INLINE zipStepRight #-}
zipStepRight ::
  forall (a2 :: Type) (s1 :: Type) (s2 :: Type).
  (s2 -> a2 -> Reduced s2) ->
  (ZipFinished, s1, s2) ->
  a2 ->
  Reduced (ZipFinished, s1, s2)
zipStepRight step2 (finished, s1, s2) a2 =
  case finished of
    ZipFinishedNone -> case step2 s2 a2 of
      Continue s2' -> Continue (ZipFinishedNone, s1, s2')
      Reduced s2' -> Continue (ZipFinished2, s1, s2')
    ZipFinished1 -> case step2 s2 a2 of
      Continue s2' -> Continue (ZipFinished1, s1, s2')
      Reduced s2' -> Reduced (ZipFinishedBoth, s1, s2')
    ZipFinished2 -> Continue (ZipFinished2, s1, s2)
    ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)

-- | @since 1.0.0
{-# INLINE zipStepBoth #-}
zipStepBoth ::
  forall (a1 :: Type) (a2 :: Type) (s1 :: Type) (s2 :: Type).
  (s1 -> a1 -> Reduced s1) ->
  (s2 -> a2 -> Reduced s2) ->
  (ZipFinished, s1, s2) ->
  a1 ->
  a2 ->
  Reduced (ZipFinished, s1, s2)
zipStepBoth step1 step2 (finished, s1, s2) a1 a2 =
  case finished of
    ZipFinishedNone -> case (step1 s1 a1, step2 s2 a2) of
      (Continue s1', Continue s2') -> Continue (ZipFinishedNone, s1', s2')
      (Reduced s1', Continue s2') -> Continue (ZipFinished1, s1', s2')
      (Continue s1', Reduced s2') -> Continue (ZipFinished2, s1', s2')
      (Reduced s1', Reduced s2') -> Reduced (ZipFinishedBoth, s1', s2')
    ZipFinished1 -> case step2 s2 a2 of
      Continue s2' -> Continue (ZipFinished1, s1, s2')
      Reduced s2' -> Reduced (ZipFinishedBoth, s1, s2')
    ZipFinished2 -> case step1 s1 a1 of
      Continue s1' -> Continue (ZipFinished2, s1', s2)
      Reduced s1' -> Reduced (ZipFinishedBoth, s1', s2)
    ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)

{-# INLINE startInitial #-}
startInitial :: SInitial init -> (s -> a -> Reduced s) -> InitialState init a s -> a -> Reduced s
startInitial SZero step state a = step state a
startInitial SOne _ state a = state a

-- | Use an empty-safe reducer with either initialization mode.
--
-- @since 1.0.0
{-# INLINE initializeReducer #-}
initializeReducer :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => Reducer 'Zero a r -> Reducer init a r
initializeReducer reducer@(Reducer state finalize step) =
  case initial @init of
    SZero -> reducer
    SOne -> Reducer (step state) finalize step

-- ** Doctest Imports (ignore)

-- $setup
-- >>> import Prelude (String, even, mod, odd)
