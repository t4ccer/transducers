{-# LANGUAGE NoFieldSelectors #-}

{- |
'Reducer's are generalized data processing steps that can be reused across multiple collections.
They process elements one by one, can carry state, and have ability to short-circuit.

Transducers are functions that take one 'Reducer' and return another.

This module is intended to be imported qualified or used without implicit @Prelude@.
Function names collide with @base@ functions that operate on lists and other collections though
output types are sometimes modified for our 'Reducer's to remain total.
-}
module Data.Transducer (
  Reducer (..),

  -- * Runners
  reduceList,
  reduceNonEmpty,
  reduceSingleton,
  reduceIterate,
  reduceRepeat,
  reduceReplicate,

  -- * Reducers
  and,
  or,
  all,
  any,
  sum,
  product,
  maximum,
  minimum,
  length,
  compareLength,
  null,
  head,
  last,
  find,
  elemBy,
  elem,
  discard,
  intoList,
  intoNonEmpty,

  -- * Transducers
  (|>),
  filter,
  take,
  takeWhile,
  drop,
  dropWhile,
  map,
  mapMaybe,
  catMaybes,
  intersperse,
  concatList,
  concatNonEmpty,
  nub,
  nubBy,
  uncons,
  unsnoc,
  enumerate,
  zipReducers,
  zipReducersSplit,
  zipReducersFork,

  -- * Building Blocks
  Reduced (..),
  mkLinearReducer,
  mkLinearReducer',
) where

import Control.Applicative (Applicative (liftA2, pure, (*>), (<*), (<*>)))
import Control.Monad (Functor (fmap))
import Data.Bool (Bool (False, True), (||))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Function (const, id, ($))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List (reverse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord (compare, max, min, (<=)), Ordering (EQ, GT, LT))
import GHC.Base (seq)
import GHC.Num (Num ((*), (+), (-)))

import Data.Transducer.Internal (
  ZipFinished (
    ZipFinished1,
    ZipFinished2,
    ZipFinishedBoth,
    ZipFinishedNone
  ),
 )

{- | Result of the reduction process.

@since 1.0.0
-}
type role Reduced representational

type Reduced :: Type -> Type
data Reduced a
  = -- | The reduction finished and the reducer step should not be called anymore.
    Reduced a
  | -- | The reduction is still in progress and the reducer step should be called if there are more values.
    Continue a

instance Functor Reduced where
  {-# INLINE fmap #-}
  fmap f = \case
    Reduced a -> Reduced (f a)
    Continue a -> Continue (f a)

{- |
@
data Reducer a r
             ^ ^
             | |
             | +-- Accumulator that will be returned after reduction
             |
             +-- Elements that the reducer is processing
@

@since 1.0.0
-}
type role Reducer representational representational

type Reducer :: Type -> Type -> Type
data Reducer a r = forall s. Reducer
  { reducerInitState :: s
  , reducerFinalize :: s -> r
  , reducerStep :: s -> a -> Reduced s
  }

instance Functor (Reducer a) where
  {-# INLINE fmap #-}
  fmap f (Reducer state finalize step) = (Reducer state (\s -> f (finalize s)) step)

instance Applicative (Reducer a) where
  {-# INLINE pure #-}
  pure :: forall (r :: Type). r -> Reducer a r
  pure r =
    Reducer
      { reducerInitState = ()
      , reducerFinalize = const r
      , reducerStep = \_ _ -> Reduced ()
      }

  {-# INLINE liftA2 #-}
  liftA2 :: (x -> y -> r) -> Reducer a x -> Reducer a y -> Reducer a r
  liftA2 f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
    Reducer
      { reducerInitState = (ZipFinishedNone, state1, state2)
      , reducerFinalize = \(_, s1, s2) -> f (finalize1 s1) (finalize2 s2)
      , reducerStep = \(finished, s1, s2) a ->
          case finished of
            ZipFinishedNone -> case (step1 s1 a, step2 s2 a) of
              (Continue s1', Continue s2') -> Continue (ZipFinishedNone, s1', s2')
              (Reduced s1', Continue s2') -> Continue (ZipFinished1, s1', s2')
              (Continue s1', Reduced s2') -> Continue (ZipFinished2, s1', s2')
              (Reduced s1', Reduced s2') -> Reduced (ZipFinishedBoth, s1', s2')
            ZipFinished1 -> case step2 s2 a of
              Continue s2' -> Continue (ZipFinished1, s1, s2')
              Reduced s2' -> Reduced (ZipFinishedBoth, s1, s2')
            ZipFinished2 -> case step1 s1 a of
              Continue s1' -> Continue (ZipFinished2, s1', s2)
              Reduced s1' -> Reduced (ZipFinishedBoth, s1', s2)
            ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)
      }

  {-# INLINE (<*>) #-}
  (<*>) :: Reducer a (x -> r) -> Reducer a x -> Reducer a r
  (<*>) = liftA2 id

  {-# INLINE (*>) #-}
  (*>) :: Reducer a x -> Reducer a r -> Reducer a r
  (*>) = liftA2 (\_ r -> r)

  {-# INLINE (<*) #-}
  (<*) :: Reducer a r -> Reducer a x -> Reducer a r
  (<*) = liftA2 const

{- | Run a reducer on all list elements.

===== Examples

>>> reduceList sum [1, 2, 3]
6

@since 1.0.0
-}
{-# INLINE reduceList #-}
reduceList :: forall (a :: Type) (r :: Type). Reducer a r -> [a] -> r
reduceList (Reducer state finalize step) input = finalize (go state input)
  where
    go s [] = s
    go s (a : as) =
      case step s a of
        Reduced s' -> s'
        Continue s' -> go s' as

{- | Run a reducer on all elements of non-empty list.

===== Examples

>>> reduceNonEmpty sum (1 :| [2, 3])
6

@since 1.0.0
-}
{-# INLINE reduceNonEmpty #-}
reduceNonEmpty :: forall (a :: Type) (r :: Type). Reducer a r -> NonEmpty a -> r
reduceNonEmpty reducer (a :| as) = reduceList reducer (a : as)

{- | Run a reducer on a single element.

===== Examples

>>> reduceSingleton head 1
Just 1

>>> reduceSingleton (drop 1 |> head) 1
Nothing

@since 1.0.0
-}
{-# INLINE reduceSingleton #-}
reduceSingleton :: forall (a :: Type) (r :: Type). Reducer a r -> a -> r
reduceSingleton reducer a = reduceList reducer [a]

{- | Run a reducer on an infinite sequence of repeated function applications.

===== Examples

>>> reduceIterate (take 10 |> intoList) (+3) 42
[42,45,48,51,54,57,60,63,66,69]

@since 1.0.0
-}
{-# INLINE reduceIterate #-}
reduceIterate :: forall (a :: Type) (r :: Type). Reducer a r -> (a -> a) -> a -> r
reduceIterate (Reducer state finalize step) f a = finalize (go state a)
  where
    go s a = case step s a of
      Reduced s' -> s'
      Continue s' -> go s' (f a)

{- | Run a reducer on an infinite sequence of the same value.

===== Examples

>>> reduceRepeat (take 10 |> intoList) 42
[42,42,42,42,42,42,42,42,42,42]

@since 1.0.0
-}
{-# INLINE reduceRepeat #-}
reduceRepeat :: forall (a :: Type) (r :: Type). Reducer a r -> a -> r
reduceRepeat (Reducer state finalize step) a = finalize (go state)
  where
    go s = case step s a of
      Reduced r' -> r'
      Continue r' -> go r'

{- | Run a reducer on a finite sequence of the same value.

===== Examples

>>> reduceReplicate intoList 10 42
[42,42,42,42,42,42,42,42,42,42]

@since 1.0.0
-}
{-# INLINE reduceReplicate #-}
reduceReplicate :: forall (a :: Type) (r :: Type). Reducer a r -> Int -> a -> r
reduceReplicate (Reducer state finalize step) n a = finalize (go state n)
  where
    go s n =
      if n <= 0
        then s
        else case step s a of
          Reduced s' -> s'
          Continue s' -> go s' (n - 1)

{- | Construct a stateless reducer that consumes whole input.

===== Examples

@sum = mkLinearReducer 0 (+)@

>>> reduceList (mkLinearReducer 0 (+)) [1, 2, 3]
6

@since 1.0.0
-}
{-# INLINE mkLinearReducer #-}
mkLinearReducer :: forall (a :: Type) (r :: Type). r -> (r -> a -> r) -> Reducer a r
mkLinearReducer acc f =
  Reducer
    { reducerInitState = acc
    , reducerFinalize = id
    , reducerStep = \r a -> Continue (f r a)
    }

{- | Like 'mkLinearReducer' but with strict accumulator

@since 1.0.0
-}
{-# INLINE mkLinearReducer' #-}
mkLinearReducer' :: forall (a :: Type) (r :: Type). r -> (r -> a -> r) -> Reducer a r
mkLinearReducer' acc f =
  Reducer
    { reducerInitState = acc
    , reducerFinalize = id
    , reducerStep = \r !a -> let !r' = f r a in Continue r'
    }

{- | Get the sum of the elements in the sequence.

===== Examples

>>> reduceList sum [1..10]
55

@since 1.0.0
-}
{-# INLINE sum #-}
sum :: forall (r :: Type). Num r => Reducer r r
sum = mkLinearReducer' 0 (+)

{- | Get the product of the elements in the sequence.

===== Examples

>>> reduceList product [1..5]
120

@since 1.0.0
-}
{-# INLINE product #-}
product :: forall (r :: Type). Num r => Reducer r r
product = mkLinearReducer' 1 (*)

{- | Get the largest element of the sequence.

===== Examples

>>> reduceList maximum [1,3,2]
Just 3

>>> reduceList maximum []
Nothing

@since 1.0.0
-}
{-# INLINE maximum #-}
maximum :: forall (a :: Type). Ord a => Reducer a (Maybe a)
maximum = mkLinearReducer' Nothing (\r a -> max (Just a) r)

{- | Get the smallest element of the sequence.

===== Examples

>>> reduceList minimum [1,3,2]
Just 1

>>> reduceList minimum []
Nothing

@since 1.0.0
-}
{-# INLINE minimum #-}
minimum :: forall (a :: Type). Ord a => Reducer a (Maybe a)
minimum = mkLinearReducer' Nothing $ \r a ->
  case r of
    Nothing -> Just a
    Just r' -> let !m = min r' a in Just m

{- | Get the length of the sequence.

===== Examples

>>> reduceList length [1,3,2]
3

@since 1.0.0
-}
{-# INLINE length #-}
length :: forall (a :: Type). Reducer a Int
length = mkLinearReducer' 0 (\acc _ -> acc + 1)

{- | Compare length of the sequence against a constant. It short-circuits upon reaching @GT@
thus terminates even on infinite sequences.

===== Examples

>>> reduceList (compareLength 0) []
EQ

>>> reduceList (compareLength 1) []
LT

>>> reduceList (compareLength 1) ['a']
EQ

>>> reduceList (compareLength 1) ['a', 'b']
GT

>>> reduceList (compareLength 100) [0..]
GT

@since 1.0.0
-}
{-# INLINE compareLength #-}
compareLength :: forall (a :: Type). Int -> Reducer a Ordering
compareLength n =
  Reducer
    { reducerInitState = (compare 0 n, 0)
    , reducerFinalize = \(r, _) -> r
    , reducerStep = \(_, s) _ ->
        case compare (s + 1) n of
          LT -> Continue (LT, s + 1)
          EQ -> Continue (EQ, s + 1)
          GT -> Reduced (GT, s + 1)
    }

{- | Get the disjunction of @Bool@s.

===== Examples

>>> reduceList or [True, False, True]
True

>>> reduceList or [False, False]
False

'or' short-circuits on first @True@ value

>>> reduceRepeat or True
True

@since 1.0.0
-}
{-# INLINE or #-}
or :: Reducer Bool Bool
or = any id

{- | Get the conjunction of @Bool@s.

===== Examples

>>> reduceList and [True, False, True]
False

>>> reduceList and [True, True]
True

'and' short-circuits on first @False@ value

>>> reduceRepeat and False
False

@since 1.0.0
-}
{-# INLINE and #-}
and :: Reducer Bool Bool
and = all id

{- | Get the conjunction of the results of passed predicates.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (all even) [4,2,6]
True

>>> reduceList (all even) [1,2,3]
False

'and' short-circuits on first @False@ value

>>> reduceList (all even) [1..]
False

@since 1.0.0
-}
{-# INLINE all #-}
all :: forall (a :: Type). (a -> Bool) -> Reducer a Bool
all pred =
  Reducer
    { reducerInitState = True
    , reducerFinalize = id
    , reducerStep = \_ a ->
        if pred a
          then Continue True
          else Reduced False
    }

{- | Get the conjunction of the results of passed predicates.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (any even) [1,2,3]
True

>>> reduceList (any even) [1,3]
False

'any' short-circuits on first @True@ value

>>> reduceRepeat (any even) 2
True

@since 1.0.0
-}
{-# INLINE any #-}
any :: forall (a :: Type). (a -> Bool) -> Reducer a Bool
any pred =
  Reducer
    { reducerInitState = False
    , reducerFinalize = id
    , reducerStep = \_ a ->
        if pred a
          then Reduced True
          else Continue False
    }

{- | Check if sequence is empty.

===== Examples

>>> reduceList null []
True

>>> reduceList null [1..]
False

@since 1.0.0
-}
{-# INLINE null #-}
null :: forall (a :: Type). Reducer a Bool
null =
  Reducer
    { reducerInitState = True
    , reducerFinalize = id
    , reducerStep = \_ _ -> Reduced False
    }

{- | Extract the first element if exists.

===== Examples

>>> reduceList head [1, 2, 3]
Just 1

>>> reduceList head [1 ..]
Just 1

>>> reduceList head []
Nothing

@since 1.0.0
-}
{-# INLINE head #-}
head :: forall (a :: Type). Reducer a (Maybe a)
head =
  Reducer
    { reducerInitState = Nothing
    , reducerFinalize = id
    , reducerStep = \_ a -> Reduced (Just a)
    }

{- | Extract the last element if exists.

===== Examples

>>> reduceList last [1, 2, 3]
Just 3

>>> reduceList last []
Nothing

@since 1.0.0
-}
{-# INLINE last #-}
last :: forall (a :: Type). Reducer a (Maybe a)
last = mkLinearReducer' Nothing (const Just)

{- | Get the first element for which the passed predicate returns 'True', if exists.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (find even) [1, 2, 3]
Just 2

>>> reduceList (find even) [1, 3, 5]
Nothing

@since 1.0.0
-}
{-# INLINE find #-}
find :: forall (a :: Type). (a -> Bool) -> Reducer a (Maybe a)
find pred =
  Reducer
    { reducerInitState = Nothing
    , reducerFinalize = id
    , reducerStep = \_ a ->
        if pred a
          then Reduced (Just a)
          else Continue Nothing
    }

{- | Check if for any elements the passed predicate returns 'True'.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (elemBy even) [1, 2, 3]
True

>>> reduceList (elemBy even) [1, 3, 5]
False

@since 1.0.0
-}
{-# INLINE elemBy #-}
elemBy :: forall (a :: Type). (a -> Bool) -> Reducer a Bool
elemBy pred =
  Reducer
    { reducerInitState = False
    , reducerFinalize = id
    , reducerStep = \_ a ->
        if pred a
          then Reduced True
          else Continue False
    }

{- | Check if any elements is equal (using '==') to the one passed.

===== Examples

>>> reduceList (elem 2) [1, 2, 3]
True

>>> reduceList (elem 2) [1, 3, 5]
False

@since 1.0.0
-}
{-# INLINE elem #-}
elem :: forall (a :: Type). Eq a => a -> Reducer a Bool
elem a = elemBy (a ==)

{- | Discard the rest of the elements. It works on infinite sequences

===== Examples

>>> reduceList discard [0..]
()

@since 1.0.0
-}
{-# INLINE discard #-}
discard :: forall (a :: Type). Reducer a ()
discard =
  Reducer
    { reducerInitState = ()
    , reducerFinalize = const ()
    , reducerStep = \_ _ -> Reduced ()
    }

{- | Collect all elements into a list.

===== Examples

>>> reduceReplicate intoList 10 42
[42,42,42,42,42,42,42,42,42,42]

@since 1.0.0
-}
{-# INLINE intoList #-}
intoList :: forall (a :: Type). Reducer a [a]
intoList =
  Reducer
    { reducerInitState = []
    , reducerFinalize = reverse
    , reducerStep = \as a -> Continue (a : as)
    }

{- | Collect all elements into a non-empty list or @Nothing@ if it does not contain any elements.

===== Examples

>>> reduceList intoNonEmpty [1,2,3]
Just (1 :| [2,3])

>>> reduceList intoNonEmpty []
Nothing

@since 1.0.0
-}
{-# INLINE intoNonEmpty #-}
intoNonEmpty :: forall (a :: Type). Reducer a (Maybe (NonEmpty a))
intoNonEmpty =
  Reducer
    { reducerInitState = Nothing
    , reducerFinalize = fmap NonEmpty.reverse
    , reducerStep = \as a ->
        case as of
          Nothing -> Continue (Just (a :| []))
          Just (b :| bs) -> Continue (Just (a :| b : bs))
    }

{- | Run two reducers on the same input.

===== Examples

>>> reduceList (zipReducers sum product) [1,2,3,4]
(10,24)

@since 1.0.0
-}
{-# INLINE zipReducers #-}
zipReducers ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
zipReducers = liftA2 (,)

{- | Run first reducer on first element of the tuple and second reducer on the second.

===== Examples

Collect @fst@ into a list and sums the @snd@

>>> reduceList (zipReducersSplit intoList sum) [(1, 2), (3, 4)]
([1,3],6)

@since 1.0.0
-}
{-# INLINE zipReducersSplit #-}
zipReducersSplit ::
  forall (a1 :: Type) (a2 :: Type) (r1 :: Type) (r2 :: Type).
  Reducer a1 r1 ->
  Reducer a2 r2 ->
  Reducer (a1, a2) (r1, r2)
zipReducersSplit (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { reducerInitState = (ZipFinishedNone, state1, state2)
    , reducerFinalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
    , reducerStep = \(finished, s1, s2) (a1, a2) ->
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
    }

{- | Run first reducer on @Left@ elements and second reducer on @Right@s.

===== Examples

Collect @fst@ into a list and sums the @snd@

>>> reduceList (zipReducersFork intoList sum) [Left 1, Right 2, Left 3, Left 4, Right 5]
([1,3,4],7)

@since 1.0.0
-}
{-# INLINE zipReducersFork #-}
zipReducersFork ::
  forall (a1 :: Type) (a2 :: Type) (r1 :: Type) (r2 :: Type).
  Reducer a1 r1 ->
  Reducer a2 r2 ->
  Reducer (Either a1 a2) (r1, r2)
zipReducersFork (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { reducerInitState = (ZipFinishedNone, state1, state2)
    , reducerFinalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
    , reducerStep = \(finished, s1, s2) a ->
        case a of
          Left a1 ->
            case finished of
              ZipFinishedNone -> case step1 s1 a1 of
                Continue s1' -> Continue (ZipFinishedNone, s1', s2)
                Reduced s1' -> Continue (ZipFinished1, s1', s2)
              ZipFinished1 -> Continue (ZipFinished1, s1, s2)
              ZipFinished2 -> case step1 s1 a1 of
                Continue s1' -> Continue (ZipFinished2, s1', s2)
                Reduced s1' -> Continue (ZipFinishedBoth, s1', s2)
              ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)
          Right a2 ->
            case finished of
              ZipFinishedNone -> case step2 s2 a2 of
                Continue s2' -> Continue (ZipFinishedNone, s1, s2')
                Reduced s2' -> Continue (ZipFinished2, s1, s2')
              ZipFinished1 -> case step2 s2 a2 of
                Continue s2' -> Continue (ZipFinished1, s1, s2')
                Reduced s2' -> Continue (ZipFinishedBoth, s1, s2')
              ZipFinished2 -> Continue (ZipFinished2, s1, s2)
              ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)
    }

-- * Transducers

{- | Transducer composition operator.

Transducers are executed left to right which may get quite counter-intuitive to read comparing
to @base@ list operators so this operator can be used to clearly indicate the flow of elements.

Note that this is just like '$' but specialized to 'Reducer's.

===== Examples

>>> reduceList (drop 2 |> take 3 |> intoList) [1..]
[3,4,5]

Note that second '|>' is technically redundant but makes code more readable.

>>> reduceList (drop 2 |> take 3 intoList) [1..]
[3,4,5]

Code that uses '|>' is equivalent to

>>> reduceList (drop 2 (take 3 intoList)) [1..]
[3,4,5]

@since 1.0.0
-}
{-# INLINE (|>) #-}
(|>) ::
  forall (a :: Type) (b :: Type) (r :: Type).
  (Reducer a r -> Reducer b r) ->
  Reducer a r ->
  Reducer b r
(|>) transducer = transducer

infixr 5 |>

{- | Apply passed function to every element in the sequence

===== Examples

>>> import GHC.Real (even)
>>> reduceList (map even |> intoList) [2,6,4,5,8]
[True,True,True,False,True]

@since 1.0.0
-}
{-# INLINE map #-}
map ::
  forall (a :: Type) (b :: Type) (r :: Type).
  (b -> a) ->
  Reducer a r ->
  Reducer b r
map f (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = \s b -> step s (f b)
    }

{- | Take first @n@ elements from the beginning of the sequence.

===== Examples

>>> reduceList (take 3 |> intoList) [1..]
[1,2,3]

@since 1.0.0
-}
{-# INLINE take #-}
take ::
  forall (a :: Type) (r :: Type).
  Int ->
  Reducer a r ->
  Reducer a r
take n (Reducer state finalize step) =
  Reducer
    { reducerInitState = (n, state)
    , reducerFinalize = \(_, state) -> finalize state
    , reducerStep = \(currN, s) a ->
        if currN <= 0
          then Reduced (currN, s)
          else
            let currN' = currN - 1
             in case step s a of
                  Reduced s' -> Reduced (currN', s')
                  Continue s' ->
                    if currN' <= 0
                      then Reduced (currN', s')
                      else Continue (currN', s')
    }

{- | Keep taking elements from the sequence as long as the passed predicate returns 'True'.
Note that it short-circuits upon first value that fails the predicate even if some further value
would pass it.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (takeWhile even |> intoList) [2,6,4,5,8]
[2,6,4]

@since 1.0.0
-}
{-# INLINE takeWhile #-}
takeWhile ::
  forall (a :: Type) (r :: Type).
  (a -> Bool) ->
  Reducer a r ->
  Reducer a r
takeWhile pred (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = \s a ->
        if pred a
          then step s a
          else Reduced s
    }

{- | Drop (skip/remove) first @n@ elements from the beginning of the sequence.

===== Examples

>>> reduceList (drop 3 |> intoList) [1..5]
[4,5]

@since 1.0.0
-}
{-# INLINE drop #-}
drop ::
  forall (a :: Type) (r :: Type).
  Int ->
  Reducer a r ->
  Reducer a r
drop n (Reducer state finalize step) =
  Reducer
    { reducerInitState = (n, state)
    , reducerFinalize = \(_, state) -> finalize state
    , reducerStep = \(currN, s) a ->
        if currN <= 0
          then fmap (currN,) (step s a)
          else Continue (currN - 1, s)
    }

{- | Keep dropping elements from the sequence as long as the passed predicate returns 'True'.
Note that upon first value that fails the predicate the whole remaining sequence will be forwarded
even if some further value would pass it.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (dropWhile even |> intoList) [2,6,4,5,8]
[5,8]

@since 1.0.0
-}
{-# INLINE dropWhile #-}
dropWhile ::
  forall (a :: Type) (r :: Type).
  (a -> Bool) ->
  Reducer a r ->
  Reducer a r
dropWhile pred (Reducer state finalize step) =
  Reducer
    { reducerInitState = (False, state)
    , reducerFinalize = \(_, state) -> finalize state
    , reducerStep = \(finishedDropping, s) a ->
        if finishedDropping
          then fmap (True,) (step s a)
          else
            if pred a
              then Continue (False, s)
              else fmap (True,) (step s a)
    }

{- | Keep only elements for which the passed predicate returns 'True'.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (filter even |> intoList) [1..10]
[2,4,6,8,10]

@since 1.0.0
-}
{-# INLINE filter #-}
filter ::
  forall (a :: Type) (r :: Type).
  (a -> Bool) ->
  Reducer a r ->
  Reducer a r
filter pred (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = \s a ->
        if pred a
          then step s a
          else Continue s
    }

{- | Keep only elements that are mapped to 'Just'.

===== Examples

>>> import Text.Read (readMaybe)
>>> reduceList (mapMaybe (readMaybe @Int) |> intoList) ["1", "foo", "2", "bar"]
[1,2]

@since 1.0.0
-}
{-# INLINE mapMaybe #-}
mapMaybe ::
  forall (a :: Type) (b :: Type) (r :: Type).
  (b -> Maybe a) ->
  Reducer a r ->
  Reducer b r
mapMaybe f (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = \s a ->
        case f a of
          Nothing -> Continue s
          Just a' -> step s a'
    }

{- | Keep only 'Just' elements.

===== Examples

>>> reduceList (catMaybes |> intoList) [Just 1, Nothing, Just 2, Just 3, Nothing]
[1,2,3]

@since 1.0.0
-}
{-# INLINE catMaybes #-}
catMaybes ::
  forall (a :: Type) (r :: Type).
  Reducer a r ->
  Reducer (Maybe a) r
catMaybes (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = \s -> \case
        Nothing -> Continue s
        Just a -> step s a
    }

{- | Insert an element in between every element in the sequence.

===== Examples

>>> reduceList (intersperse 0 |> intoList) [1,2,3]
[1,0,2,0,3]

@since 1.0.0
-}
{-# INLINE intersperse #-}
intersperse ::
  forall (a :: Type) (r :: Type).
  a ->
  Reducer a r ->
  Reducer a r
intersperse middle (Reducer state finalize step) =
  Reducer
    { reducerInitState = (False, state)
    , reducerFinalize = \(_, state) -> finalize state
    , reducerStep = \(acc, s) a ->
        if acc
          then case step s middle of
            Reduced s2 -> Reduced (True, s2)
            Continue s2 -> fmap (True,) (step s2 a)
          else fmap (True,) (step s a)
    }

{- | Flatten a sequence of @[a]@ into a sequence of @a@.

===== Examples

>>> reduceList (concatList |> intoList) [[1,2],[3,4,5]]
[1,2,3,4,5]

@since 1.0.0
-}
{-# INLINE concatList #-}
concatList ::
  forall (a :: Type) (r :: Type).
  Reducer a r ->
  Reducer [a] r
concatList (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = step'
    }
  where
    step' s = \case
      [] -> Continue s
      (a : as) -> case step s a of
        Reduced s' -> Reduced s'
        Continue s' -> step' s' as

{- | Flatten a sequence of @NonEmpty a@ into a sequence of @a@.

===== Examples

>>> reduceList (concatNonEmpty |> intoList) [1:|[2], 3:|[4,5]]
[1,2,3,4,5]

@since 1.0.0
-}
{-# INLINE concatNonEmpty #-}
concatNonEmpty ::
  forall (a :: Type) (r :: Type).
  Reducer a r ->
  Reducer (NonEmpty a) r
concatNonEmpty reducer = map NonEmpty.toList |> concatList reducer

{- | Remove duplicate elements and keep only the first occurrence that are equal accordingly to '=='.

Note that it has the same performance problems as @Data.List.nub@.

===== Examples

>>> reduceList (nub |> intoList) [1,2,1,3,3,2,2]
[1,2,3]

@since 1.0.0
-}
{-# INLINE nub #-}
nub ::
  forall (a :: Type) (r :: Type).
  Eq a =>
  Reducer a r ->
  Reducer a r
nub = nubBy (==)

{- | Remove duplicate elements and keep only the first occurrence that are equal accordingly to passed predicate.

Note that it has the same performance problems as 'Data.List.nubBy'.

===== Examples

Remove elements that are equal to some previous element modulo @5@.

>>> import GHC.Real (mod)
>>> reduceList (nubBy (\lhs rhs -> lhs `mod` 5 == rhs `mod` 5) |> intoList) [5,10,1,6,7,12]
[5,1,7]

@since 1.0.0
-}
{-# INLINE nubBy #-}
nubBy ::
  forall (a :: Type) (r :: Type).
  (a -> a -> Bool) ->
  Reducer a r ->
  Reducer a r
nubBy eq (Reducer state finalize step) =
  Reducer
    { reducerInitState = ([], state)
    , reducerFinalize = \(_, state) -> finalize state
    , reducerStep = \(seen, s) a ->
        if list_elemBy a seen
          then Continue (seen, s)
          else fmap (a : seen,) (step s a)
    }
  where
    list_elemBy :: a -> [a] -> Bool
    list_elemBy _ [] = False
    list_elemBy y (x : xs) = x `eq` y || list_elemBy y xs

{- | Preserve the first element and run the passed reducer on the remaining sequence.
'Nothing' if the sequence has no elements.

===== Examples

>>> reduceList (uncons sum) [1,2,3]
Just (1,5)

@since 1.0.0
-}
{-# INLINE uncons #-}
uncons ::
  forall (a :: Type) (r :: Type).
  Reducer a r ->
  Reducer a (Maybe (a, r))
uncons (Reducer state finalize step) =
  Reducer
    { reducerInitState = (Nothing, state)
    , reducerFinalize = \(r, s) -> case r of
        Nothing -> Nothing
        Just fst -> Just (fst, finalize s)
    , reducerStep = \(mr, s) a -> case mr of
        Nothing -> Continue (Just a, s)
        Just fst -> fmap (Just fst,) (step s a)
    }

{- | Preserve the last element and run the passed reducer on the initial sequence.
'Nothing' if the sequence has no elements.

===== Examples

>>> reduceList (unsnoc sum) [1,2,3,4]
Just (6,4)

@since 1.0.0
-}
{-# INLINE unsnoc #-}
unsnoc ::
  forall (a :: Type) (r :: Type).
  Reducer a r ->
  Reducer a (Maybe (r, a))
unsnoc (Reducer state finalize step) =
  Reducer
    { reducerInitState = (Nothing, state)
    , reducerFinalize = \(r, s) -> case r of
        Nothing -> Nothing
        Just lst -> Just (finalize s, lst)
    , reducerStep = \(mr, s) a -> case mr of
        Nothing -> Continue (Just a, s)
        Just lst -> fmap (Just a,) (step s lst)
    }

{-# INLINE scan' #-}
scan' ::
  forall (a :: Type) (r :: Type) (acc :: Type).
  (acc -> a -> acc) ->
  acc ->
  Reducer (acc, a) r ->
  Reducer a r
scan' scanStep scanInit (Reducer state finalize step) =
  Reducer
    { reducerInitState = (scanInit, state)
    , reducerFinalize = \(_, state) -> finalize state
    , reducerStep = \(scanAcc, s) a ->
        case step s (scanAcc, a) of
          Reduced s' -> Reduced (scanAcc, s')
          Continue s' -> Continue (scanAcc `seq` scanStep scanAcc a, s')
    }

{- | Associate index with each element, starting at zero.

===== Examples

>>> reduceList (enumerate |> intoList) ["foo", "bar"]
[(0,"foo"),(1,"bar")]

@since 1.0.0
-}
{-# INLINE enumerate #-}
enumerate ::
  Reducer (Int, a) r ->
  Reducer a r
enumerate = scan' (\acc _ -> acc + 1) 0
