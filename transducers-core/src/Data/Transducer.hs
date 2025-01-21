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
  zipReducers,
  zipReducersSplit,
  zipReducersFork,

  -- * Building Blocks
  Reduced (..),
  simpleStatelessReducer,
  statelessTransducer,
  makeTransducer,
) where

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
type Reduced :: Type -> Type
data Reduced a
  = -- | The reduction finished and the reducer step should not be called anymore.
    Reduced a
  | -- | The reduction is still in progress and the reducer step should be called if there are more values.
    Continue a

{- |
@
data Reducer s a r
             ^ ^ ^
             | | |
             | | +-- Accumulator that will be returned after reduction
             | |
             | +-- Elements that the reducer is processing
             |
             +-- Internal state of the reducer
@

@since 1.0.0
-}
type Reducer :: Type -> Type -> Type -> Type
data Reducer s a r = Reducer
  { reducerInitState :: s
  , reducerInitAcc :: r
  , reducerFinalize :: s -> r -> r
  , reducerStep :: s -> r -> a -> (Reduced r, s)
  }

{- | Run a reducer on all list elements.

===== Examples

>>> reduceList sum [1, 2, 3]
6

@since 1.0.0
-}
reduceList :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> [a] -> r
reduceList reducer as =
  let (r', s') = go (reducerInitState reducer) (reducerInitAcc reducer) as
   in reducerFinalize reducer s' r'
  where
    go s r [] = (r, s)
    go s r (a : as) =
      case reducerStep reducer s r a of
        (Reduced r', s') -> (r', s')
        (Continue r', s') -> go s' r' as

{- | Run a reducer on all elements of non-empty list.

===== Examples

>>> reduceNonEmpty sum (1 :| [2, 3])
6

@since 1.0.0
-}
reduceNonEmpty :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> NonEmpty a -> r
reduceNonEmpty reducer (a :| as) = reduceList reducer (a : as)

{- | Run a reducer on a single element.

===== Examples

>>> reduceSingleton head 1
Just 1

>>> reduceSingleton (drop 1 |> head) 1
Nothing

@since 1.0.0
-}
reduceSingleton :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> a -> r
reduceSingleton reducer a = reduceList reducer [a]

{- | Run a reducer on an infinite sequence of repeated function applications.

===== Examples

>>> reduceIterate (take 10 |> intoList) (+3) 42
[42,45,48,51,54,57,60,63,66,69]

@since 1.0.0
-}
reduceIterate :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> (a -> a) -> a -> r
reduceIterate reducer f a =
  let (r', s') = go (reducerInitState reducer) (reducerInitAcc reducer) a
   in reducerFinalize reducer s' r'
  where
    go s r a = case reducerStep reducer s r a of
      (Reduced r', s') -> (r', s')
      (Continue r', s') -> go s' r' (f a)

{- | Run a reducer on an infinite sequence of the same value.

===== Examples

>>> reduceRepeat (take 10 |> intoList) 42
[42,42,42,42,42,42,42,42,42,42]

@since 1.0.0
-}
reduceRepeat :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> a -> r
reduceRepeat reducer a =
  let (r', s') = go (reducerInitState reducer) (reducerInitAcc reducer)
   in reducerFinalize reducer s' r'
  where
    go s r = case reducerStep reducer s r a of
      (Reduced r', s') -> (r', s')
      (Continue r', s') -> go s' r'

{- | Run a reducer on a finite sequence of the same value.

===== Examples

>>> reduceReplicate intoList 10 42
[42,42,42,42,42,42,42,42,42,42]

@since 1.0.0
-}
reduceReplicate :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> Int -> a -> r
reduceReplicate reducer n a =
  let (r', s') = go (reducerStep reducer) (reducerInitState reducer) (reducerInitAcc reducer) n
   in reducerFinalize reducer s' r'
  where
    go step s r n =
      if n <= 0
        then (r, s)
        else case step s r a of
          (Reduced r', s') -> (r', s')
          (Continue r', s') -> go step s' r' (n - 1)

{- | Construct a stateless reducer that consumes whole input.

===== Examples

@sum = simpleStatelessReducer 0 (+)@

>>> reduceList (simpleStatelessReducer 0 (+)) [1, 2, 3]
6

@since 1.0.0
-}
simpleStatelessReducer :: forall (a :: Type) (r :: Type). r -> (r -> a -> r) -> Reducer () a r
simpleStatelessReducer acc f =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = acc
    , reducerFinalize = const id
    , reducerStep = \s r a -> (Continue (f r a), s)
    }

{- | Get the sum of the elements in the sequence.

===== Examples

>>> reduceList sum [1..10]
55

@since 1.0.0
-}
sum :: forall (r :: Type). Num r => Reducer () r r
sum = simpleStatelessReducer 0 (+)

{- | Get the product of the elements in the sequence.

===== Examples

>>> reduceList product [1..5]
120

@since 1.0.0
-}
product :: forall (r :: Type). Num r => Reducer () r r
product = simpleStatelessReducer 1 (*)

{- | Get the largest element of the sequence.

===== Examples

>>> reduceList maximum [1,3,2]
Just 3

>>> reduceList maximum []
Nothing

@since 1.0.0
-}
maximum :: forall (a :: Type). Ord a => Reducer () a (Maybe a)
maximum = simpleStatelessReducer Nothing (\r a -> max (Just a) r)

{- | Get the smallest element of the sequence.

===== Examples

>>> reduceList minimum [1,3,2]
Just 1

>>> reduceList minimum []
Nothing

@since 1.0.0
-}
minimum :: forall (a :: Type). Ord a => Reducer () a (Maybe a)
minimum = simpleStatelessReducer Nothing $ \r a ->
  case r of
    Nothing -> Just a
    Just r' -> Just (min r' a)

{- | Get the length of the sequence.

===== Examples

>>> reduceList length [1,3,2]
3

@since 1.0.0
-}
length :: forall (a :: Type). Reducer () a Int
length = simpleStatelessReducer 0 (\acc _ -> acc + 1)

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
compareLength :: forall (a :: Type). Int -> Reducer Int a Ordering
compareLength n =
  Reducer
    { reducerInitState = 0
    , reducerInitAcc = compare 0 n
    , reducerFinalize = const id
    , reducerStep = \s _ _ ->
        case compare (s + 1) n of
          LT -> (Continue LT, s + 1)
          EQ -> (Continue EQ, s + 1)
          GT -> (Reduced GT, s + 1)
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
or :: Reducer () Bool Bool
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
and :: Reducer () Bool Bool
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
all :: forall (a :: Type). (a -> Bool) -> Reducer () a Bool
all pred =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = True
    , reducerFinalize = const id
    , reducerStep = \s _ a ->
        if pred a
          then (Continue True, s)
          else (Reduced False, s)
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
any :: forall (a :: Type). (a -> Bool) -> Reducer () a Bool
any pred =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = False
    , reducerFinalize = const id
    , reducerStep = \s _ a ->
        if pred a
          then (Reduced True, s)
          else (Continue False, s)
    }

{- | Check if sequence is empty.

===== Examples

>>> reduceList null []
True

>>> reduceList null [1..]
False

@since 1.0.0
-}
null :: forall (a :: Type). Reducer () a Bool
null =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = True
    , reducerFinalize = const id
    , reducerStep = \s _ _ -> (Reduced False, s)
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
head :: forall (a :: Type). Reducer () a (Maybe a)
head =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = Nothing
    , reducerFinalize = const id
    , reducerStep = \s _ a -> (Reduced (Just a), s)
    }

{- | Extract the last element if exists.

===== Examples

>>> reduceList last [1, 2, 3]
Just 3

>>> reduceList last []
Nothing

@since 1.0.0
-}
last :: forall (a :: Type). Reducer () a (Maybe a)
last = simpleStatelessReducer Nothing (const Just)

{- | Get the first element for which the passed predicate returns 'True', if exists.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (find even) [1, 2, 3]
Just 2

>>> reduceList (find even) [1, 3, 5]
Nothing

@since 1.0.0
-}
find :: forall (a :: Type). (a -> Bool) -> Reducer () a (Maybe a)
find pred =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = Nothing
    , reducerFinalize = const id
    , reducerStep = \s _ a ->
        if pred a
          then (Reduced (Just a), s)
          else (Continue Nothing, s)
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
elemBy :: forall (a :: Type). (a -> Bool) -> Reducer () a Bool
elemBy pred =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = False
    , reducerFinalize = const id
    , reducerStep = \s _ a ->
        if pred a
          then (Reduced True, s)
          else (Continue False, s)
    }

{- | Check if any elements is equal (using '==') to the one passed.

===== Examples

>>> reduceList (elem 2) [1, 2, 3]
True

>>> reduceList (elem 2) [1, 3, 5]
False

@since 1.0.0
-}
elem :: forall (a :: Type). Eq a => a -> Reducer () a Bool
elem a = elemBy (a ==)

{- | Collect all elements into a list.

===== Examples

>>> reduceReplicate intoList 10 42
[42,42,42,42,42,42,42,42,42,42]

@since 1.0.0
-}
intoList :: forall (a :: Type). Reducer () a [a]
intoList =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = []
    , reducerFinalize = const reverse
    , reducerStep = \s as a -> (Continue (a : as), s)
    }

{- | Collect all elements into a non-empty list or @Nothing@ if it does not contain any elements.

===== Examples

>>> reduceList intoNonEmpty [1,2,3]
Just (1 :| [2,3])

>>> reduceList intoNonEmpty []
Nothing

@since 1.0.0
-}
intoNonEmpty :: forall (a :: Type). Reducer () a (Maybe (NonEmpty a))
intoNonEmpty =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = Nothing
    , reducerFinalize = const (fmap NonEmpty.reverse)
    , reducerStep = \s as a ->
        case as of
          Nothing -> (Continue (Just (a :| [])), s)
          Just (b :| bs) -> (Continue (Just (a :| b : bs)), s)
    }

{- | Run two reducers on the same input.

===== Examples

>>> reduceList (zipReducers sum product) [1,2,3,4]
(10,24)

@since 1.0.0
-}
zipReducers ::
  Reducer s1 a r1 ->
  Reducer s2 a r2 ->
  Reducer (ZipFinished, s1, s2) a (r1, r2)
zipReducers reducer1 reducer2 = map (\a -> (a, a)) |> zipReducersSplit reducer1 reducer2

{- | Run first reducer on first element of the tuple and second reducer on the second.

===== Examples

Collect @fst@ into a list and sums the @snd@

>>> reduceList (zipReducersSplit intoList sum) [(1, 2), (3, 4)]
([1,3],6)

@since 1.0.0
-}
zipReducersSplit ::
  Reducer s1 a1 r1 ->
  Reducer s2 a2 r2 ->
  Reducer (ZipFinished, s1, s2) (a1, a2) (r1, r2)
zipReducersSplit reducer1 reducer2 =
  Reducer
    { reducerInitState = (ZipFinishedNone, reducerInitState reducer1, reducerInitState reducer2)
    , reducerInitAcc = (reducerInitAcc reducer1, reducerInitAcc reducer2)
    , reducerFinalize = \(_, s1, s2) (r1, r2) ->
        (reducerFinalize reducer1 s1 r1, reducerFinalize reducer2 s2 r2)
    , reducerStep = \(finished, s1, s2) (r1, r2) (a1, a2) ->
        case finished of
          ZipFinishedNone ->
            let (r1', s1') = reducerStep reducer1 s1 r1 a1
                (r2', s2') = reducerStep reducer2 s2 r2 a2
             in case (r1', r2') of
                  (Continue r1'', Continue r2'') -> (Continue (r1'', r2''), (ZipFinishedNone, s1', s2'))
                  (Reduced r1'', Continue r2'') -> (Continue (r1'', r2''), (ZipFinished1, s1', s2'))
                  (Continue r1'', Reduced r2'') -> (Continue (r1'', r2''), (ZipFinished2, s1', s2'))
                  (Reduced r1'', Reduced r2'') -> (Reduced (r1'', r2''), (ZipFinishedBoth, s1', s2'))
          ZipFinished1 ->
            let (r2', s2') = reducerStep reducer2 s2 r2 a2
             in case r2' of
                  Continue r2'' -> (Continue (r1, r2''), (ZipFinished1, s1, s2'))
                  Reduced r2'' -> (Reduced (r1, r2''), (ZipFinishedBoth, s1, s2'))
          ZipFinished2 ->
            let (r1', s1') = reducerStep reducer1 s1 r1 a1
             in case r1' of
                  Continue r1'' -> (Continue (r1'', r2), (ZipFinished1, s1', s2))
                  Reduced r1'' -> (Reduced (r1'', r2), (ZipFinishedBoth, s1', s2))
          ZipFinishedBoth -> (Reduced (r1, r2), (ZipFinishedBoth, s1, s2))
    }

{- | Run first reducer on @Left@ elements and second reducer on @Right@s.

===== Examples

Collect @fst@ into a list and sums the @snd@

>>> reduceList (zipReducersFork intoList sum) [Left 1, Right 2, Left 3, Left 4, Right 5]
([1,3,4],7)

@since 1.0.0
-}
zipReducersFork ::
  Reducer s1 a1 r1 ->
  Reducer s2 a2 r2 ->
  Reducer (ZipFinished, s1, s2) (Either a1 a2) (r1, r2)
zipReducersFork reducer1 reducer2 =
  Reducer
    { reducerInitState = (ZipFinishedNone, reducerInitState reducer1, reducerInitState reducer2)
    , reducerInitAcc = (reducerInitAcc reducer1, reducerInitAcc reducer2)
    , reducerFinalize = \(_, s1, s2) (r1, r2) ->
        (reducerFinalize reducer1 s1 r1, reducerFinalize reducer2 s2 r2)
    , reducerStep = \(finished, s1, s2) (r1, r2) a ->
        case a of
          Left a1 ->
            case finished of
              ZipFinishedNone ->
                let (r1', s1') = reducerStep reducer1 s1 r1 a1
                 in case r1' of
                      Continue r1'' -> (Continue (r1'', r2), (ZipFinishedNone, s1', s2))
                      Reduced r1'' -> (Continue (r1'', r2), (ZipFinished1, s1', s2))
              ZipFinished1 -> (Continue (r1, r2), (ZipFinished1, s1, s2))
              ZipFinished2 ->
                let (r1', s1') = reducerStep reducer1 s1 r1 a1
                 in case r1' of
                      Continue r1'' -> (Continue (r1'', r2), (ZipFinished1, s1', s2))
                      Reduced r1'' -> (Reduced (r1'', r2), (ZipFinished2, s1', s2))
              ZipFinishedBoth -> (Reduced (r1, r2), (ZipFinishedBoth, s1, s2))
          Right a2 ->
            case finished of
              ZipFinishedNone ->
                let (r2', s2') = reducerStep reducer2 s2 r2 a2
                 in case r2' of
                      Continue r2'' -> (Continue (r1, r2''), (ZipFinishedNone, s1, s2'))
                      Reduced r2'' -> (Continue (r1, r2''), (ZipFinished2, s1, s2'))
              ZipFinished1 ->
                let (r2', s2') = reducerStep reducer2 s2 r2 a2
                 in case r2' of
                      Continue r2'' -> (Continue (r1, r2''), (ZipFinished1, s1, s2'))
                      Reduced r2'' -> (Reduced (r1, r2''), (ZipFinished2, s1, s2'))
              ZipFinished2 -> (Continue (r1, r2), (ZipFinished2, s1, s2))
              ZipFinishedBoth -> (Reduced (r1, r2), (ZipFinishedBoth, s1, s2))
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
(|>) ::
  forall (a :: Type) (b :: Type) (r :: Type) (s1 :: Type) (s2 :: Type).
  (Reducer s1 a r -> Reducer s2 b r) ->
  Reducer s1 a r ->
  Reducer s2 b r
(|>) transducer = transducer

infixr 5 |>

{- | Create a transducer that does not change the state or accumulator.

@since 1.0.0
-}
statelessTransducer ::
  forall (a :: Type) (b :: Type) (r :: Type) (s :: Type).
  Reducer s a r ->
  (s -> r -> b -> (Reduced r, s)) ->
  Reducer s b r
statelessTransducer reducer reducerStep =
  Reducer
    { reducerInitState = reducerInitState reducer
    , reducerInitAcc = reducerInitAcc reducer
    , reducerFinalize = reducerFinalize reducer
    , reducerStep
    }

{- | Create a transducer that does not change accumulator.

@since 1.0.0
-}
makeTransducer ::
  forall (a :: Type) (b :: Type) (r :: Type) (s1 :: Type) (s2 :: Type).
  Reducer s1 a r ->
  s2 ->
  ((s2, s1) -> r -> b -> (Reduced r, (s2, s1))) ->
  Reducer (s2, s1) b r
makeTransducer reducer s2 reducerStep =
  Reducer
    { reducerInitState = (s2, reducerInitState reducer)
    , reducerInitAcc = reducerInitAcc reducer
    , reducerFinalize = \(_, s1) r -> reducerFinalize reducer s1 r
    , reducerStep
    }

{- | Apply passed function to every element in the sequence

===== Examples

>>> import GHC.Real (even)
>>> reduceList (map even |> intoList) [2,6,4,5,8]
[True,True,True,False,True]

@since 1.0.0
-}
map ::
  forall (a :: Type) (b :: Type) (r :: Type) (s :: Type).
  (b -> a) ->
  Reducer s a r ->
  Reducer s b r
map f reducer = statelessTransducer reducer $ \s r b -> reducerStep reducer s r (f b)

{- | Take first @n@ elements from the beginning of the sequence.

===== Examples

>>> reduceList (take 3 |> intoList) [1..]
[1,2,3]

@since 1.0.0
-}
take ::
  forall (a :: Type) (r :: Type) (s :: Type).
  Int ->
  Reducer s a r ->
  Reducer (Int, s) a r
take n reducer = makeTransducer reducer n $ \(currN, s) r a ->
  if currN <= 0
    then (Reduced r, (currN, s))
    else
      let (r', s') = reducerStep reducer s r a
       in (r', (currN - 1, s'))

{- | Keep taking elements from the sequence as long as the passed predicate returns 'True'.
Note that it short-circuits upon first value that fails the predicate even if some further value
would pass it.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (takeWhile even |> intoList) [2,6,4,5,8]
[2,6,4]

@since 1.0.0
-}
takeWhile ::
  forall (a :: Type) (r :: Type) (s :: Type).
  (a -> Bool) ->
  Reducer s a r ->
  Reducer s a r
takeWhile pred reducer = statelessTransducer reducer $ \s r a ->
  if pred a
    then reducerStep reducer s r a
    else (Reduced r, s)

{- | Drop (skip/remove) first @n@ elements from the beginning of the sequence.

===== Examples

>>> reduceList (drop 3 |> intoList) [1..5]
[4,5]

@since 1.0.0
-}
drop ::
  forall (a :: Type) (r :: Type) (s :: Type).
  Int ->
  Reducer s a r ->
  Reducer (Int, s) a r
drop n reducer = makeTransducer reducer n $ \(currN, s) r a ->
  if currN <= 0
    then
      let (r', s') = reducerStep reducer s r a
       in (r', (currN, s'))
    else (Continue r, (currN - 1, s))

{- | Keep dropping elements from the sequence as long as the passed predicate returns 'True'.
Note that upon first value that fails the predicate the whole remaining sequence will be forwarded
even if some further value would pass it.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (dropWhile even |> intoList) [2,6,4,5,8]
[5,8]

@since 1.0.0
-}
dropWhile ::
  forall (a :: Type) (r :: Type) (s :: Type).
  (a -> Bool) ->
  Reducer s a r ->
  Reducer (Bool, s) a r
dropWhile pred reducer = makeTransducer reducer False $ \(finishedDropping, s) r a ->
  if finishedDropping
    then case reducerStep reducer s r a of
      (r', s') -> (r', (True, s'))
    else
      if pred a
        then (Continue r, (False, s))
        else case reducerStep reducer s r a of
          (r', s') -> (r', (True, s'))

{- | Keep only elements for which the passed predicate returns 'True'.

===== Examples

>>> import GHC.Real (even)
>>> reduceList (filter even |> intoList) [1..10]
[2,4,6,8,10]

@since 1.0.0
-}
filter ::
  forall (a :: Type) (r :: Type) (s :: Type).
  (a -> Bool) ->
  Reducer s a r ->
  Reducer s a r
filter pred reducer = statelessTransducer reducer $ \s r a ->
  if pred a
    then reducerStep reducer s r a
    else (Continue r, s)

{- | Keep only elements that are mapped to 'Just'.

===== Examples

>>> import Text.Read (readMaybe)
>>> reduceList (mapMaybe (readMaybe @Int) |> intoList) ["1", "foo", "2", "bar"]
[1,2]

@since 1.0.0
-}
mapMaybe ::
  forall (a :: Type) (b :: Type) (r :: Type) (s :: Type).
  (b -> Maybe a) ->
  Reducer s a r ->
  Reducer s b r
mapMaybe f reducer = statelessTransducer reducer $ \s r a ->
  case f a of
    Nothing -> (Continue r, s)
    Just a' -> reducerStep reducer s r a'

{- | Keep only 'Just' elements.

===== Examples

>>> reduceList (catMaybes |> intoList) [Just 1, Nothing, Just 2, Just 3, Nothing]
[1,2,3]

@since 1.0.0
-}
catMaybes ::
  forall (a :: Type) (r :: Type) (s :: Type).
  Reducer s a r ->
  Reducer s (Maybe a) r
catMaybes reducer = statelessTransducer reducer $ \s r -> \case
  Nothing -> (Continue r, s)
  Just a -> reducerStep reducer s r a

{- | Insert an element in between every element in the sequence.

===== Examples

>>> reduceList (intersperse 0 |> intoList) [1,2,3]
[1,0,2,0,3]

@since 1.0.0
-}
intersperse ::
  forall (a :: Type) (r :: Type) (s :: Type).
  a ->
  Reducer s a r ->
  Reducer (Bool, s) a r
intersperse middle reducer = makeTransducer reducer False $ \(acc, s) r a ->
  if acc
    then
      let (r2, s2) = reducerStep reducer s r middle
       in case r2 of
            Reduced r3 -> (Reduced r3, (True, s2))
            Continue r3 ->
              let (r4, s3) = reducerStep reducer s2 r3 a
               in (r4, (True, s3))
    else
      let (r', s') = reducerStep reducer s r a
       in (r', (True, s'))

{- | Flatten a sequence of @[a]@ into a sequence of @a@.

===== Examples

>>> reduceList (concatList |> intoList) [[1,2],[3,4,5]]
[1,2,3,4,5]

@since 1.0.0
-}
concatList ::
  forall (a :: Type) (s :: Type) (r :: Type).
  Reducer s a r ->
  Reducer s [a] r
concatList reducer = statelessTransducer reducer step
  where
    step :: s -> r -> [a] -> (Reduced r, s)
    step s r = \case
      [] -> (Continue r, s)
      (a : as) -> case reducerStep reducer s r a of
        (Reduced r', s') -> (Reduced r', s')
        (Continue r', s') -> step s' r' as

{- | Flatten a sequence of @NonEmpty a@ into a sequence of @a@.

===== Examples

>>> reduceList (concatNonEmpty |> intoList) [1:|[2], 3:|[4,5]]
[1,2,3,4,5]

@since 1.0.0
-}
concatNonEmpty ::
  forall (a :: Type) (s :: Type) (r :: Type).
  Reducer s a r ->
  Reducer s (NonEmpty a) r
concatNonEmpty reducer = map NonEmpty.toList |> concatList reducer

{- | Remove duplicate elements and keep only the first occurrence that are equal accordingly to '=='.

Note that it has the same performance problems as @Data.List.nub@.

===== Examples

>>> reduceList (nub |> intoList) [1,2,1,3,3,2,2]
[1,2,3]

@since 1.0.0
-}
nub ::
  forall (a :: Type) (s :: Type) (r :: Type).
  Eq a =>
  Reducer s a r ->
  Reducer ([a], s) a r
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
nubBy ::
  forall (a :: Type) (s :: Type) (r :: Type).
  (a -> a -> Bool) ->
  Reducer s a r ->
  Reducer ([a], s) a r
nubBy f reducer = makeTransducer reducer [] $ \(seen, s) r a ->
  if list_elemBy f a seen
    then (Continue r, (seen, s))
    else case reducerStep reducer s r a of
      (r', s') -> (r', (a : seen, s'))
  where
    list_elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
    list_elemBy _ _ [] = False
    list_elemBy eq y (x : xs) = x `eq` y || list_elemBy eq y xs

{- | Preserve the first element and run the passed reducer on the remaining sequence.
'Nothing' if the sequence has no elements.

===== Examples

>>> reduceList (uncons sum) [1,2,3]
Just (1,5)

@since 1.0.0
-}
uncons ::
  Reducer s a r ->
  Reducer s a (Maybe (a, r))
uncons reducer =
  Reducer
    { reducerInitState = reducerInitState reducer
    , reducerInitAcc = Nothing
    , reducerFinalize = \s -> \case
        Nothing -> Nothing
        Just (fst, r) -> Just (fst, reducerFinalize reducer s r)
    , reducerStep = \s mr a -> case mr of
        Nothing -> (Continue (Just (a, reducerInitAcc reducer)), s)
        Just (fst, r) -> case reducerStep reducer s r a of
          (Reduced r', s') -> (Reduced (Just (fst, r')), s')
          (Continue r', s') -> (Continue (Just (fst, r')), s')
    }

{- | Preserve the last element and run the passed reducer on the initial sequence.
'Nothing' if the sequence has no elements.

===== Examples

>>> reduceList (unsnoc sum) [1,2,3,4]
Just (6,4)

@since 1.0.0
-}
unsnoc ::
  Reducer s a r ->
  Reducer s a (Maybe (r, a))
unsnoc reducer =
  Reducer
    { reducerInitState = reducerInitState reducer
    , reducerInitAcc = Nothing
    , reducerFinalize = \s -> \case
        Nothing -> Nothing
        Just (r, lst) -> Just (reducerFinalize reducer s r, lst)
    , reducerStep = \s mr a -> case mr of
        Nothing -> (Continue (Just (reducerInitAcc reducer, a)), s)
        Just (r, lst) -> case reducerStep reducer s r lst of
          (Reduced r', s') -> (Reduced (Just (r', a)), s')
          (Continue r', s') -> (Continue (Just (r', a)), s')
    }
