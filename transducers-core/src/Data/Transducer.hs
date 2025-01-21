module Data.Transducer (
  Reducer (..),
  Reduced (..),

  -- * Runners
  reduceList,
  reduceNonEmpty,
  reduceSingleton,
  reduceIterate,
  reduceRepeat,

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
  zipReducers,
  zipReducersSplit,
  zipReducersFork,

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

  -- * Building Blocks
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

type Reduced :: Type -> Type
data Reduced a
  = Reduced a
  | Continue a

type Reducer :: Type -> Type -> Type -> Type
data Reducer s a r = Reducer
  { reducerInitState :: s
  , reducerInitAcc :: r
  , reducerFinalize :: s -> r -> r
  , reducerStep :: s -> r -> a -> (Reduced r, s)
  }

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

reduceNonEmpty :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> NonEmpty a -> r
reduceNonEmpty reducer (a :| as) = reduceList reducer (a : as)

reduceSingleton :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> a -> r
reduceSingleton reducer a = reduceList reducer [a]

reduceIterate :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> (a -> a) -> a -> r
reduceIterate reducer f a =
  let (r', s') = go (reducerInitState reducer) (reducerInitAcc reducer) a
   in reducerFinalize reducer s' r'
  where
    go s r a = case reducerStep reducer s r a of
      (Reduced r', s') -> (r', s')
      (Continue r', s') -> go s' r' (f a)

reduceRepeat :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> a -> r
reduceRepeat reducer a =
  let (r', s') = go (reducerInitState reducer) (reducerInitAcc reducer)
   in reducerFinalize reducer s' r'
  where
    go s r = case reducerStep reducer s r a of
      (Reduced r', s') -> (r', s')
      (Continue r', s') -> go s' r'

simpleStatelessReducer :: forall (a :: Type) (r :: Type). r -> (r -> a -> r) -> Reducer () a r
simpleStatelessReducer acc f =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = acc
    , reducerFinalize = const id
    , reducerStep = \s r a -> (Continue (f r a), s)
    }

sum :: forall (r :: Type). Num r => Reducer () r r
sum = simpleStatelessReducer 0 (+)

product :: forall (r :: Type). Num r => Reducer () r r
product = simpleStatelessReducer 1 (*)

maximum :: forall (a :: Type). Ord a => Reducer () a (Maybe a)
maximum = simpleStatelessReducer Nothing (\r a -> max (Just a) r)

minimum :: forall (a :: Type). Ord a => Reducer () a (Maybe a)
minimum = simpleStatelessReducer Nothing $ \r a ->
  case r of
    Nothing -> Just a
    Just r' -> Just (min r' a)

length :: forall (a :: Type). Reducer () a Int
length = simpleStatelessReducer 0 (\acc _ -> acc + 1)

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

or :: Reducer () Bool Bool
or = any id

and :: Reducer () Bool Bool
and = all id

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

null :: forall (a :: Type). Reducer () a Bool
null =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = True
    , reducerFinalize = const id
    , reducerStep = \s _ _ -> (Reduced False, s)
    }

head :: forall (a :: Type). Reducer () a (Maybe a)
head =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = Nothing
    , reducerFinalize = const id
    , reducerStep = \s _ a -> (Reduced (Just a), s)
    }

last :: forall (a :: Type). Reducer () a (Maybe a)
last = simpleStatelessReducer Nothing (const Just)

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

elem :: forall (a :: Type). Eq a => a -> Reducer () a Bool
elem a = elemBy (a ==)

intoList :: forall (a :: Type). Reducer () a [a]
intoList =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = []
    , reducerFinalize = const reverse
    , reducerStep = \s as a -> (Continue (a : as), s)
    }

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

zipReducers ::
  Reducer s1 a r1 ->
  Reducer s2 a r2 ->
  Reducer (ZipFinished, s1, s2) a (r1, r2)
zipReducers reducer1 reducer2 = map (\a -> (a, a)) |> zipReducersSplit reducer1 reducer2

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

(|>) ::
  forall (a :: Type) (b :: Type) (r :: Type) (s1 :: Type) (s2 :: Type).
  (Reducer s1 a r -> Reducer s2 b r) ->
  Reducer s1 a r ->
  Reducer s2 b r
(|>) transducer = transducer
infixr 5 |>

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

map ::
  forall (a :: Type) (b :: Type) (r :: Type) (s :: Type).
  (b -> a) ->
  Reducer s a r ->
  Reducer s b r
map f reducer = statelessTransducer reducer $ \s r b -> reducerStep reducer s r (f b)

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

takeWhile ::
  forall (a :: Type) (r :: Type) (s :: Type).
  (a -> Bool) ->
  Reducer s a r ->
  Reducer s a r
takeWhile pred reducer = statelessTransducer reducer $ \s r a ->
  if pred a
    then reducerStep reducer s r a
    else (Reduced r, s)

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

filter ::
  forall (a :: Type) (r :: Type) (s :: Type).
  (a -> Bool) ->
  Reducer s a r ->
  Reducer s a r
filter pred reducer = statelessTransducer reducer $ \s r a ->
  if pred a
    then reducerStep reducer s r a
    else (Continue r, s)

mapMaybe ::
  forall (a :: Type) (b :: Type) (r :: Type) (s :: Type).
  (b -> Maybe a) ->
  Reducer s a r ->
  Reducer s b r
mapMaybe f reducer = statelessTransducer reducer $ \s r a ->
  case f a of
    Nothing -> (Continue r, s)
    Just a' -> reducerStep reducer s r a'

catMaybes ::
  forall (a :: Type) (r :: Type) (s :: Type).
  Reducer s a r ->
  Reducer s (Maybe a) r
catMaybes reducer = statelessTransducer reducer $ \s r -> \case
  Nothing -> (Continue r, s)
  Just a -> reducerStep reducer s r a

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

concatNonEmpty ::
  forall (a :: Type) (s :: Type) (r :: Type).
  Reducer s a r ->
  Reducer s (NonEmpty a) r
concatNonEmpty reducer = map NonEmpty.toList |> concatList reducer

nub ::
  forall (a :: Type) (s :: Type) (r :: Type).
  Eq a =>
  Reducer s a r ->
  Reducer ([a], s) a r
nub = nubBy (==)

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
