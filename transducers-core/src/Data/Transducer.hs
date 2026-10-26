{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
t'Reducer's are generalized data processing steps that can be reused across multiple collections.
They process elements one by one, can carry state, and have ability to short-circuit.

Transducers are functions that take one t'Reducer' and return another.

This module is intended to be imported qualified or used without implicit @Prelude@.
Function names collide with @base@ functions that operate on lists and other collections though
output types are sometimes modified for our t'Reducer's to remain total.
-}
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
import Data.Kind (Type)
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
  { state :: s
  , finalize :: s -> r
  , step :: s -> a -> Reduced s
  }

{- | Change the final accumulator value.

Note that 'fmap' is different than 'map':

@
'fmap' :: (r -> x) -> t'Reducer' a r -> t'Reducer' a x
 'map' :: (b -> a) -> t'Reducer' a r -> t'Reducer' b r
@

@since 1.0.0
-}
instance Functor (Reducer a) where
  {-# INLINE fmap #-}
  fmap f (Reducer state finalize step) = (Reducer state (\s -> f (finalize s)) step)

{- | 'liftA2' runs two reducers in parallel, together with @ApplicativeDo@ it allows to
ergonomically define new reducers that run multiple operations with a single input traversal:

@
{\-# LANGUAGE ApplicativeDo #-\}

mean :: t'Reducer' 'Int' ('Maybe' 'Int')
mean = do
  total <- 'sum'
  len <- 'length'
  'pure' (case len of 0 -> 'Nothing'; _ -> 'Just' (total \`'Preldue.div'\` len))
@

@since 1.0.0
-}
instance Applicative (Reducer a) where
  {-# INLINE pure #-}
  pure :: forall (r :: Type). r -> Reducer a r
  pure r =
    Reducer
      { state = ()
      , finalize = const r
      , step = \_ _ -> Reduced ()
      }

  {-# INLINE liftA2 #-}
  liftA2 :: (x -> y -> r) -> Reducer a x -> Reducer a y -> Reducer a r
  liftA2 f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
    Reducer
      { state = (ZipFinishedNone, state1, state2)
      , finalize = \(_, s1, s2) -> f (finalize1 s1) (finalize2 s2)
      , step = \s a -> zipStepBoth step1 step2 s a a
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

{- | @('<>') = 'liftA2' ('<>')@

@since 1.0.0
-}
instance Semigroup r => Semigroup (Reducer a r) where
  (<>) = liftA2 (<>)

{- | @'mempty' = 'pure' 'mempty'@

@since 1.0.0
-}
instance Monoid r => Monoid (Reducer a r) where
  mempty = pure mempty

{- | Result of the reduction process.

@since 1.0.0
-}
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

{- | Extract the result of the reduction.

===== Examples

>>> unReduced (Reduced 42)
42

>>> unReduced (Continue 42)
42

@since 1.0.0
-}
{-# INLINE unReduced #-}
unReduced :: Reduced a -> a
unReduced = \case
  Reduced a -> a
  Continue a -> a

-- * Runners

{- $runners
To run a t'Reducer' it must be fed input elements until it finishes the process.
All of @reduce*@ functions take a t'Reducer' and a source.
-}

{- | Run a reducer on all list elements.

===== Examples

>>> reduceList sum [1,2,3]
6

@since 1.0.0
-}
{-# INLINE reduceList #-}
reduceList :: forall (a :: Type) (r :: Type). Reducer a r -> [a] -> r
reduceList = reduceFoldr

{- | Run a reducer on all elements of non-empty list.

===== Examples

>>> reduceNonEmpty sum (1 :| [2,3])
6

@since 1.0.0
-}
{-# INLINE reduceNonEmpty #-}
reduceNonEmpty :: forall (a :: Type) (r :: Type). Reducer a r -> NonEmpty a -> r
reduceNonEmpty = reduceFoldr

{- | Run a non-empty reducer on all elements of non-empty list.

===== Examples

>>> reduceNonEmpty1 maximum1 (2 :| [1,4,3])
4

@since 1.0.0
-}
{-# INLINE reduceNonEmpty1 #-}
reduceNonEmpty1 :: forall (a :: Type) (r :: Type). (a -> Reducer a r) -> NonEmpty a -> r
reduceNonEmpty1 mkReducer (a :| as) = reduceList (mkReducer a) as

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

{- | Run a reducer on any 'Foldable' by feeding elements with 'Foldable.foldr'.

>>> import Data.Complex (Complex ((:+)))
>>> reduceFoldr head (1 :+ 2)
Just 1

@since 1.0.0
-}
{-# INLINE reduceFoldr #-}
reduceFoldr ::
  forall (a :: Type) (r :: Type) (f :: Type -> Type).
  Foldable f =>
  Reducer a r ->
  f a ->
  r
reduceFoldr (Reducer state finalize step) f = finalize (Foldable.foldr go id f state)
  where
    go a k s =
      case step s a of
        Reduced s' -> s'
        Continue s' -> k s'

-- * Reducers

{- | Get the sum of the elements in the sequence.

===== Examples

>>> reduceList sum [1..10]
55

>>> reduceList sum ([] :: [Int])
0

@since 1.0.0
-}
{-# INLINE sum #-}
sum :: forall (r :: Type). Num r => Reducer r r
sum = linearReducer 0 (+)

{- | Get the product of the elements in the sequence.

===== Examples

>>> reduceList product [1..5]
120

>>> reduceList product ([] :: [Int])
1

@since 1.0.0
-}
{-# INLINE product #-}
product :: forall (r :: Type). Num r => Reducer r r
product = linearReducer 1 (*)

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
maximum = linearReducer Nothing (\r a -> max (Just a) r)

{- | Get the largest element of the non-empty sequence.

===== Examples

>>> reduceNonEmpty1 maximum1 (2 :| [1,4,3])
4

@since 1.0.0
-}
{-# INLINE maximum1 #-}
maximum1 :: forall (a :: Type). Ord a => a -> Reducer a a
maximum1 a = linearReducer a max

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
minimum = linearReducer Nothing $ \r a ->
  case r of
    Nothing -> Just a
    Just r' -> let !m = min r' a in Just m

{- | Get the largest element of the non-empty sequence.

===== Examples

>>> reduceNonEmpty1 minimum1 (2 :| [1,4,3])
1

@since 1.0.0
-}
{-# INLINE minimum1 #-}
minimum1 :: forall (a :: Type). Ord a => a -> Reducer a a
minimum1 a = linearReducer a min

{- | Get the length of the sequence.

===== Examples

>>> reduceList length [1,3,2]
3

@since 1.0.0
-}
{-# INLINE length #-}
length :: forall (a :: Type). Reducer a Int
length = linearReducer 0 (\acc _ -> acc + 1)

{- | Compare length of the sequence against a constant. It short-circuits upon reaching 'GT'
thus terminates even on infinite sequences.

===== Examples

>>> reduceList (compareLength 0) []
EQ

>>> reduceList (compareLength 1) []
LT

>>> reduceList (compareLength 1) ['a']
EQ

>>> reduceList (compareLength 1) ['a','b']
GT

>>> reduceList (compareLength 100) [0..]
GT

@since 1.0.0
-}
{-# INLINE compareLength #-}
compareLength :: forall (a :: Type). Int -> Reducer a Ordering
compareLength n =
  Reducer
    { state = (compare 0 n, 0)
    , finalize = \(r, _) -> r
    , step = \(_, s) _ ->
        case compare (s + 1) n of
          LT -> Continue (LT, s + 1)
          EQ -> Continue (EQ, s + 1)
          GT -> Reduced (GT, s + 1)
    }

{- | Get the mean (average) of the elements.

===== Examples

>>> reduceList mean [1..5]
Just 3.0

>>> reduceList mean []
Nothing

@since 1.0.0
-}
{-# INLINE mean #-}
mean :: forall (a :: Type). Fractional a => Reducer a (Maybe a)
mean =
  Reducer
    { state = (0 :: Int, 0 :: a)
    , finalize = \(len, sum) -> if len == 0 then Nothing else Just (sum / fromRational (fromIntegral len))
    , step = \(!len, !sum) a -> Continue (len + 1, sum + a)
    }

{- | Get the mean (average) of non-empty sequence of elements.

===== Examples

>>> reduceNonEmpty1 mean1 (1:|[2..5])
3.0

@since 1.0.0
-}
{-# INLINE mean1 #-}
mean1 :: forall (a :: Type). Fractional a => a -> Reducer a a
mean1 a =
  Reducer
    { state = (1 :: Int, a)
    , finalize = \(len, sum) -> sum / fromRational (fromIntegral len)
    , step = \(!len, !sum) a -> Continue (len + 1, sum + a)
    }

{- | Get the variance (population) of the elements.

===== Examples

>>> reduceList variance [1,5,3,9,2]
Just 8.0

>>> reduceList variance []
Nothing

@since 1.0.0
-}
{-# INLINE variance #-}
variance :: forall (a :: Type). Fractional a => Reducer a (Maybe a)
variance =
  Reducer
    { state = (0, 0, 0 :: Int)
    , finalize = \(total, totalSq, len) ->
        if len == 0
          then Nothing
          else
            let m = total / fromRational (fromIntegral len)
             in Just ((totalSq / fromRational (fromIntegral len)) - (m * m))
    , step = \(!total, !totalSq, !n) a -> Continue (total + a, totalSq + (a * a), n + 1)
    }

{- | Get the variance (population) of non-empty sequence of elements.

===== Examples

>>> reduceNonEmpty1 variance1 (1 :| [5,3,9,2])
8.0

@since 1.0.0
-}
{-# INLINE variance1 #-}
variance1 :: forall (a :: Type). Fractional a => a -> Reducer a a
variance1 a =
  Reducer
    { state = (a, a * a, 1 :: Int)
    , finalize = \(total, totalSq, len) ->
        let m = total / fromRational (fromIntegral len)
         in (totalSq / fromRational (fromIntegral len)) - (m * m)
    , step = \(!total, !totalSq, !n) a -> Continue (total + a, totalSq + (a * a), n + 1)
    }

{- | Get the disjunction of 'Bool's.

===== Examples

>>> reduceList or [True,False,True]
True

>>> reduceList or [False,False]
False

'or' short-circuits on first 'True' value

>>> reduceRepeat or True
True

@since 1.0.0
-}
{-# INLINE or #-}
or :: Reducer Bool Bool
or = any id

{- | Get the conjunction of 'Bool's.

===== Examples

>>> reduceList and [True,False,True]
False

>>> reduceList and [True,True]
True

'and' short-circuits on first 'False' value

>>> reduceRepeat and False
False

@since 1.0.0
-}
{-# INLINE and #-}
and :: Reducer Bool Bool
and = all id

{- | Get the conjunction of the results of passed predicates.

===== Examples

>>> reduceList (all even) [4,2,6]
True

>>> reduceList (all even) [1,2,3]
False

'and' short-circuits on first 'False' value

>>> reduceList (all even) [1..]
False

@since 1.0.0
-}
{-# INLINE all #-}
all :: forall (a :: Type). (a -> Bool) -> Reducer a Bool
all pred =
  Reducer
    { state = True
    , finalize = id
    , step = \_ a ->
        if pred a
          then Continue True
          else Reduced False
    }

{- | Get the conjunction of the results of passed predicates.

===== Examples

>>> reduceList (any even) [1,2,3]
True

>>> reduceList (any even) [1,3]
False

'any' short-circuits on first 'True' value

>>> reduceRepeat (any even) 2
True

@since 1.0.0
-}
{-# INLINE any #-}
any :: forall (a :: Type). (a -> Bool) -> Reducer a Bool
any pred =
  Reducer
    { state = False
    , finalize = id
    , step = \_ a ->
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
    { state = True
    , finalize = id
    , step = \_ _ -> Reduced False
    }

{- | Extract the first element if exists.

===== Examples

>>> reduceList head [1,2,3]
Just 1

>>> reduceList head [1..]
Just 1

>>> reduceList head []
Nothing

@since 1.0.0
-}
{-# INLINE head #-}
head :: forall (a :: Type). Reducer a (Maybe a)
head =
  Reducer
    { state = Nothing
    , finalize = id
    , step = \_ a -> Reduced (Just a)
    }

{- | Extract the first element from non-empty sequence.

===== Examples

>>> reduceNonEmpty1 head1 (2 :| [1,4,3])
2

@since 1.0.0
-}
{-# INLINE head1 #-}
head1 :: forall (a :: Type). a -> Reducer a a
head1 = pure

{- | Extract the last element if exists.

===== Examples

>>> reduceList last [1,2,3]
Just 3

>>> reduceList last []
Nothing

@since 1.0.0
-}
{-# INLINE last #-}
last :: forall (a :: Type). Reducer a (Maybe a)
last = linearReducer Nothing (const Just)

{- | Extract the last element from non-empty sequence.

===== Examples

>>> reduceNonEmpty1 last1 (2 :| [1,4,3])
3

@since 1.0.0
-}
{-# INLINE last1 #-}
last1 :: forall (a :: Type). a -> Reducer a a
last1 a = linearReducer a (\_ a -> a)

{- | Get the first element for which the passed predicate returns 'True', if exists.

===== Examples

>>> reduceList (find even) [1,2,3]
Just 2

>>> reduceList (find even) [1,3,5]
Nothing

@since 1.0.0
-}
{-# INLINE find #-}
find :: forall (a :: Type). (a -> Bool) -> Reducer a (Maybe a)
find pred =
  Reducer
    { state = Nothing
    , finalize = id
    , step = \_ a ->
        if pred a
          then Reduced (Just a)
          else Continue Nothing
    }

{- | Check if any elements is equal (using '==') to the one passed.

===== Examples

>>> reduceList (elem 2) [1,2,3]
True

>>> reduceList (elem 2) [1,3,5]
False

@since 1.0.0
-}
{-# INLINE elem #-}
elem :: forall (a :: Type). Eq a => a -> Reducer a Bool
elem a = elemBy (a ==)

{- | Check if for any elements the passed predicate returns 'True'.

===== Examples

>>> reduceList (elemBy even) [1,2,3]
True

>>> reduceList (elemBy even) [1,3,5]
False

@since 1.0.0
-}
{-# INLINE elemBy #-}
elemBy :: forall (a :: Type). (a -> Bool) -> Reducer a Bool
elemBy pred =
  Reducer
    { state = False
    , finalize = id
    , step = \_ a ->
        if pred a
          then Reduced True
          else Continue False
    }

{- | Apply '<>' to all elements in the sequence.

===== Examples

>>> reduceList sconcat ["hello"," ","world"]
Just "hello world"

>>> reduceList sconcat []
Nothing

@since 1.0.0
-}
{-# INLINE sconcat #-}
sconcat :: forall (a :: Type). Semigroup a => Reducer a (Maybe a)
sconcat = linearReducer Nothing $ \acc a -> case acc of
  Nothing -> Just a
  Just acc -> Just (acc <> a)

{- | Apply '<>' to all elements in the sequence.

===== Examples

>>> reduceList (sconcat1 "hello") [" ","world"]
"hello world"

>>> reduceList (sconcat1 "hello") []
"hello"

@since 1.0.0
-}
{-# INLINE sconcat1 #-}
sconcat1 :: forall (a :: Type). Semigroup a => a -> Reducer a a
sconcat1 a = linearReducer a (<>)

{- | Apply '<>' to all elements in the sequence.

===== Examples

>>> reduceList mconcat ["hello"," ","world"]
"hello world"

>>> reduceList mconcat ([] :: [String])
""

@since 1.0.0
-}
{-# INLINE mconcat #-}
mconcat :: forall (a :: Type). Monoid a => Reducer a a
mconcat = linearReducer mempty (<>)

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
    { state = []
    , finalize = List.reverse
    , step = \as a -> Continue (a : as)
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
    { state = Nothing
    , finalize = fmap NonEmpty.reverse
    , step = \as a ->
        case as of
          Nothing -> Continue (Just (a :| []))
          Just (b :| bs) -> Continue (Just (a :| b : bs))
    }

{- | Collect all elements from non-empty sequence into a non-empty list.

===== Examples

>>> reduceNonEmpty1 intoNonEmpty1 (1 :| [2,3])
1 :| [2,3]

@since 1.0.0
-}
{-# INLINE intoNonEmpty1 #-}
intoNonEmpty1 :: forall (a :: Type). a -> Reducer a (NonEmpty a)
intoNonEmpty1 a =
  Reducer
    { state = []
    , finalize = \acc -> a :| List.reverse acc
    , step = \as a -> Continue (a : as)
    }

{- | Discard the rest of the elements. It works on infinite sequences.

It seems pointless but can be useful when a transducer takes multiple reducers
and you care only about some of them.

===== Examples

>>> reduceList discard [0..]
()

@since 1.0.0
-}
{-# INLINE discard #-}
discard :: forall (a :: Type). Reducer a ()
discard =
  Reducer
    { state = ()
    , finalize = const ()
    , step = \_ _ -> Reduced ()
    }

-- * Transducers

{- | Transducer composition operator.

Transducers are executed left to right which may get quite counter-intuitive to read comparing
to @base@ list operators so this operator can be used to clearly indicate the flow of elements.

Note that this is just like '$' but specialized to t'Reducer's.

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
    { state = state
    , finalize = finalize
    , step = \s b -> step s (f b)
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
    { state = (n, state)
    , finalize = \(_, state) -> finalize state
    , step = \(currN, s) a ->
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
    { state = state
    , finalize = finalize
    , step = \s a ->
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
    { state = (n, state)
    , finalize = \(_, state) -> finalize state
    , step = \(currN, s) a ->
        if currN <= 0
          then fmap (currN,) (step s a)
          else Continue (currN - 1, s)
    }

{- | Keep dropping elements from the sequence as long as the passed predicate returns 'True'.
Note that upon first value that fails the predicate the whole remaining sequence will be forwarded
even if some further value would pass it.

===== Examples

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
    { state = (False, state)
    , finalize = \(_, state) -> finalize state
    , step = \(finishedDropping, s) a ->
        if finishedDropping
          then fmap (True,) (step s a)
          else
            if pred a
              then Continue (False, s)
              else fmap (True,) (step s a)
    }

{- | Keep only elements for which the passed predicate returns 'True'.

===== Examples

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
    { state = state
    , finalize = finalize
    , step = \s a ->
        if pred a
          then step s a
          else Continue s
    }

{- | Keep only elements that are mapped to 'Just'.

===== Examples

>>> import Text.Read (readMaybe)
>>> reduceList (mapMaybe (readMaybe @Int) |> intoList) ["1","foo","2","bar"]
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
    { state = state
    , finalize = finalize
    , step = \s a ->
        case f a of
          Nothing -> Continue s
          Just a' -> step s a'
    }

{- | Keep only 'Just' elements.

===== Examples

>>> reduceList (catMaybes |> intoList) [Just 1,Nothing,Just 2,Just 3,Nothing]
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
    { state = state
    , finalize = finalize
    , step = \s -> \case
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
    { state = (False, state)
    , finalize = \(_, state) -> finalize state
    , step = \(acc, s) a ->
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

{- | Flatten a sequence of @NonEmpty a@ into a sequence of @a@.

===== Examples

>>> reduceList (concatNonEmpty |> intoList) [1:|[2],3:|[4,5]]
[1,2,3,4,5]

@since 1.0.0
-}
{-# INLINE concatNonEmpty #-}
concatNonEmpty ::
  forall (a :: Type) (r :: Type).
  Reducer a r ->
  Reducer (NonEmpty a) r
concatNonEmpty reducer = map NonEmpty.toList |> concatList reducer

{- | Remove duplicate elements and keep only the first occurrence that are equal.

Note that it has the same performance problems as 'List.nub'.

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

{- | Remove duplicate elements and keep only the first occurrence that are equal
accordingly to passed predicate.

Note that it has the same performance problems as 'List.nubBy'.

===== Examples

Get elements that are not equal to each other modulo @5@.

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
    { state = ([], state)
    , finalize = \(_, state) -> finalize state
    , step = \(seen, s) a ->
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
    { state = (Nothing, state)
    , finalize = \(r, s) -> case r of
        Nothing -> Nothing
        Just fst -> Just (fst, finalize s)
    , step = \(mr, s) a -> case mr of
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
    { state = (Nothing, state)
    , finalize = \(r, s) -> case r of
        Nothing -> Nothing
        Just lst -> Just (finalize s, lst)
    , step = \(mr, s) a -> case mr of
        Nothing -> Continue (Just a, s)
        Just lst -> fmap (Just a,) (step s lst)
    }

{- | Apply the passed function with every element of the input and add it to the input stream.

===== Examples

Running sums

>>> reduceList (scan (+) 0 |> intoList) [1..5]
[(0,1),(1,2),(3,3),(6,4),(10,5)]

@since 1.0.0
-}
{-# INLINE scan #-}
scan ::
  forall (a :: Type) (r :: Type) (acc :: Type).
  (acc -> a -> acc) ->
  acc ->
  Reducer (acc, a) r ->
  Reducer a r
scan scanStep scanInit (Reducer state finalize step) =
  Reducer
    { state = (scanInit, state)
    , finalize = \(_, state) -> finalize state
    , step = \(scanAcc, s) a ->
        case step s (scanAcc, a) of
          Reduced s' -> Reduced (scanAcc, s')
          Continue s' -> Continue (scanAcc `seq` scanStep scanAcc a, s')
    }

{- | Associate index with each element, starting at zero.

===== Examples

>>> reduceList (enumerate |> intoList) ["foo","bar"]
[(0,"foo"),(1,"bar")]

@since 1.0.0
-}
{-# INLINE enumerate #-}
enumerate ::
  forall (a :: Type) (r :: Type).
  Reducer (Int, a) r ->
  Reducer a r
enumerate = scan (\acc _ -> acc + 1) 0

{- | Run two reducers on the same input.

===== Examples

>>> reduceList (zip sum product) [1,2,3,4]
(10,24)

@since 1.0.0
-}
{-# INLINE zip #-}
zip ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
zip = liftA2 (,)

{- | Run first reducer on first element of the tuple and second reducer on the second.

===== Examples

Collect @fst@ into a list and sums the @snd@

>>> reduceList (zipTuple intoList sum) [(1,2),(3,4)]
([1,3],6)

@since 1.0.0
-}
{-# INLINE zipTuple #-}
zipTuple ::
  forall (a1 :: Type) (a2 :: Type) (r1 :: Type) (r2 :: Type).
  Reducer a1 r1 ->
  Reducer a2 r2 ->
  Reducer (a1, a2) (r1, r2)
zipTuple (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (ZipFinishedNone, state1, state2)
    , finalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
    , step = \s (a1, a2) -> zipStepBoth step1 step2 s a1 a2
    }

{- | Run first reducer on @Left@ elements and second reducer on @Right@s.

===== Examples

Collect @fst@ into a list and sums the @snd@

>>> reduceList (zipEither intoList sum) [Left 1,Right 2,Left 3,Left 4,Right 5]
([1,3,4],7)

@since 1.0.0
-}
{-# INLINE zipEither #-}
zipEither ::
  forall (a1 :: Type) (a2 :: Type) (r1 :: Type) (r2 :: Type).
  Reducer a1 r1 ->
  Reducer a2 r2 ->
  Reducer (Either a1 a2) (r1, r2)
zipEither (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (ZipFinishedNone, state1, state2)
    , finalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
    , step = \s a ->
        case a of
          Left a1 -> zipStepLeft step1 s a1
          Right a2 -> zipStepRight step2 s a2
    }

{- | Run first reducer if on elements that satisfy the predicate and second otherwise

===== Examples

>>> reduceList (partition even sum intoList) [1,2,3,4,5]
(6,[1,3,5])

@since 1.0.0
-}
{-# INLINE partition #-}
partition ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  (a -> Bool) ->
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
partition f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (ZipFinishedNone, state1, state2)
    , finalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
    , step = \s a -> if f a then zipStepLeft step1 s a else zipStepRight step2 s a
    }

{- | Run first reducer on the longest prefix that satisfies the predicate, then second.

===== Examples

>>> reduceList (span even intoList intoList) [2,6,4,1,2,3,4]
([2,6,4],[1,2,3,4])

@since 1.0.0
-}
{-# INLINE span #-}
span ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  (a -> Bool) ->
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
span f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (True, (ZipFinishedNone, state1, state2))
    , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
    , step = \(onFirst, s) a ->
        if onFirst && f a
          then fmap (True,) (zipStepLeft step1 s a)
          else fmap (False,) (zipStepRight step2 s a)
    }

{- | Run first reducer on the longest prefix that does not satisfy the predicate, then second.

@'break' p = 'span' (not . p)@

===== Examples

>>> reduceList (break odd intoList intoList) [2,6,4,1,2,3,4]
([2,6,4],[1,2,3,4])

@since 1.0.0
-}
{-# INLINE break #-}
break ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  (a -> Bool) ->
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
break f (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (True, (ZipFinishedNone, state1, state2))
    , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
    , step = \(onFirst, s) a ->
        if onFirst && not (f a)
          then fmap (True,) (zipStepLeft step1 s a)
          else fmap (False,) (zipStepRight step2 s a)
    }

{- | Run first reducer on the first @n@ elements,then second.

===== Examples

>>> reduceList (splitAt 3 intoList intoList) [2,6,4,1,2,3,4]
([2,6,4],[1,2,3,4])

@since 1.0.0
-}
{-# INLINE splitAt #-}
splitAt ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  Int ->
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
splitAt n (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (n, (ZipFinishedNone, state1, state2))
    , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
    , step = \(n, s) a ->
        if n > 0
          then fmap (n - 1,) (zipStepLeft step1 s a)
          else fmap (n,) (zipStepRight step2 s a)
    }

{- | Split sequence into two alternating sequences.

===== Examples

>>> reduceList (uninterleave intoList intoList) [1..5]
([1,3,5],[2,4])

@since 1.0.0
-}
{-# INLINE uninterleave #-}
uninterleave ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
uninterleave (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (True, (ZipFinishedNone, state1, state2))
    , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
    , step = \(first, s) a ->
        if first
          then fmap (False,) (zipStepLeft step1 s a)
          else fmap (True,) (zipStepRight step2 s a)
    }

{- | Split off every @n@th element into the first reducer and rest to the second

===== Examples

>>> reduceList (takeNth 3 intoList intoList) [1..10]
([1,5,9],[2,3,4,6,7,8,10])

@since 1.0.0
-}
{-# INLINE takeNth #-}
takeNth ::
  forall (a :: Type) (r1 :: Type) (r2 :: Type).
  Int ->
  Reducer a r1 ->
  Reducer a r2 ->
  Reducer a (r1, r2)
takeNth n (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  Reducer
    { state = (0, (ZipFinishedNone, state1, state2))
    , finalize = \(_, (_, s1, s2)) -> (finalize1 s1, finalize2 s2)
    , step = \(k, s) a ->
        if k <= 0
          then fmap (n,) (zipStepLeft step1 s a)
          else fmap (k - 1,) (zipStepRight step2 s a)
    }

-- ** Grouping

{- | Split the sequence into groups of consecutive equal elements.

===== Examples

>>> reduceList (group intoList |> intoList) [1,2,2,3,3,1]
[[1],[2,2],[3,3],[1]]

@since 1.0.0
-}
{-# INLINE group #-}
group ::
  forall (a :: Type) (p :: Type) (r :: Type).
  Eq a =>
  Reducer a p ->
  Reducer p r ->
  Reducer a r
group = groupBy (==)

{- | Split the sequence into groups of consecutive elements for which the passed predicate
returns 'True' when applied to the first element of the group and the current element.

===== Examples

Get non-decreasing sub-sequences

>>> reduceList (groupBy (\x y -> x <= y) intoList |> intoList) [3,4,5,1,2,6,0]
[[3,4,5],[1,2,6],[0]]

@since 1.0.0
-}
{-# INLINE groupBy #-}
groupBy ::
  forall (a :: Type) (p :: Type) (r :: Type).
  (a -> a -> Bool) ->
  Reducer a p ->
  Reducer p r ->
  Reducer a r
groupBy eq = groupByKey id eq

{- | Split the sequence into groups of consecutive elements that map to the same key.

===== Examples

>>> reduceList (groupOn even intoList |> intoList) [1,2,4,5,7,9,6,8]
[[1],[2,4],[5,7,9],[6,8]]

@since 1.0.0
-}
{-# INLINE groupOn #-}
groupOn ::
  forall (k :: Type) (a :: Type) (p :: Type) (r :: Type).
  Eq k =>
  (a -> k) ->
  Reducer a p ->
  Reducer p r ->
  Reducer a r
groupOn key = groupByKey key (\k a -> k == key a)

{- | Split the sequence into groups of consecutive elements.

===== Examples

Group elements that are at most 2 greater than the first element of the group.

>>> reduceList (groupByKey (+ 2) (\bound x -> x <= bound) intoList |> intoList) [1,2,3,4,5,10,11,12,13]
[[1,2,3],[4,5],[10,11,12],[13]]

@since 1.0.0
-}
{-# INLINE groupByKey #-}
groupByKey ::
  forall (k :: Type) (a :: Type) (p :: Type) (r :: Type).
  (a -> k) ->
  (k -> a -> Bool) ->
  Reducer a p ->
  Reducer p r ->
  Reducer a r
groupByKey key sameGroup (Reducer groupState groupFinalize groupStep) =
  groupByKeyWith key sameGroup (groupStep groupState) groupFinalize groupStep

{- | Like 'group' but takes non-empty reducer.

===== Examples

Run-length encoding

>>> reduceList (group1 (\c -> liftA2 (,) (head1 c) (feed1 c |> length)) |> intoList) "aaabccdddd"
[('a',3),('b',1),('c',2),('d',4)]

@since 1.0.0
-}
{-# INLINE group1 #-}
group1 ::
  forall (a :: Type) (p :: Type) (r :: Type).
  Eq a =>
  (a -> Reducer a p) ->
  Reducer p r ->
  Reducer a r
group1 = groupBy1 (==)

{- | Like 'groupBy' but takes non-empty reducer.

===== Examples

>>> reduceList (groupBy1 (\a b -> (a `mod` 3) == (b `mod` 3)) maximum1 |> intoList) [1,4,5,9,6]
[4,5,9]

@since 1.0.0
-}
{-# INLINE groupBy1 #-}
groupBy1 ::
  forall (a :: Type) (p :: Type) (r :: Type).
  (a -> a -> Bool) ->
  (a -> Reducer a p) ->
  Reducer p r ->
  Reducer a r
groupBy1 eq = groupByKey1 id eq

{- | Like 'groupOn' but takes non-empty reducer.

===== Examples

>>> reduceList (groupOn1 even head1 |> intoList) [1,2,4,5,7,9,6,8]
[1,2,5,6]

@since 1.0.0
-}
{-# INLINE groupOn1 #-}
groupOn1 ::
  forall (k :: Type) (a :: Type) (p :: Type) (r :: Type).
  Eq k =>
  (a -> k) ->
  (a -> Reducer a p) ->
  Reducer p r ->
  Reducer a r
groupOn1 key = groupByKey1 key (\k a -> k == key a)

{- | Like 'groupByKey' but takes non-empty reducer.

@since 1.0.0
-}
{-# INLINE groupByKey1 #-}
groupByKey1 ::
  forall (k :: Type) (a :: Type) (p :: Type) (r :: Type).
  (a -> k) ->
  (k -> a -> Bool) ->
  (a -> Reducer a p) ->
  Reducer p r ->
  Reducer a r
groupByKey1 key sameGroup mkGroupReducer =
  groupByKeyWith
    key
    sameGroup
    (\a -> Continue (mkGroupReducer a))
    (\(Reducer groupState groupFinalize _) -> groupFinalize groupState)
    (\groupReducer a -> feedReduced a groupReducer)
  where
    {-# INLINE feedReduced #-}
    feedReduced :: forall (a :: Type) (r :: Type). a -> Reducer a r -> Reduced (Reducer a r)
    feedReduced a (Reducer state finalize step) =
      case step state a of
        Continue state' -> Continue (Reducer state' finalize step)
        Reduced state' -> Reduced (Reducer state' finalize step)

{- | Building block for @group*@ functions

@since 1.0.0
-}
{-# INLINE groupByKeyWith #-}
groupByKeyWith ::
  forall (k :: Type) (a :: Type) (g :: Type) (p :: Type) (r :: Type).
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
  Reducer p r ->
  Reducer a r
groupByKeyWith key sameGroup groupSeed groupFinalize groupStep (Reducer state finalize step) =
  Reducer
    { state = (Nothing, state)
    , finalize = \(current, s) ->
        case current of
          Nothing -> finalize s
          Just (_, g) -> finalize (unReduced (step s (groupFinalize (unReduced g))))
    , step = \(current, s) a ->
        case current of
          Nothing -> Continue (Just (key a, groupSeed a), s)
          Just (k, g) ->
            if sameGroup k a
              then case g of
                Reduced _ -> Continue (current, s)
                Continue gs -> Continue (Just (k, groupStep gs a), s)
              else case step s (groupFinalize (unReduced g)) of
                Reduced s' -> Reduced (Nothing, s')
                Continue s' -> Continue (Just (key a, groupSeed a), s')
    }

{- | Feed one element to the reducer before the rest of the sequence.

===== Examples

>>> reduceList (feed1 1 |> intoList) [2,3]
[1,2,3]

@since 1.0.0
-}
{-# INLINE feed1 #-}
feed1 :: forall (a :: Type) (r :: Type). a -> Reducer a r -> Reducer a r
feed1 a (Reducer state finalize step) =
  case step state a of
    Continue state' -> Reducer state' finalize step
    Reduced state' -> Reducer state' finalize (\s _ -> Reduced s)

-- * Building Blocks

{- | Construct a reducer that consumes whole input.

===== Examples

@sum = linearReducer 0 (+)@

>>> reduceList (linearReducer 0 (+)) [1,2,3]
6

@since 1.0.0
-}
{-# INLINE linearReducer #-}
linearReducer :: forall (a :: Type) (r :: Type). r -> (r -> a -> r) -> Reducer a r
linearReducer acc f =
  Reducer
    { state = acc
    , finalize = id
    , step = \r !a -> let !r' = f r a in Continue r'
    }

-- ** Zipping

{- | Internal state of @zip*@ functions.

It is used to track if some reducer already short circuted to not call step function on it anymore
but the overall combinator must still return @Continue@ to give the opportunity to both reducers
to finish.

@since 1.0.0
-}
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

-- | @since 1.0.0
{-# INLINE zipStepLeft #-}
zipStepLeft ::
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
      Reduced s1' -> Continue (ZipFinishedBoth, s1', s2)
    ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)

-- | @since 1.0.0
{-# INLINE zipStepRight #-}
zipStepRight ::
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
      Reduced s2' -> Continue (ZipFinishedBoth, s1, s2')
    ZipFinished2 -> Continue (ZipFinished2, s1, s2)
    ZipFinishedBoth -> Reduced (ZipFinishedBoth, s1, s2)

-- | @since 1.0.0
{-# INLINE zipStepBoth #-}
zipStepBoth ::
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

-- ** Doctest Imports (ignore)

{- $setup
>>> import Prelude (String, even, mod, odd)
-}
