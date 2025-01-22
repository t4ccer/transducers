-- | A tiny set of utilities to construct reducers from @Sequence@s or reduce into @Sequence@s
module Data.Transducer.Sequence (
  -- * Runners
  reduceSeq,

  -- * Reducers
  intoSeq,

  -- * Transducers
  concatSeq,
) where

import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Sequence

import Data.Transducer (
  Reduced (Continue, Reduced),
  Reducer (reducerFinalize, reducerInitAcc, reducerInitState, reducerStep),
  simpleStatelessReducer,
  statelessTransducer,
 )

{- $setup
>>> import Data.Transducer ((|>), sum, take, reduceIterate, reduceList)
>>> import GHC.Num (Num ((+)))
-}

{- | Run a reducer on all @Seq@ elements.

===== Examples

>>> reduceSeq sum (Sequence.fromList [1, 2, 3])
6

@since 1.0.0
-}
reduceSeq :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> Seq a -> r
reduceSeq reducer seq =
  let (r', s') = go (reducerInitState reducer) (reducerInitAcc reducer) seq
   in reducerFinalize reducer s' r'
  where
    go s r Sequence.Empty = (r, s)
    go s r (a Sequence.:<| as) =
      case reducerStep reducer s r a of
        (Reduced r', s') -> (r', s')
        (Continue r', s') -> go s' r' as

{- | Collect all elements into a @Seq@.

===== Examples

>>> reduceIterate (take 10 |> intoSeq) (+3) 42
fromList [42,45,48,51,54,57,60,63,66,69]

@since 1.0.0
-}
intoSeq :: forall (a :: Type). Reducer () a (Seq a)
intoSeq = simpleStatelessReducer Sequence.empty (Sequence.|>)

{- | Flatten a sequence of @Seq a@ into a sequence of @a@.

===== Examples

>>> reduceList (concatSeq |> intoSeq) [Sequence.fromList [1,2], Sequence.fromList [3,4,5]]
fromList [1,2,3,4,5]

@since 1.0.0
-}
concatSeq ::
  forall (a :: Type) (s :: Type) (r :: Type).
  Reducer s a r ->
  Reducer s (Seq a) r
concatSeq reducer = statelessTransducer reducer step
  where
    step :: s -> r -> Seq a -> (Reduced r, s)
    step s r = \case
      Sequence.Empty -> (Continue r, s)
      (a Sequence.:<| as) -> case reducerStep reducer s r a of
        (Reduced r', s') -> (Reduced r', s')
        (Continue r', s') -> step s' r' as
