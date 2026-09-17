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
  Reducer (Reducer, reducerFinalize, reducerInitState, reducerStep),
  mkLinearReducer,
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
reduceSeq :: forall (a :: Type) (r :: Type). Reducer a r -> Seq a -> r
reduceSeq (Reducer state finalize step) seq = finalize (go state seq)
  where
    go s Sequence.Empty = s
    go s (a Sequence.:<| as) =
      case step s a of
        Reduced s' -> s'
        Continue s' -> go s' as

{- | Collect all elements into a @Seq@.

===== Examples

>>> reduceIterate (take 10 |> intoSeq) (+3) 42
fromList [42,45,48,51,54,57,60,63,66,69]

@since 1.0.0
-}
intoSeq :: forall (a :: Type). Reducer a (Seq a)
intoSeq = mkLinearReducer Sequence.empty (Sequence.|>)

{- | Flatten a sequence of @Seq a@ into a sequence of @a@.

===== Examples

>>> reduceList (concatSeq |> intoSeq) [Sequence.fromList [1,2], Sequence.fromList [3,4,5]]
fromList [1,2,3,4,5]

@since 1.0.0
-}
concatSeq :: forall (a :: Type) (r :: Type). Reducer a r -> Reducer (Seq a) r
concatSeq (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = step'
    }
  where
    step' s = \case
      Sequence.Empty -> Continue s
      (a Sequence.:<| as) -> case step s a of
        Reduced s' -> Reduced s'
        Continue s' -> step' s' as
