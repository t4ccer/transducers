-- | A tiny set of utilities to construct reducers from @Sequence@s or reduce into @Sequence@s
module Data.Transducer.Sequence (
  -- * Runners
  reduceSeq,

  -- * Reducers
  intoSeq,

  -- * Transducers
  concatSeq,
) where

import Data.Foldable (foldr)
import Data.Function (($))
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Sequence

import Data.Transducer (
  Initial (Zero),
  IsInitial,
  Reduced (Continue, Reduced),
  Reducer (Reducer, finalize, state, step),
  initializeReducer,
  linearReducer,
  reduceFoldr,
 )

-- $setup
-- >>> import Data.Transducer ((|>), sum, take, reduceIterate, reduceList)
-- >>> import GHC.Num (Num ((+)))

-- | Run a reducer on all @Seq@ elements.
--
-- ===== Examples
--
-- >>> reduceSeq sum (Sequence.fromList [1, 2, 3])
-- 6
--
-- @since 1.0.0
{-# INLINE reduceSeq #-}
reduceSeq :: forall (a :: Type) (r :: Type). Reducer 'Zero a r -> Seq a -> r
reduceSeq = reduceFoldr

-- | Collect all elements into a @Seq@.
--
-- ===== Examples
--
-- >>> reduceIterate @'Zero (take 10 |> intoSeq) (+3) 42
-- fromList [42,45,48,51,54,57,60,63,66,69]
--
-- @since 1.0.0
{-# INLINE intoSeq #-}
intoSeq :: forall (init :: Initial) (a :: Type). (IsInitial init) => Reducer init a (Seq a)
intoSeq = linearReducer Sequence.empty (Sequence.|>)

-- | Flatten a sequence of @Seq a@ into a sequence of @a@.
--
-- ===== Examples
--
-- >>> reduceList (concatSeq |> intoSeq) [Sequence.fromList [1,2], Sequence.fromList [3,4,5]]
-- fromList [1,2,3,4,5]
--
-- @since 1.0.0
{-# INLINE concatSeq #-}
concatSeq :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => Reducer 'Zero a r -> Reducer init (Seq a) r
concatSeq (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = state
      , finalize = finalize
      , step = \s seq -> foldr go Continue seq s
      }
  where
    go a k s = case step s a of
      Reduced s' -> Reduced s'
      Continue s' -> k s'
