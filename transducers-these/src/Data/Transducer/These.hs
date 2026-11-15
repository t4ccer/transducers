-- | A tiny set of utilities for 'These'.
module Data.Transducer.These (
  -- * Transducers
  zipThese,
) where

import Data.Function (($))
import Data.Kind (Type)
import Data.These (These (That, These, This))

import Data.Transducer (
  Initial (Zero),
  IsInitial,
  Reducer (Reducer, finalize, state, step),
  ZipFinished (ZipFinishedNone),
  initializeReducer,
  zipStepBoth,
  zipStepLeft,
  zipStepRight,
 )

-- $setup
-- >>> import Data.Transducer ((|>), intoList, reduceList, discard)

-- | Run first reducer on 'This' elements, second reducer on 'That', and both on 'These'.
--
-- ===== Examples
--
-- >>> reduceList (zipThese intoList intoList) [This 1, That 2, These 3 4, This 5]
-- ([1,3,5],[2,4])
--
-- @since 1.0.0
{-# INLINE zipThese #-}
zipThese ::
  forall (init :: Initial) (a1 :: Type) (a2 :: Type) (r1 :: Type) (r2 :: Type).
  (IsInitial init) =>
  Reducer 'Zero a1 r1 ->
  Reducer 'Zero a2 r2 ->
  Reducer init (These a1 a2) (r1, r2)
zipThese (Reducer state1 finalize1 step1) (Reducer state2 finalize2 step2) =
  initializeReducer $
    Reducer
      { state = (ZipFinishedNone, state1, state2)
      , finalize = \(_, s1, s2) -> (finalize1 s1, finalize2 s2)
      , step = \s a ->
          case a of
            This a1 -> zipStepLeft step1 s a1
            That a2 -> zipStepRight step2 s a2
            These a1 a2 -> zipStepBoth step1 step2 s a1 a2
      }
