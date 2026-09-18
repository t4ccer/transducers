{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
Internal implementation utilities.
They are exposed because not allowing to look under the hood is harmful for ones ability to innovate, however, they are NOT under PVP versioning.
-}
module Data.Transducer.Internal where

import Control.Monad (Functor (fmap))
import Data.Kind (Type)

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

{- | Internal state of @zipReducers*@ functions.

It is used to track if some reducer already short circuted to not call step function on it anymore
but the overall combinator must still return @Continue@ to give the opportunity to both reducers
to finish.
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
