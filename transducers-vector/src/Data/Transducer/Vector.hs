-- | A tiny set of utilities to construct reducers from @Vector@s or reduce into @Vector@s
module Data.Transducer.Vector (
  -- * Runners
  reduceVector,
  reduceGenericVector,
  reduceVectorStream,

  -- * Reducers
  intoVector,
  intoGenericVector,
  intoVectorStream,

  -- * Transducers
  concatVector,
  concatGenericVector,
) where

import Control.Applicative (Applicative (pure))
import Control.Monad (Functor (fmap), Monad)
import Data.Kind (Type)
import Data.List (reverse)
import Data.Ord ((>=))
import Data.Vector (Vector)
import Data.Vector.Fusion.Bundle qualified as Bundle
import Data.Vector.Fusion.Stream.Monadic (Step (Done, Skip, Yield), Stream (Stream))
import Data.Vector.Fusion.Stream.Monadic qualified as Stream
import Data.Vector.Generic qualified as Generic
import GHC.Num (Num ((+)))

import Data.Transducer (
  Reduced (Continue, Reduced),
  Reducer (Reducer, reducerFinalize, reducerInitState, reducerStep),
 )

{- $setup
>>> import Data.Vector qualified as Vector
>>> import Data.Transducer ((|>), sum, take, reduceIterate, reduceList)
>>> import Data.Functor.Identity (Identity)
-}

{- | Run a reducer on all @Vector@ elements.

===== Examples

>>> reduceVector sum (Vector.fromList [1, 2, 3])
6

@since 1.0.0
-}
{-# INLINE reduceVector #-}
reduceVector :: forall (a :: Type) (r :: Type). Reducer a r -> Vector a -> r
reduceVector = reduceGenericVector

{- | Like 'reduceVector' but works over generic vector interface.

@since 1.0.0
-}
{-# INLINE reduceGenericVector #-}
reduceGenericVector ::
  forall (v :: Type -> Type) (a :: Type) (r :: Type).
  Generic.Vector v a =>
  Reducer a r ->
  v a ->
  r
reduceGenericVector (Reducer state finalize step) v = finalize (go state 0 (Generic.length v) v)
  where
    go s i len v =
      if i >= len
        then s
        else case step s (Generic.unsafeIndex v i) of
          Reduced s' -> s'
          Continue s' -> go s' (i + 1) len v

{- | Run a reducer on all @Stream@ elements.

===== Examples

>>> reduceVectorStream sum (Stream.fromList [1,2,3])
6

@since 1.0.0
-}
{-# INLINE reduceVectorStream #-}
reduceVectorStream ::
  forall (m :: Type -> Type) (a :: Type) (r :: Type).
  Monad m =>
  Reducer a r ->
  Stream m a ->
  m r
reduceVectorStream (Reducer state finalize step) (Stream streamStep ss) =
  fmap finalize (go state ss)
  where
    go s ss = do
      streamR <- streamStep ss
      case streamR of
        Yield a streamR' -> case step s a of
          Reduced s' -> pure s'
          Continue s' -> go s' streamR'
        Skip streamR' -> go s streamR'
        Done -> pure s

{- | Collect all elements into a @Vector@.

===== Examples

>>> reduceIterate (take 10 |> intoVector) (+3) 42
[42,45,48,51,54,57,60,63,66,69]

@since 1.0.0
-}
{-# INLINE intoVector #-}
intoVector :: forall (a :: Type). Reducer a (Vector a)
intoVector = intoGenericVector

{- | Like 'intoVector' but works over generic vector interface.

@since 1.0.0
-}
{-# INLINE intoGenericVector #-}
intoGenericVector ::
  forall (a :: Type) (v :: Type -> Type). Generic.Vector v a => Reducer a (v a)
intoGenericVector =
  Reducer
    { reducerInitState = (0, [])
    , reducerFinalize = \(n, as) -> Generic.unstreamR (Bundle.fromListN n as)
    , reducerStep = \(!n, as) a -> Continue (n + 1, a : as)
    }

{- | Collect all elements into a @Stream@.

===== Examples

>>> Stream.toList (reduceList (intoVectorStream @Identity) [1,2,3])
Identity [1,2,3]

@since 1.0.0
-}
{-# INLINE intoVectorStream #-}
intoVectorStream ::
  forall (m :: Type -> Type) (a :: Type). Monad m => Reducer a (Stream m a)
intoVectorStream =
  Reducer
    { reducerInitState = []
    , reducerFinalize = \as -> Stream.fromList (reverse as)
    , reducerStep = \as a -> Continue (a : as)
    }

{- | Flatten a sequence of @Vector a@ into a sequence of @a@.

===== Examples

>>> reduceList (concatVector |> intoVector) [Vector.fromList [1,2], Vector.fromList [3,4,5]]
[1,2,3,4,5]

@since 1.0.0
-}
{-# INLINE concatVector #-}
concatVector ::
  forall (a :: Type) (r :: Type). Reducer a r -> Reducer (Vector a) r
concatVector = concatGenericVector

{- | Like 'concatVector' but works over generic vector interface.

@since 1.0.0
-}
{-# INLINE concatGenericVector #-}
concatGenericVector ::
  forall (v :: Type -> Type) (a :: Type) (r :: Type).
  Generic.Vector v a =>
  Reducer a r ->
  Reducer (v a) r
concatGenericVector (Reducer state finalize step) =
  Reducer
    { reducerInitState = state
    , reducerFinalize = finalize
    , reducerStep = step' 0
    }
  where
    step' i s v =
      if i >= Generic.length v
        then Continue s
        else case step s (Generic.unsafeIndex v i) of
          Reduced s' -> Reduced s'
          Continue s' -> step' (i + 1) s' v
