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
import Control.Monad (Monad)
import Data.Function (const, id, ($))
import Data.Int (Int)
import Data.Kind (Type)
import Data.Ord ((>=))
import Data.Vector (Vector)
import Data.Vector.Fusion.Bundle.Monadic qualified as Bundle
import Data.Vector.Fusion.Bundle.Size (Size (Unknown))
import Data.Vector.Fusion.Stream.Monadic (Step (Done, Skip, Yield), Stream (Stream))
import Data.Vector.Fusion.Stream.Monadic qualified as Stream
import Data.Vector.Fusion.Util (Id)
import Data.Vector.Generic qualified as Generic
import GHC.Num (Num ((+)))

import Data.Transducer (
  Reduced (Continue, Reduced),
  Reducer (Reducer, reducerFinalize, reducerInitAcc, reducerInitState, reducerStep),
  statelessTransducer,
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
{-# INLINEABLE reduceVector #-}
reduceVector :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> Vector a -> r
reduceVector = reduceGenericVector

{- | Like 'reduceVector' but works over generic vector interface.

@since 1.0.0
-}
{-# SPECIALIZE reduceGenericVector :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> Vector a -> r #-}
{-# INLINEABLE reduceGenericVector #-}
reduceGenericVector ::
  forall (v :: Type -> Type) (a :: Type) (r :: Type) (s :: Type).
  Generic.Vector v a =>
  Reducer s a r ->
  v a ->
  r
reduceGenericVector reducer v =
  let (b', s) = go (reducerInitState reducer) (reducerInitAcc reducer) 0 (Generic.length v) v
   in reducerFinalize reducer s b'
  where
    go s r i len v =
      if i >= len
        then (r, s)
        else case reducerStep reducer s r (Generic.unsafeIndex v i) of
          (Reduced r', s) -> (r', s)
          (Continue r', s) -> go s r' (i + 1) len v

{- | Run a reducer on all @Stream@ elements.

===== Examples

>>> reduceVectorStream sum (Stream.fromList [1,2,3])
6

@since 1.0.0
-}
{-# INLINEABLE reduceVectorStream #-}
reduceVectorStream ::
  forall (m :: Type -> Type) (a :: Type) (s :: Type) (r :: Type).
  Monad m =>
  Reducer s a r ->
  Stream m a ->
  m r
reduceVectorStream reducer (Stream step ss) = do
  (r', s) <- go (reducerInitState reducer) (reducerInitAcc reducer) ss
  pure $ reducerFinalize reducer s r'
  where
    go s r ss = do
      streamR <- step ss
      case streamR of
        Yield a streamR' -> case reducerStep reducer s r a of
          (Reduced v, s) -> pure (v, s)
          (Continue v, s) -> go s v streamR'
        Skip streamR' -> go s r streamR'
        Done -> pure (r, s)

{- | Collect all elements into a @Vector@.

===== Examples

>>> reduceIterate (take 10 |> intoVector) (+3) 42
[42,45,48,51,54,57,60,63,66,69]

@since 1.0.0
-}
{-# INLINEABLE intoVector #-}
intoVector :: forall (a :: Type). Reducer (Stream Id a) a (Vector a)
intoVector = intoGenericVector

{- | Like 'intoVector' but works over generic vector interface.

@since 1.0.0
-}
{-# SPECIALIZE intoGenericVector :: forall (a :: Type). Reducer (Stream Id a) a (Vector a) #-}
{-# INLINEABLE intoGenericVector #-}
intoGenericVector ::
  forall (a :: Type) (v :: Type -> Type). Generic.Vector v a => Reducer (Stream Id a) a (v a)
intoGenericVector =
  Reducer
    { reducerInitState = Stream.empty
    , reducerInitAcc = Generic.empty
    , reducerFinalize = \s _ -> Generic.unstream (Bundle.fromStream s Unknown)
    , reducerStep = \s r a -> (Continue r, Stream.snoc s a)
    }

{- | Collect all elements into a @Stream@.

===== Examples

>>> Stream.toList (reduceList (intoVectorStream @Identity) [1,2,3])
Identity [1,2,3]

@since 1.0.0
-}
{-# INLINEABLE intoVectorStream #-}
intoVectorStream ::
  forall (m :: Type -> Type) (a :: Type). Monad m => Reducer () a (Stream m a)
intoVectorStream =
  Reducer
    { reducerInitState = ()
    , reducerInitAcc = Stream.empty
    , reducerFinalize = const id
    , reducerStep = \s r a -> (Continue (Stream.snoc r a), s)
    }

{- | Flatten a sequence of @Vector a@ into a sequence of @a@.

===== Examples

>>> reduceList (concatVector |> intoVector) [Vector.fromList [1,2], Vector.fromList [3,4,5]]
[1,2,3,4,5]

@since 1.0.0
-}
{-# INLINEABLE concatVector #-}
concatVector ::
  forall (a :: Type) (s :: Type) (r :: Type). Reducer s a r -> Reducer s (Vector a) r
concatVector = concatGenericVector

{- | Like 'concatVector' but works over generic vector interface.

@since 1.0.0
-}
{-# SPECIALIZE concatGenericVector :: forall (a :: Type) (s :: Type) (r :: Type). Reducer s a r -> Reducer s (Vector a) r #-}
{-# INLINEABLE concatGenericVector #-}
concatGenericVector ::
  forall (v :: Type -> Type) (a :: Type) (s :: Type) (r :: Type).
  Generic.Vector v a =>
  Reducer s a r ->
  Reducer s (v a) r
concatGenericVector reducer = statelessTransducer reducer (step 0)
  where
    step :: Int -> s -> r -> v a -> (Reduced r, s)
    step i s r v =
      if i >= Generic.length v
        then (Continue r, s)
        else case reducerStep reducer s r (Generic.unsafeIndex v i) of
          (Reduced r', s') -> (Reduced r', s')
          (Continue r', s') -> step (i + 1) s' r' v
