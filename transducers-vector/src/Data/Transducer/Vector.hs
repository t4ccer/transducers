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

import Control.Applicative (Applicative (pure))
import Data.Transducer (
  Reduced (Continue, Reduced),
  Reducer (Reducer, reducerFinalize, reducerInitAcc, reducerInitState, reducerStep),
  statelessTransducer,
 )

{-# INLINEABLE reduceVector #-}
reduceVector :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> Vector a -> r
reduceVector = reduceGenericVector

{-# SPECIALIZE reduceGenericVector :: forall (a :: Type) (r :: Type) (s :: Type). Reducer s a r -> Vector a -> r #-}
{-# INLINEABLE reduceGenericVector #-}
reduceGenericVector ::
  forall (v :: Type -> Type) (a :: Type) (r :: Type) (s :: Type).
  Generic.Vector v a =>
  Reducer s a r ->
  v a ->
  r
reduceGenericVector reducer v =
  let (b', s) =
        reduceGenericVector'
          (reducerStep reducer)
          (reducerInitState reducer)
          (reducerInitAcc reducer)
          0
          (Generic.length v)
          v
   in reducerFinalize reducer s b'

{-# INLINEABLE reduceGenericVector' #-}
reduceGenericVector' ::
  forall (v :: Type -> Type) (a :: Type) (r :: Type) (s :: Type).
  Generic.Vector v a =>
  (s -> r -> a -> (Reduced r, s)) ->
  s ->
  r ->
  Int ->
  Int ->
  v a ->
  (r, s)
reduceGenericVector' f s r i len v =
  if i >= len
    then (r, s)
    else case f s r (Generic.unsafeIndex v i) of
      (Reduced r', s) -> (r', s)
      (Continue r', s) -> reduceGenericVector' f s r' (i + 1) len v

{-# INLINEABLE reduceVectorStream #-}
reduceVectorStream ::
  forall (m :: Type -> Type) (a :: Type) (s :: Type) (r :: Type).
  Monad m =>
  Reducer s a r ->
  Stream m a ->
  m r
reduceVectorStream reducer (Stream step ss) = do
  (r', s) <-
    reduceVectorStream'
      (reducerStep reducer)
      (reducerInitState reducer)
      (reducerInitAcc reducer)
      ss
  pure $ reducerFinalize reducer s r'
  where
    reduceVectorStream' f s r ss = do
      streamR <- step ss
      case streamR of
        Yield a streamR' -> case f s r a of
          (Reduced v, s) -> pure (v, s)
          (Continue v, s) -> reduceVectorStream' f s v streamR'
        Skip streamR' -> reduceVectorStream' f s r streamR'
        Done -> pure (r, s)

{-# INLINEABLE intoVector #-}
intoVector :: forall (a :: Type). Reducer (Stream Id a) a (Vector a)
intoVector = intoGenericVector

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

{-# INLINEABLE concatVector #-}
concatVector ::
  forall (a :: Type) (s :: Type) (r :: Type). Reducer s a r -> Reducer s (Vector a) r
concatVector = concatGenericVector

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
