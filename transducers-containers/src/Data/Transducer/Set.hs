-- | A tiny set of utilities to construct reducers from @Set@s or reduce into @Set@s
module Data.Transducer.Set (
  -- * Runners
  reduceSet,

  -- * Reducers
  intoSet,

  -- * Transducers
  concatSet,
) where

import Data.Kind (Type)
import Data.Ord (Ord)
import Data.Set (Set)
import Data.Set qualified as Set
import Prelude (flip, ($))

import Data.Transducer (
  Initial (Zero),
  IsInitial,
  Reduced (Continue, Reduced),
  Reducer (Reducer, finalize, state, step),
  initializeReducer,
  linearReducer,
  reduceFoldr,
 )

-- NOTE: In doctest, never assert on output of `Set.toList [...]` as it is unstable

-- $setup
-- >>> import Data.Transducer ((|>), sum, take, reduceIterate, reduceList)
-- >>> import GHC.Num (Num ((+)))

-- | Run a reducer on all elements of a 'Set'.
--
-- ===== Examples
--
-- >>> reduceSet sum (Set.fromList [1,2,3])
-- 6
--
-- @since 1.0.0
{-# INLINE reduceSet #-}
reduceSet :: forall (a :: Type) (r :: Type). Reducer 'Zero a r -> Set a -> r
reduceSet = reduceFoldr

-- | Collect all elements into a @Set@.
--
-- ===== Examples
--
-- >>> Set.member 60 (reduceIterate @'Zero (take 10 |> intoSet) (+3) 42)
-- True
--
-- @since 1.0.0
{-# INLINE intoSet #-}
intoSet :: forall (init :: Initial) (a :: Type). (IsInitial init, Ord a) => Reducer init a (Set a)
intoSet = linearReducer Set.empty (flip Set.insert) -- TODO: Optimize

-- | Flatten a sequence of @Set a@ into a sequence of @a@.
--
-- ===== Examples
--
-- >>> reduceList (concatSet |> sum) [Set.fromList [1,2], Set.fromList [3,4,5]]
-- 15
--
-- @since 1.0.0
{-# INLINE concatSet #-}
concatSet :: forall (init :: Initial) (a :: Type) (r :: Type). (IsInitial init) => Reducer 'Zero a r -> Reducer init (Set a) r
concatSet (Reducer state finalize step) =
  initializeReducer $
    Reducer
      { state = state
      , finalize = finalize
      , step = \s set -> Set.foldr' go Continue set s
      }
  where
    go a k s = case step s a of
      Reduced s' -> Reduced s'
      Continue s' -> k s'
