{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
Internal implementation utilities.
They are exposed because not allowing to look under the hood is harmful for ones ability to innovate, however, they are NOT under PVP versioning.
-}
module Data.Transducer.Internal where

import Data.Kind (Type)

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
