module Data.Transducer.Internal (ZipFinished (..)) where

import Data.Kind (Type)

type ZipFinished :: Type
data ZipFinished
  = ZipFinishedNone
  | ZipFinished1
  | ZipFinished2
  | ZipFinishedBoth
