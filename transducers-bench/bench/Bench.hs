{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Main (main) where

import Control.Applicative (pure)
import Data.Foldable (foldl')
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Kind (Type)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import GHC.Base (seq, ($), (.))
import GHC.Num (Integer, (*), (+), (-))
import GHC.Real (div, fromIntegral)
import GHC.Show (show)
import System.IO (IO)
import Test.Tasty ()
import Test.Tasty.Bench (Benchmark, bcompare, bench, bgroup, defaultMain, env, nf)
import Prelude qualified as List

import Data.Transducer (Reducer, length, map, reduceList, sum, (|>))

main :: IO ()
main = defaultMain (List.map varianceGroup [1e5, 1e6, 1e7])

varianceGroup :: Integer -> Benchmark
varianceGroup n =
  env (pure (let lst = [1 .. n] in List.last lst `seq` lst)) $ \lst ->
    bgroup
      groupName
      [ bgroup
          "list"
          [ bench baselineName (nf (reduceList variance) [1 .. n])
          , vsBaseline $ bench "transducers materialized" (nf (reduceList variance) lst)
          , vsBaseline $ bench "list lazy naive" (nf (\n -> variance_list_naive [1 .. n]) n)
          , vsBaseline $ bench "list materialized naive" (nf variance_list_naive lst)
          , vsBaseline $ bench "list lazy fold" (nf (\n -> variance_list_fold [1 .. n]) n)
          , vsBaseline $ bench "list materialized fold" (nf variance_list_fold lst)
          ]
      ]
  where
    groupName = "variance [1.." <> show n <> "]"
    baselineName = "transducers lazy"
    vsBaseline = bcompare ("$2 == \"" <> groupName <> "\" && $NF == \"" <> baselineName <> "\"")

variance_list_naive :: [Integer] -> Maybe Integer
variance_list_naive xs =
  let total = List.sum xs
      totalSq = List.sum (List.map (\x -> x * x) xs)
      len = fromIntegral (List.length xs)
      m = total `div` len
   in case len of
        0 -> Nothing
        _ -> Just ((totalSq `div` len) - (m * m))

type Acc :: Type
data Acc = Acc !Integer !Integer !Int

accVariance :: Acc -> Maybe Integer
accVariance (Acc total totalSq n) =
  case n of
    0 -> Nothing
    _ ->
      let len = fromIntegral n
          m = total `div` len
       in Just ((totalSq `div` len) - (m * m))

variance_list_fold :: [Integer] -> Maybe Integer
variance_list_fold =
  accVariance . foldl' (\(Acc total totalSq n) x -> Acc (total + x) (totalSq + x * x) (n + 1)) (Acc 0 0 0)

{-# INLINE variance #-}
variance :: Reducer Integer (Maybe Integer)
variance = do
  total <- sum
  totalSq <- map (\x -> x * x) |> sum
  len <- fmap fromIntegral length
  pure
    ( case len of
        0 -> Nothing
        _ ->
          let m = total `div` len
           in Just (totalSq `div` len - m * m)
    )
