{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Function (($))
import Data.Int (Int)
import Data.Ord (Ord (max))
import Data.These (These (That, These, This), partitionHereThere)
import Data.These.Combinators (catHere, catThere)
import Data.Tuple (fst, snd)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (IO)
import Test.QuickCheck (Arbitrary (arbitrary), (===))
import Test.QuickCheck qualified as QC
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

import Data.Transducer (discard, intoList, reduceList)
import Data.Transducer.These (zipReducersForkThese)

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    QC.elements [This a, That b, These a b]
  shrink = \case
    This _ -> []
    That _ -> []
    These a b -> [This a, That b]

main :: IO ()
main = do
  setLocaleEncoding utf8

  let
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000

  defaultMain $
    adjustOption moreTests $
      testGroup
        "transducers-these"
        [ testGroup
            "Vector"
            [ testProperty "zipReducersForkThese intoList intoList = partitionHereThere" $
                \(xs :: [These Int Int]) ->
                  reduceList (zipReducersForkThese intoList intoList) xs
                    === partitionHereThere xs
            , testProperty "fst . zipReducersForkThese intoList discard = catHere" $
                \(xs :: [These Int Int]) ->
                  fst (reduceList (zipReducersForkThese intoList discard) xs)
                    === catHere xs
            , testProperty "snd . zipReducersForkThese discard intoList = catHere" $
                \(xs :: [These Int Int]) ->
                  snd (reduceList (zipReducersForkThese discard intoList) xs)
                    === catThere xs
            ]
        ]
