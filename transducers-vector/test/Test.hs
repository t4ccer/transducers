module Main (main) where

import Data.Function (($))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Int (Int)
import Data.List qualified as List
import Data.Ord (Ord (max))
import Data.Vector qualified as Vector
import Data.Vector.Fusion.Stream.Monadic qualified as Stream
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (IO)
import Test.QuickCheck ((===))
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

import Data.Transducer (intoList, reduceList, (|>))
import Data.Transducer.Vector (concatVector, intoVector, intoVectorStream, reduceVector, reduceVectorStream)

main :: IO ()
main = do
  setLocaleEncoding utf8

  let
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000

  defaultMain $
    adjustOption moreTests $
      testGroup
        "transducers-vector"
        [ testGroup
            "Vector"
            [ testProperty "reduceList intoVector = Vector.fromList" $ \(xs :: [Int]) ->
                reduceList intoVector xs === Vector.fromList xs
            , testProperty "reduceVector intoList . Vector.fromList = id" $ \(xs :: [Int]) ->
                reduceVector intoList (Vector.fromList xs) === xs
            , testProperty "reduceList concatVector . List.map Vector.fromList === List.concat" $ \(xs :: [[Int]]) ->
                reduceList (concatVector |> intoList) (List.map Vector.fromList xs) === List.concat xs
            ]
        , testGroup
            "Stream"
            [ testProperty "reduceList intoVectorStream = Stream.fromList" $ \(xs :: [Int]) ->
                Stream.toList (reduceList (intoVectorStream @Identity) xs)
                  === Stream.toList (Stream.fromList xs)
            , testProperty "reduceVectorStream intoList . Stream.fromList = id" $ \(xs :: [Int]) ->
                runIdentity (reduceVectorStream intoList (Stream.fromList xs)) === xs
            ]
        ]
