module Main (main) where

import Data.Function (($))
import Data.Int (Int)
import Data.List qualified as List
import Data.Ord (Ord (max))
import Data.Sequence qualified as Sequence
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (IO)
import Test.QuickCheck ((===))
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

import Data.Transducer (intoList, reduceList, (|>))
import Data.Transducer.Sequence (concatSeq, intoSeq, reduceSeq)

main :: IO ()
main = do
  setLocaleEncoding utf8

  let
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000

  defaultMain $
    adjustOption moreTests $
      testGroup
        "transducers-containers"
        [ testGroup
            "Seq"
            [ testProperty "reduceList intoSeq = Sequence.fromList" $ \(xs :: [Int]) ->
                Sequence.fromList xs === reduceList intoSeq xs
            , testProperty "reduceSeq intoList . Sequence.fromList = id" $ \(xs :: [Int]) ->
                reduceSeq intoList (Sequence.fromList xs) === xs
            , testProperty "reduceList concatSeq . List.map Sequence.fromList === List.concat" $ \(xs :: [[Int]]) ->
                reduceList (concatSeq |> intoList) (List.map Sequence.fromList xs) === List.concat xs
            ]
        ]
