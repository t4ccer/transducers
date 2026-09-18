{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Main (main) where

import Data.Bool (Bool (False, True))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Function (id, ($))
import Data.Int (Int)
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (Maybe (Just, Nothing))
import Data.Maybe qualified as Maybe
import Data.Monoid qualified as Monoid
import Data.Ord (Ord (max), Ordering (EQ, GT, LT))
import Data.Semigroup qualified as Semigroup
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Num (Num ((+)))
import GHC.Real (even)
import System.IO (IO)
import Test.QuickCheck ((===), pattern Fn, pattern Fn2)
import Test.QuickCheck qualified as QC (NonEmptyList (NonEmpty))
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Prelude (undefined)

#if !(MIN_VERSION_base(4,19,0))
import Data.Function ((.))
import Data.Maybe (maybe)
#endif

#if !(MIN_VERSION_base(4,21,0))
import Prelude (Num((-)), Ord((<), (>)), foldr, otherwise)
#endif

import Data.Transducer (
  all,
  and,
  any,
  catMaybes,
  compareLength,
  concatList,
  drop,
  dropWhile,
  enumerate,
  filter,
  find,
  group,
  groupBy,
  groupOn,
  head,
  head1,
  intersperse,
  intoList,
  intoNonEmpty,
  intoNonEmpty1,
  last,
  last1,
  length,
  map,
  mapMaybe,
  maximum,
  maximum1,
  mconcat,
  minimum,
  minimum1,
  nub,
  nubBy,
  null,
  or,
  product,
  reduceIterate,
  reduceList,
  reduceNonEmpty,
  reduceNonEmpty1,
  reduceRepeat,
  reduceReplicate,
  sconcat,
  sconcat1,
  sum,
  take,
  takeWhile,
  uncons,
  unsnoc,
  zip,
  zipEither,
  (|>),
 )

main :: IO ()
main = do
  setLocaleEncoding utf8

  let
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 1_000

  defaultMain $
    adjustOption moreTests $
      testGroup
        "transducers-core"
        [ testGroup
            "List"
            [ testProperty "reduceList intoList = id" $ \(xs :: [Int]) ->
                reduceList intoList xs === xs
            ]
        , testGroup
            "NonEmpty"
            [ testProperty "nonEmpty . reduceList intoList = reduceList intoNonEmpty" $ \(xs :: [Int]) ->
                nonEmpty (reduceList intoList xs) === reduceList intoNonEmpty xs
            , testProperty "reduceNonEmpty1 intoNonEmpty1 = id" $ \(xs :: NonEmpty Int) ->
                reduceNonEmpty1 intoNonEmpty1 xs === xs
            ]
        , testGroup
            "iterate"
            [ testProperty "Equivalent to []" $ \(x :: Int) (Fn (f :: Int -> Int)) (d :: Int) (t :: Int) ->
                List.take t (List.drop d (List.iterate f x))
                  === reduceIterate (drop d |> take t |> intoList) f x
            ]
        , testGroup
            "repeat"
            [ testProperty "Equivalent to []" $ \(x :: Int) (d :: Int) (t :: Int) ->
                List.take t (List.drop d (List.repeat x))
                  === reduceRepeat (drop d |> take t |> intoList) x
            ]
        , testGroup
            "replicate"
            [ testProperty "Equivalent to []" $ \(x :: Int) (n :: Int) (d :: Int) (t :: Int) ->
                List.take t (List.drop d (List.replicate n x))
                  === reduceReplicate (drop d |> take t |> intoList) n x
            ]
        , testGroup
            "sum"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.sum xs === reduceList sum xs
            , testCase
                "sum [1,2,3] = 6"
                (reduceList sum ([1, 2, 3] :: [Int]) @?= 6)
            , testCase
                "sum (map (+1) [1,2,3]) = 9"
                (reduceList (map (+ 1) |> sum) ([1, 2, 3] :: [Int]) @?= 9)
            , testCase
                "sum (take 3 [1..]) = 6"
                (reduceList (take 3 |> sum) ([1, 2, 3] :: [Int]) @?= 6)
            ]
        , testGroup
            "product"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.product xs === reduceList product xs
            ]
        , testGroup
            "take"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (n :: Int) ->
                List.take n xs === reduceList (take n |> intoList) xs
            , testCase
                "take 3 [1..] = [1,2,3]"
                (reduceList (take 3 |> intoList) ([1 ..] :: [Int]) @?= [1, 2, 3])
            , testCase
                "take 3 |> map (+1) [1..] = [2,3,4]"
                (reduceList (take 3 |> map (+ 1) |> intoList) ([1 ..] :: [Int]) @?= [2, 3, 4])
            , testCase
                "map (+1) |> take 3 [1..] = [2,3,4]"
                (reduceList (map (+ 1) |> take 3 |> intoList) ([1 ..] :: [Int]) @?= [2, 3, 4])
            ]
        , testGroup
            "filter"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (pred :: Int -> Bool)) ->
                List.filter pred xs === reduceList (filter pred |> intoList) xs
            , testCase
                "filter even [1,2,3,4] = [2,4]"
                (reduceList (filter even |> intoList) ([1, 2, 3, 4] :: [Int]) @?= [2, 4])
            ]
        , testGroup
            "drop"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (n :: Int) ->
                List.drop n xs === reduceList (drop n |> intoList) xs
            , testCase
                "drop 2 [1,2,3,4,5] = [3,4,5]"
                (reduceList (drop 2 |> intoList) ([1, 2, 3, 4, 5] :: [Int]) @?= [3, 4, 5])
            , testCase
                "drop 2 |> take 3 [1..] = [3,4,5]"
                (reduceList (drop 2 |> take 3 |> intoList) ([1 ..] :: [Int]) @?= [3, 4, 5])
            ]
        , testGroup
            "and"
            [ testProperty "Equivalent to []" $ \(xs :: [Bool]) ->
                List.and xs === reduceList and xs
            ]
        , testGroup
            "or"
            [ testProperty "Equivalent to []" $ \(xs :: [Bool]) ->
                List.or xs === reduceList or xs
            ]
        , testGroup
            "all"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (pred :: Int -> Bool)) ->
                List.all pred xs === reduceList (all pred) xs
            , testCase
                "all even [2,4,6] = True"
                (reduceList (all even) ([2, 4, 6] :: [Int]) @?= True)
            , testCase
                "all even [2,5,6] = False"
                (reduceList (all even) ([2, 5, 6] :: [Int]) @?= False)
            , testCase
                "all even [2..] = False"
                (reduceList (all even) ([2 ..] :: [Int]) @?= False)
            ]
        , testGroup
            "any"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (pred :: Int -> Bool)) ->
                List.any pred xs === reduceList (any pred) xs
            , testCase
                "any even [1..] = True"
                (reduceList (any even) ([1 ..] :: [Int]) @?= True)
            ]
        , testGroup
            "maximum"
            [ testProperty "Equivalent to []" $ \(QC.NonEmpty (xs :: [Int])) ->
                Just (List.maximum xs) === reduceList maximum xs
            , testCase "maximum [] == Nothing" (reduceList maximum ([] :: [Int]) @?= Nothing)
            ]
        , testGroup
            "maximum1"
            [ testProperty "Equivalent to NonEmpty" $ \(xs :: NonEmpty Int) ->
                List.maximum xs === reduceNonEmpty1 maximum1 xs
            ]
        , testGroup
            "minimum"
            [ testProperty "Equivalent to []" $ \(QC.NonEmpty (xs :: [Int])) ->
                Just (List.minimum xs) === reduceList minimum xs
            , testCase "minimum [] = Nothing" (reduceList minimum ([] :: [Int]) @?= Nothing)
            ]
        , testGroup
            "minimum1"
            [ testProperty "Equivalent to NonEmpty" $ \(xs :: NonEmpty Int) ->
                List.minimum xs === reduceNonEmpty1 minimum1 xs
            ]
        , testGroup
            "head"
            [ testProperty "Equivalent to []" $ \(QC.NonEmpty (xs :: [Int])) ->
                Just (List.head xs) === reduceList head xs
            , testCase "head [] == Nothing" (reduceList head ([] :: [Int]) @?= Nothing)
            ]
        , testGroup
            "head1"
            [ testProperty "Equivalent to NonEmpty" $ \(xs :: NonEmpty Int) ->
                NonEmpty.head xs === reduceNonEmpty1 head1 xs
            , testCase "head1 (a :| undefined) == a" (reduceNonEmpty1 head1 ('a' :| undefined) @?= 'a')
            ]
        , testGroup
            "last"
            [ testProperty "Equivalent to []" $ \(QC.NonEmpty (xs :: [Int])) ->
                Just (List.last xs) === reduceList last xs
            , testCase "last [] = Nothing" (reduceList last ([] :: [Int]) @?= Nothing)
            ]
        , testGroup
            "last1"
            [ testProperty "Equivalent to NonEmpty" $ \(xs :: NonEmpty Int) ->
                NonEmpty.last xs === reduceNonEmpty1 last1 xs
            ]
        , testGroup
            "find"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (pred :: Int -> Bool)) ->
                List.find pred xs === reduceList (find pred) xs
            , testCase "last [] = Nothing" (reduceList last ([] :: [Int]) @?= Nothing)
            ]
        , testGroup
            "takeWhile"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (pred :: Int -> Bool)) ->
                List.takeWhile pred xs === reduceList (takeWhile pred |> intoList) xs
            ]
        , testGroup
            "dropWhile"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (pred :: Int -> Bool)) ->
                List.dropWhile pred xs === reduceList (dropWhile pred |> intoList) xs
            ]
        , testGroup
            "mapMaybe"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (pred :: Int -> Maybe Bool)) ->
                Maybe.mapMaybe pred xs === reduceList (mapMaybe pred |> intoList) xs
            , testProperty "mapMaybe id = catMaybes" $ \(xs :: [Maybe Int]) ->
                reduceList (mapMaybe id |> intoList) xs === reduceList (catMaybes |> intoList) xs
            ]
        , testGroup
            "null"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.null xs === reduceList null xs
            ]
        , testGroup
            "length"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.length xs === reduceList length xs
            ]
        , testGroup
            "compareLength"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (n :: Int) ->
                list_compareLength xs n === reduceList (compareLength n) xs
            , testCase
                "compareLength 42 [0..10] = LT"
                (reduceList (compareLength 42) ([0 .. 10] :: [Int]) @?= LT)
            , testCase
                "compareLength 42 [0..41] = EQ"
                (reduceList (compareLength 42) ([0 .. 41] :: [Int]) @?= EQ)
            , testCase
                "compareLength 42 [0..] = GT"
                (reduceList (compareLength 42) ([0 ..] :: [Int]) @?= GT)
            ]
        , testGroup
            "sconcat"
            [ testProperty "Equivalent to NonEmpty" $ \(xs :: NonEmpty [Int]) ->
                Just (Semigroup.sconcat xs) === reduceNonEmpty sconcat xs
            ]
        , testGroup
            "sconcat1"
            [ testProperty "Equivalent to NonEmpty" $ \(xs :: NonEmpty [Int]) ->
                Semigroup.sconcat xs === reduceNonEmpty1 sconcat1 xs
            ]
        , testGroup
            "mconcat"
            [ testProperty "Equivalent to []" $ \(xs :: [[Int]]) ->
                Monoid.mconcat xs === reduceList mconcat xs
            ]
        , testGroup
            "intersperse"
            [ testProperty "Equlivalent to []" $ \(xs :: [Int]) (n :: Int) ->
                List.intersperse n xs === reduceList (intersperse n |> intoList) xs
            , testCase
                "(intersperse 0 |> take 4) [1..] = [1,0,2,0]"
                (reduceList (intersperse 0 |> take 4 |> intoList) ([1 ..] :: [Int]) @?= [1, 0, 2, 0])
            , testCase
                "(intersperse 0 |> take 5) [1..] = [1,0,2,0,3]"
                (reduceList (intersperse 0 |> take 5 |> intoList) ([1 ..] :: [Int]) @?= [1, 0, 2, 0, 3])
            ]
        , testGroup
            "concatList"
            [ testProperty "Equlivalent to []" $ \(xs :: [[Int]]) ->
                List.concat xs === reduceList (concatList |> intoList) xs
            , testCase
                "(concatList |> take 3) [[1,2],[3,4],[5,6]] = [1,2,3]"
                (reduceList (concatList |> take 3 |> intoList) ([[1, 2], [3, 4], [5, 6]] :: [[Int]]) @?= [1, 2, 3])
            ]
        , testGroup
            "nub"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.nub xs === reduceList (nub |> intoList) xs
            ]
        , testGroup
            "nubBy"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn2 (pred :: Int -> Int -> Bool)) ->
                List.nubBy pred xs === reduceList (nubBy pred |> intoList) xs
            ]
        , testGroup
            "zip"
            [ testCase
                "(drop 1 |> zip (take 1) (take 2)) [1..] = ([2],[2,3])"
                ( reduceList
                    (drop 1 |> zip (take 1 |> intoList) (take 2 |> intoList))
                    ([1 ..] :: [Int])
                    @?= ([2], [2, 3])
                )
            , testCase
                "(drop 1 |> map splitEven |> zipEither head intoList) [0..6] = (Just 1,[2,4,6])"
                ( reduceList (drop 1 |> map splitEven |> zipEither head intoList) [0 .. 6]
                    @?= (Just 1, [2, 4, 6])
                )
            , testCase
                "(map splitEven |> zipEither head (take 3 |> last)) [1..] = (Just 1,Just 6)"
                ( reduceList (map splitEven |> zipEither head (take 3 |> last)) [1 ..]
                    @?= (Just 1, Just 6)
                )
            ]
        , testGroup
            "uncons"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.uncons xs === reduceList (uncons intoList) xs
            , testCase
                "uncons intoList [1,2,3] = Just (1, [2,3])"
                (reduceList (uncons intoList) ([1, 2, 3] :: [Int]) @?= Just (1, [2, 3]))
            , testCase
                "(drop 2 |> uncons (take 3)) [1..] = Just (3, [4,5,6])"
                (reduceList (drop 2 |> uncons (take 3 |> intoList)) ([1 ..] :: [Int]) @?= Just (3, [4, 5, 6]))
            ]
        , testGroup
            "unsnoc"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                list_unsnoc xs === reduceList (unsnoc intoList) xs
            ]
        , testGroup
            "enumerate"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.zip [0 ..] xs === reduceList (enumerate |> intoList) xs
            ]
        , testGroup
            "group"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) ->
                List.group xs === reduceList (group intoList |> intoList) xs
            ]
        , testGroup
            "groupBy"
            [ testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn2 (eq :: Int -> Int -> Bool)) ->
                List.groupBy eq xs === reduceList (groupBy eq intoList |> intoList) xs
            ]
        , testGroup
            "groupOn"
            [ testCase
                "groupOn even [1, 2, 4, 5, 7, 9, 6, 8] = [[1], [2, 4], [5, 7, 9], [6, 8]]"
                ( reduceList (groupOn even intoList |> intoList) [1 :: Int, 2, 4, 5, 7, 9, 6, 8]
                    @?= ([[1], [2, 4], [5, 7, 9], [6, 8]])
                )
            , testProperty "Equivalent to []" $ \(xs :: [Int]) (Fn (key :: Int -> Int)) ->
                List.groupBy (\x y -> key x == key y) xs === reduceList (groupOn key intoList |> intoList) xs
            ]
        ]

-- * Utils

splitEven :: Int -> Either Int Int
splitEven n = if even n then Right n else Left n

-- * Stubs

list_compareLength :: forall (a :: Type). [a] -> Int -> Ordering
#if !(MIN_VERSION_base(4,21,0))
list_compareLength xs n
  | n < 0 = GT
  | otherwise = foldr
    (\_ f m -> if m > 0 then f (m - 1) else GT)
    (\m -> if m > 0 then LT else EQ)
    xs
    n
#else
list_compareLength = List.compareLength
#endif

list_unsnoc :: forall (a :: Type). [a] -> Maybe ([a], a)
#if !(MIN_VERSION_base(4,19,0))
list_unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
#else
list_unsnoc = List.unsnoc
#endif
