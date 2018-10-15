module Benchmark.Main where

import Prelude
import Data.Foldable
import Data.Traversable
import Data.Tuple
import Data.Maybe
import Data.Array ((..))
import Data.DList as DL
import Data.List as L
import Math (floor, sqrt)
import Test.QuickCheck.Gen
import Effect.Console
import Effect
import Test.QuickCheck.Arbitrary (arbitrary)

import Benchotron.Core
import Benchotron.UI.Console

bimap :: forall a b c d. (a -> b) -> (c -> d) -> Tuple a c -> Tuple b d
bimap f g (Tuple x y) = Tuple (f x) (g y)

benchAppendTwo :: Benchmark
benchAppendTwo = mkBenchmark
  { slug: "append"
  , title: "Append two structures together"
  , sizes: (1..50) <#> (_*100)
  , sizeInterpretation: "Number of elements in each structure"
  , inputsPerSize: 1
  , gen: \n -> do
     a <- (vectorOf n arbitrary) :: Gen (Array Int)
     b <- (vectorOf n arbitrary) :: Gen (Array Int)
     pure $ Tuple a b
  , functions: 
    [ benchFn "DList" $ \tuple ->
        (DL.dlist2List <<< uncurry (<>)) $
          (bimap DL.toDList DL.toDList) tuple
    , benchFn "List" $ \tuple ->
      (uncurry (<>)) $ (bimap L.fromFoldable L.fromFoldable) tuple
  ]
  }

benchAppendMany :: Benchmark
benchAppendMany = mkBenchmark
  { slug: "appendmany"
  , title: "Append many two-element structures together"
  , sizes: (1..50) <#> (_*50)
  , sizeInterpretation: "Number of structures"
  , inputsPerSize: 1
  , gen: \n -> 
    vectorOf n (vectorOf 2 arbitrary) :: Gen (Array (Array Int))
  , functions: 
    [ benchFn "DList" $ \xs ->
      (DL.dlist2List <<< foldMap identity) $ (map DL.toDList) xs
    , benchFn "List" $ \xs ->
      (foldMap identity) $ (map L.fromFoldable) xs
    ]
  }

main =
  runSuite
    [ benchAppendTwo ]

foreign import randomArray :: forall e. Int -> Effect (Array Number)
foreign import randomArrays :: forall e. Int -> Int -> Effect (Array (Array Number))
