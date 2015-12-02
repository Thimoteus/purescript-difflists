module Benchmark.Main where

import Prelude
import Data.Foldable
import Data.Traversable
import Data.Tuple
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Sequence as S
import qualified Data.DList as DL
import qualified Data.List as L
import qualified Data.DArray as DA
import Math (floor, sqrt)
import Test.QuickCheck.Gen
import Control.Monad.Eff

import Benchotron.Core
import Benchotron.UI.Console

(..) = A.(..)

bimap :: forall a b c d. (a -> b) -> (c -> d) -> Tuple a c -> Tuple b d
bimap f g (Tuple x y) = Tuple (f x) (g y)

benchAppendTwo :: forall e. Benchmark e
benchAppendTwo = mkBenchmark
  { slug: "append"
  , title: "Append two structures together"
  , sizes: (1..50) <#> (*100)
  , sizeInterpretation: "Number of elements in each structure"
  , inputsPerSize: 1
  , gen: \n -> Tuple <$> randomArray n <*> randomArray n
  , functions: -- [ benchFn "Array" (uncurry (<>))
               [ benchFn' "DList" (DL.dlist2List <<< uncurry (<>)) (bimap DL.toDList DL.toDList)
               -- , benchFn' "DArray" (uncurry (<>)) (bimap DA.toDArray DA.toDArray)
               , benchFn' "List" (uncurry (<>)) (bimap L.toList L.toList)
               -- , benchFn' "Seq"  (S.fullyForce <<< uncurry (<>))
                                 -- (bimap S.toSeq S.toSeq)
               ]
  }

benchAppendMany :: forall e. Benchmark e
benchAppendMany = mkBenchmark
  { slug: "appendmany"
  , title: "Append many two-element structures together"
  , sizes: (1..50) <#> (*50)
  , sizeInterpretation: "Number of structures"
  , inputsPerSize: 1
  , gen: \n -> randomArrays n n
  , functions: [ benchFn' "DList" (DL.dlist2List <<< foldMap id) (map DL.toDList)
               , benchFn' "List" (foldMap id) (map L.toList) ]
  }

main =
  runSuite
    [ benchAppendMany ]

foreign import randomArray :: forall e. Int -> Eff (BenchEffects e) (Array Number)
foreign import randomArrays :: forall e. Int -> Int -> Eff (BenchEffects e) (Array (Array Number))
