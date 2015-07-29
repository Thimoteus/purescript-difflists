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

benchAppend :: forall e. Benchmark e
benchAppend = mkBenchmark
  { slug: "append"
  , title: "Append two structures together"
  , sizes: (1..50) <#> (*100)
  , sizeInterpretation: "Number of elements in each structure"
  , inputsPerSize: 1
  , gen: \n -> Tuple <$> randomArray n <*> randomArray n
  , functions: -- [ benchFn "Array" (uncurry (<>))
               [ benchFn' "DList" (uncurry (<>)) (bimap DL.toDList DL.toDList)
               -- , benchFn' "DArray" (uncurry (<>)) (bimap DA.toDArray DA.toDArray)
               , benchFn' "List" (uncurry (<>)) (bimap L.toList L.toList)
               -- , benchFn' "Seq"  (S.fullyForce <<< uncurry (<>))
                                 -- (bimap S.toSeq S.toSeq)
               ]
  }

main =
  runSuite
    [ benchAppend ]

foreign import randomArray :: forall e. Int -> Eff (BenchEffects e) (Array Number)
