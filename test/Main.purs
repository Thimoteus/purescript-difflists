module Test.Main where

import Prelude
import Effect (Effect)
import Test.DList.Alternative as Alternative
import Test.DList.Functor as Functor
import Test.DList.MonadPlus as MonadPlus
import Test.DList.Monoid as Monoid
import Test.DList.Applicative as Applicative
import Test.DList.Isomorphism as Isomorphism
import Test.DList.Monad as Monad
import Test.DList.Ord as Ord

main :: Effect Unit
main = do
  Alternative.spec
  Functor.spec
  MonadPlus.spec
  Monoid.spec
  Applicative.spec
  Isomorphism.spec
  Monad.spec
  Ord.spec
