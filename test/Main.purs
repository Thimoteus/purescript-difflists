module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
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
  log "Running alternative tests ..."
  Alternative.spec
  log "Running functor tests ..."
  Functor.spec
  log "Running monadplus tests ..."
  MonadPlus.spec
  log "Running monoid tests ..."
  Monoid.spec
  log "Running applicative tests ..."
  Applicative.spec
  log "Running isomorphism tests ..."
  Isomorphism.spec
  log "Running monad tests ..."
  Monad.spec
  log "Running partial order tests ..."
  Ord.spec
