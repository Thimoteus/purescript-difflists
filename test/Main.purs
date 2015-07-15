module Test.Main where

import Prelude

import Control.Monad.Eff.Console

main = do

  log "Running isomorphism tests ..."
  Test.DList.Isomorphism.main

  log "Running partial order tests ..."
  Test.DList.Ord.main

  log "Running monoid tests ..."
  Test.DList.Monoid.main

  log "Running functor tests ..."
  Test.DList.Functor.main

  log "Running applicative tests ..."
  Test.DList.Applicative.main

  log "Running monad tests ..."
  Test.DList.Monad.main

  log "Running alternative tests ..."
  Test.DList.Alternative.main

  log "Running monadplus tests ..."
  Test.DList.MonadPlus.main
