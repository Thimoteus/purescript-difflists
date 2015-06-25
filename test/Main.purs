module Test.Main where

import Debug.Trace (trace)

main = do

  trace "Running isomorphism tests ..."
  Test.DList.Isomorphism.main

  trace "Running partial order tests ..."
  Test.DList.Ord.main

  trace "Running monoid tests ..."
  Test.DList.Monoid.main

  trace "Running functor tests ..."
  Test.DList.Functor.main

  trace "Running applicative tests ..."
  Test.DList.Applicative.main

  trace "Running monad tests ..."
  Test.DList.Monad.main

  trace "Running alternative tests ..."
  Test.DList.Alternative.main

  trace "Running monadplus tests ..."
  Test.DList.MonadPlus.main
