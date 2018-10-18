module Test.DList.Monad where

import Prelude

import Test.QuickCheck
import Effect.Console (log)
import Data.DList

cx :: DList Boolean -> DList Boolean
cx = identity

cf :: (Boolean -> DList String) -> Boolean -> DList String
cf = identity

cg :: (String -> DList Number) -> String -> DList Number
cg = identity

spec = do
  log "Associativity:"
  quickCheck $ \ x f g -> ((cx x >>= cf f) >>= cg g) == (x >>= (\ k -> f k >>= g))

  log "Left identity:"
  quickCheck $ \ x f -> (pure (x :: Boolean) >>= (cf f)) == f x

  log "Right identity:"
  quickCheck $ \ x -> (x >>= pure) == (cx x)
