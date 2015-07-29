module Test.DList.Monad where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Data.DList
import Test.DList.Instances

cx :: DList Boolean -> DList Boolean
cx = id

cf :: (Boolean -> DList String) -> Boolean -> DList String
cf = id

cg :: (String -> DList Number) -> String -> DList Number
cg = id

main = do
  log "Associativity:"
  quickCheck $ \ x f g -> ((cx x >>= cf f) >>= cg g) == (x >>= (\ k -> f k >>= g))

  log "Left identity:"
  quickCheck $ \ x f -> (pure (x :: Boolean) >>= (cf f)) == f x

  log "Right identity:"
  quickCheck $ \ x -> (x >>= pure) == (cx x)
