module Test.DList.Monad where

import Test.QuickCheck
import Debug.Trace
import Data.DList hiding (cons)

cx :: DList Boolean -> DList Boolean
cx = id

cf :: (Boolean -> DList String) -> Boolean -> DList String
cf = id

cg :: (String -> DList Number) -> String -> DList Number
cg = id

main = do
  trace "Associativity:"
  quickCheck $ \ x f g -> ((cx x >>= cf f) >>= cg g) == (x >>= (\ k -> f k >>= g))

  trace "Left identity:"
  quickCheck $ \ x f -> (pure (x :: Boolean) >>= (cf f)) == f x

  trace "Right identity:"
  quickCheck $ \ x -> (x >>= pure) == (cx x)
