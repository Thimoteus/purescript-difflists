module Test.DList.Applicative where

import Test.QuickCheck
import Debug.Trace
import Data.DList hiding (cons)

cf :: (DList (String -> Number)) -> (DList (String -> Number))
cf = id

cg :: (DList (Boolean -> String)) -> (DList (Boolean -> String))
cg = id

ch :: (DList Boolean) -> (DList Boolean)
ch = id

main = do
  trace "Associative composition:"
  quickCheck $ \ f g h -> ((<<<) <$> (cf f) <*> (cg g) <*> (ch h)) == (f <*> (g <*> h))

  trace "Identity:"
  quickCheck $ \ v -> (pure id) <*> (ch v) == v

  trace "Composition:"
  quickCheck $ \ f g h -> (pure (<<<)) <*> (cf f) <*> (cg g) <*> (ch h) == f <*> (g <*> h)

  trace "Homomorphism:"
  quickCheck $ \ f x -> (pure f) <*> (pure x) == pure ((f :: String -> Boolean) (x :: String)) :: DList Boolean

  trace "Interchange:"
  quickCheck $ \ u y -> (cf u) <*> (pure (y :: String)) == (pure ($ y)) <*> u
