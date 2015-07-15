module Test.DList.Applicative where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Data.DList

cf :: (DList (String -> Number)) -> (DList (String -> Number))
cf = id

cg :: (DList (Boolean -> String)) -> (DList (Boolean -> String))
cg = id

ch :: (DList Boolean) -> (DList Boolean)
ch = id

main = do
  log "Associative composition:"
  quickCheck $ \ f g h -> ((<<<) <$> (cf f) <*> (cg g) <*> (ch h)) == (f <*> (g <*> h))

  log "Identity:"
  quickCheck $ \ v -> (pure id) <*> (ch v) == v

  log "Composition:"
  quickCheck $ \ f g h -> (pure (<<<)) <*> (cf f) <*> (cg g) <*> (ch h) == f <*> (g <*> h)

  log "Homomorphism:"
  quickCheck $ \ f x -> (pure f) <*> (pure x) == pure ((f :: String -> Boolean) (x :: String)) :: DList Boolean

  log "Interchange:"
  quickCheck $ \ u y -> (cf u) <*> (pure (y :: String)) == (pure ($ y)) <*> u
