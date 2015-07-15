module Test.DList.Alternative where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Control.Alt
import Control.Plus
import Control.Alternative
import Data.DList

c :: DList String -> DList String
c = id

c' :: DList Boolean -> DList Boolean
c' = id

c'' :: DList Number -> DList Number
c'' = id

cf :: (String -> Boolean) -> String -> Boolean
cf = id

cg :: DList (String -> Number) -> DList (String -> Number)
cg = id

main = do
  -- Alt
  log "Associativity:"
  quickCheck $ \ x y z -> ((c x <|> c y) <|> c z) == (x <|> (y <|> z))

  log "Distributivity of map:"
  quickCheck $ \ f x y -> ((cf f) <$> (c x <|> c y)) == ((f <$> x) <|> (f <$> y))

  -- Plus
  log "Left identity:"
  quickCheck $ \ x -> (empty <|> x) == c x

  log "Right identity:"
  quickCheck $ \ x -> (x <|> empty) == c x

  log "Annihilation of map:"
  quickCheck $ \ f -> ((cf f) <$> (c empty)) == (c' empty)

  log "Distributivity of apply:"
  quickCheck $ \ f g x -> ((cg f <|> cg g) <*> x) == ((f <*> x) <|> (g <*> x))

  log "Annihilation of apply:"
  quickCheck $ \ f -> ((cg empty) <*> (c f)) == (c'' empty)

