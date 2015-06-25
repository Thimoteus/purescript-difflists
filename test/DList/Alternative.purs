module Test.DList.Alternative where

import Test.QuickCheck
import Debug.Trace
import Control.Alt
import Control.Plus
import Control.Alternative
import Data.DList hiding (cons)

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
  trace "Associativity:"
  quickCheck $ \ x y z -> ((c x <|> c y) <|> c z) == (x <|> (y <|> z))

  trace "Distributivity of map:"
  quickCheck $ \ f x y -> ((cf f) <$> (c x <|> c y)) == ((f <$> x) <|> (f <$> y))

  -- Plus
  trace "Left identity:"
  quickCheck $ \ x -> (empty <|> x) == c x

  trace "Right identity:"
  quickCheck $ \ x -> (x <|> empty) == c x

  trace "Annihilation of map:"
  quickCheck $ \ f -> ((cf f) <$> (c empty)) == (c' empty)

  trace "Distributivity of apply:"
  quickCheck $ \ f g x -> ((cg f <|> cg g) <*> x) == ((f <*> x) <|> (g <*> x))

  trace "Annihilation of apply:"
  quickCheck $ \ f -> ((cg empty) <*> (c f)) == (c'' empty)

