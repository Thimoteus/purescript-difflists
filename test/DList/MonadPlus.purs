module Test.DList.MonadPlus where

import Test.QuickCheck
import Debug.Trace
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.DList hiding (cons)

c :: DList String -> DList String
c = id

c' :: DList Number -> DList Number
c' = id

cf :: (String -> DList Number) -> String -> DList Number
cf = id

main = do
  trace "Distributivity:"
  quickCheck $ \ x y f -> ((c x <|> y) >>= (cf f)) == ((x >>= f) <|> (y >>= f))

  trace "Annihilation:"
  quickCheck $ \ f -> ((c empty) >>= cf f) == (c' empty)
