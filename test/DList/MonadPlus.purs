module Test.DList.MonadPlus where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.DList
import Test.DList.Instances

c :: DList String -> DList String
c = id

c' :: DList Number -> DList Number
c' = id

cf :: (String -> DList Number) -> String -> DList Number
cf = id

main = do
  log "Distributivity:"
  quickCheck $ \ x y f -> ((c x <|> y) >>= (cf f)) == ((x >>= f) <|> (y >>= f))

  log "Annihilation:"
  quickCheck $ \ f -> ((c empty) >>= cf f) == (c' empty)
