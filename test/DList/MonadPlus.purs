module Test.DList.MonadPlus where

import Prelude

import Test.QuickCheck
import Effect.Console (log)
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.DList

c :: DList String -> DList String
c = identity

c' :: DList Number -> DList Number
c' = identity

cf :: (String -> DList Number) -> String -> DList Number
cf = identity

spec = do
  log "Distributivity:"
  quickCheck $ \ x y f -> ((c x <|> y) >>= (cf f)) == ((x >>= f) <|> (y >>= f))

  log "Annihilation:"
  quickCheck $ \ f -> ((c empty) >>= cf f) == (c' empty)
