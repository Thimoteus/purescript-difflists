module Test.DList.Monoid where

import Prelude

import Test.QuickCheck
import Effect.Console (log)
import Data.Monoid
import Data.DList

spec = do
  log "Associativity:"
  quickCheck $ \ dla dlb dlc -> (dla <> dlb) <> dlc == dla <> (dlb <> (dlc :: DList Number))

  log "Identity:"
  quickCheck $ \ dla -> dla <> mempty == dla :: DList String
