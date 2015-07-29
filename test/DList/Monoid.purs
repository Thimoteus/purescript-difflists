module Test.DList.Monoid where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Data.Monoid
import Data.DList
import Test.DList.Instances

main = do
  log "Associativity:"
  quickCheck $ \ dla dlb dlc -> (dla <> dlb) <> dlc == dla <> (dlb <> (dlc :: DList Number))

  log "Identity:"
  quickCheck $ \ dla -> dla <> mempty == dla :: DList String
