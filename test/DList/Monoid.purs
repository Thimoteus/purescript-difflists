module Test.DList.Monoid where

import Test.QuickCheck
import Debug.Trace
import Data.Monoid
import Data.DList hiding (cons)

main = do
  trace "Associativity:"
  quickCheck $ \ dla dlb dlc -> (dla <> dlb) <> dlc == dla <> (dlb <> (dlc :: DList Number))

  trace "Identity:"
  quickCheck $ \ dla -> dla <> mempty == dla :: DList String
