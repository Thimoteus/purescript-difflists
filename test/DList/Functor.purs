module Test.DList.Functor where

import Test.QuickCheck
import Debug.Trace
import Data.DList hiding (cons)

main = do
  trace "Identity:"
  quickCheck $ \ dlx -> ((<$>) id) dlx == (dlx :: DList Number)

  trace "Composition:"
  quickCheck $ \ f g dlx -> (<$>) ((f :: Number -> Number) <<< (g :: Number -> Number)) dlx == ((f <$>) <<< (g <$>)) (dlx :: DList Number)
