module Test.DList.Functor where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Data.DList 

main = do
  log "Identity:"
  quickCheck $ \ dlx -> ((<$>) id) dlx == (dlx :: DList Number)

  log "Composition:"
  quickCheck $ \ f g dlx -> (<$>) ((f :: Number -> Number) <<< (g :: Number -> Number)) dlx == ((f <$>) <<< (g <$>)) (dlx :: DList Number)
