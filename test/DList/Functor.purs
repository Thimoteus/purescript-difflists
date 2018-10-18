module Test.DList.Functor where

import Prelude

import Test.QuickCheck
import Effect.Console (log)
import Data.DList 

spec = do
  log "Identity:"
  quickCheck $ \ dlx -> ((<$>) identity) dlx == (dlx :: DList Number)

  log "Composition:"
  quickCheck $ 
    \ f g dlx -> 
      (<$>) ((f :: Number -> Number) <<< (g :: Number -> Number)) 
        dlx == ((f <$> _) <<< (g <$>_)) (dlx :: DList Number)
