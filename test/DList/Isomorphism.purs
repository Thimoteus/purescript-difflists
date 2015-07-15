module Test.DList.Isomorphism where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Data.DList

f :: forall a. DList a -> Array a
f = fromDList

g :: forall a. Array a -> DList a
g = toDList

main = do
  log "toDList is the inverse of fromDList"
  quickCheck $ \ xs -> (f <<< g $ xs) == (xs :: Array Number)

  log "fromDList is the inverse of toDList"
  quickCheck $ \ dlx -> (g <<< f $ dlx) == (dlx :: DList Number)

  log "fromDList is a homomorphism to ([], ++)"
  quickCheck $ \ dlx dly -> f (dlx >< dly) == (f dlx) ++ (f dly :: Array Number)

  log "toDList is a homomorphism to (DList, ><)"
  quickCheck $ \ xs ys -> g (xs ++ ys) == (g xs) >< (g ys :: DList Number)

