module Test.DList.Isomorphism where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Data.DList
import Data.List(List(..))

f :: forall a. DList a -> List a
f = dlist2List

g :: forall a. List a -> DList a
g = list2DList

main = do
  log "list2DList is the inverse of dlist2List"
  quickCheck $ \ xs -> (f <<< g $ xs) == (xs :: List Number)

  log "fromDList is the inverse of toDList"
  quickCheck $ \ dlx -> (g <<< f $ dlx) == (dlx :: DList Number)

  log "fromDList is a homomorphism to ([], ++)"
  quickCheck $ \ dlx dly -> f (dlx >< dly) == (f dlx) ++ (f dly :: List Number)

  log "toDList is a homomorphism to (DList, ><)"
  quickCheck $ \ xs ys -> g (xs ++ ys) == (g xs) >< (g ys :: DList Number)

