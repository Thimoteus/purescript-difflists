module Test.DList.Isomorphism where

import Prelude

import Test.QuickCheck
import Control.Monad.Eff.Console
import Data.DList
import Data.List(List(..))
import Test.DList.Instances

f :: forall a. DList a -> List a
f = dlist2List

g :: forall a. List a -> DList a
g = list2DList

main = do
  log "list2DList is the inverse of dlist2List"
  quickCheck $ \ xs -> (f <<< g $ xs) == (xs :: List Number)

  log "dlist2List is the inverse of list2DList"
  quickCheck $ \ dlx -> (g <<< f $ dlx) == (dlx :: DList Number)

  log "dlist2List is a homomorphism to (List, ++)"
  quickCheck $ \ dlx dly -> f (dlx >< dly) == (f dlx) ++ (f dly :: List Number)

  log "list2DList is a homomorphism to (DList, ><)"
  quickCheck $ \ xs ys -> g (xs ++ ys) == (g xs) >< (g ys :: DList Number)

