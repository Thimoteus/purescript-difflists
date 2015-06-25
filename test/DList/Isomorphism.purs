module Test.DList.Isomorphism where

import Test.QuickCheck
import Debug.Trace
import Data.DList hiding (cons)

f :: forall a. DList a -> [a]
f = fromDList

g :: forall a. [a] -> DList a
g = toDList

main = do
  trace "toDList is the inverse of fromDList"
  quickCheck $ \ xs -> (f <<< g $ xs) == (xs :: [Number])

  trace "fromDList is the inverse of toDList"
  quickCheck $ \ dlx -> (g <<< f $ dlx) == (dlx :: DList Number)

  trace "fromDList is a homomorphism to ([], ++)"
  quickCheck $ \ dlx dly -> f (dlx >< dly) == (f dlx) ++ (f dly :: [Number])

  trace "toDList is a homomorphism to (DList, ><)"
  quickCheck $ \ xs ys -> g (xs ++ ys) == (g xs) >< (g ys :: DList Number)

