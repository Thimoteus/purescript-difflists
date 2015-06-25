module Test.DList.Ord where

import Test.QuickCheck
import Debug.Trace
import Data.DList hiding (cons)

main = do
  trace "Reflexivity:"
  quickCheck $ \ dlx -> dlx <= (dlx :: DList Number)

  trace "Antisymmetry:"
  quickCheck $ \ dlx dly -> if dlx <= dly && dly <= dlx then dlx == dly else (dlx :: DList Number) /= (dly :: DList Number)

  trace "Transitivity:"
  quickCheck $ \ dlx dly dlz -> if (dlx :: DList Number) <= (dly :: DList Number) && dly <= (dlz :: DList Number) then dlx <= dlz else (dlx > dly) || (dly > dlz)
