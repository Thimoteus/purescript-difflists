module Test.DList.Ord where

import Prelude

import Test.QuickCheck
import Effect.Console (log)
import Data.DList

spec = do
  log "Reflexivity:"
  quickCheck $ \ dlx -> dlx <= (dlx :: DList Number)

  log "Antisymmetry:"
  quickCheck $ \ dlx dly -> if dlx <= dly && dly <= dlx then dlx == dly else (dlx :: DList Number) /= (dly :: DList Number)

  log "Transitivity:"
  quickCheck $ \ dlx dly dlz -> if (dlx :: DList Number) <= (dly :: DList Number) && dly <= (dlz :: DList Number) then dlx <= dlz else (dlx > dly) || (dly > dlz)
