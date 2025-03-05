{-# LANGUAGE RebindableSyntax #-}

module Embeds.RebindedDoSpec where

import Embeds.NatTransSpec

import "embeds"Embeds.RebindedDo

import "transformers" Control.Monad.Trans.State
import "base" Prelude hiding ((>>), (>>=))
import "hspec" Test.Hspec (Spec, describe, it)

-- test that we don't need `embed` with `RebindableSyntax`

combineTheTwoRebinded :: StateT Int IO ()
combineTheTwoRebinded = do
  ioOperation
  statefulOperation

spec :: Spec
spec = describe "Embeds.RebindedDo" $ do
  it "compiles" $ do
    pure () :: IO ()
