module Embeds.NatTransSpec where

import "embeds" Embeds.NatTrans

import "hspec" Test.Hspec (Spec)
import "transformers" Control.Monad.Trans.State

-- check that `Embeds IO (StateT Int IO)` and `Embeds (State Int) (StateT Int IO)` work

ioOperation :: IO ()
ioOperation = print "Marco"

statefulOperation :: State Int ()
statefulOperation = state (\i -> ((), i + 1))

combineTheTwo :: StateT Int IO ()
combineTheTwo = do
  embed ioOperation
  embed statefulOperation

-- check that `Embeds (StateT Int IO) (StateT Int IO)` works

combineTheTwoAgain :: StateT Int IO ()
combineTheTwoAgain = do
  embed combineTheTwo

spec :: Spec
spec = pure ()
