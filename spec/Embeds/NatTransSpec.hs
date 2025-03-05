module Embeds.NatTransSpec where

import "embeds" Embeds.NatTrans

import "hspec" Test.Hspec (Spec, describe, it)
import "transformers" Control.Monad.Trans.State
import "base" Data.Functor.Identity (Identity)

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

-- check that `Embeds Identity Identity` works

embedIdentityInIdentity :: Identity Int
embedIdentityInIdentity = do
  embed (pure 42 :: Identity Int)

-- check that `Embeds Identity (StateT Int IO)` works

embedIdentityInStateTIO :: StateT Int IO ()
embedIdentityInStateTIO = do
  embed (pure () :: Identity ())

spec :: Spec
spec =
  describe "Embeds.NatTrans" $ do
    it "compiles" $ do
      pure () :: IO ()
