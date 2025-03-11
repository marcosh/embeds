module Embeds.NatTransSpec where

import "embeds" Embeds.NatTrans

import "hspec" Test.Hspec (Spec, describe, it)
import "transformers" Control.Monad.Trans.State
import "base" Data.Functor.Identity (Identity)
import "transformers" Control.Monad.Trans.Maybe (MaybeT)
import "transformers" Control.Monad.Trans.Except (ExceptT)

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

-- check that `Embeds Maybe (MaybeT Int IO)` works

embedMaybeInMaybeT :: MaybeT IO ()
embedMaybeInMaybeT = do
  embed (pure () :: Maybe ())

-- check that `Embeds (Either e) (ExceptT e IO)` works

embedEitherInExceptT :: ExceptT String IO ()
embedEitherInExceptT = do
  embed (pure () :: Either String ())

spec :: Spec
spec =
  describe "Embeds.NatTrans" $ do
    it "compiles" $ do
      pure () :: IO ()
