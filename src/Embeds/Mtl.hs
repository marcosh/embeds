{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Embeds.Mtl where

import Embeds.NatTrans (Embeds (..))

import "mtl" Control.Monad.Except (MonadError (..))
import "mtl" Control.Monad.Identity (Identity (runIdentity))
import "mtl" Control.Monad.Reader (MonadReader (..), Reader, ReaderT (..))
import "mtl" Control.Monad.State (MonadState (..), State, StateT (..))

class Depends e m | m -> e

-- MonadError

instance (MonadError e m) => Embeds (Either e) m where
  embed :: Either e a -> m a
  embed (Left e) = throwError e
  embed (Right a) = pure a

-- instance (Monad m, Embeds (Either e) m, Depends e m) => MonadError e m where
--   throwError :: e -> m a
--   throwError e = embed (Left e)

--   catchError :: m a -> (e -> m a) -> m a
--   catchError ma f = _

-- MonadReader

instance (MonadReader r m) => Embeds (Reader r) m where
  embed :: Reader r a -> m a
  embed (ReaderT f) = reader (runIdentity . f)

-- MonadState

instance (MonadState s m) => Embeds (State s) m where
  embed :: State s a -> m a
  embed (StateT f) = state (runIdentity . f)
