{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Control.Monad.Database.Type where

import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO)
import           Database.HDBC

class (Monad m, MonadIO m, MonadThrow m, MonadCatch m) => MonadDatabase m where
  getConnection :: m (Maybe ConnWrapper)
  newConnection :: m ConnWrapper
  destroyConnection :: ConnWrapper -> m ()
  withConnection :: Maybe ConnWrapper -> m a -> m a
