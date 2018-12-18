{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Control.Monad.Database.Type
  ( MonadDatabase
  , getConnection
  , newConnection
  , withConnection
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Database.HDBC

class (Monad m, MonadIO m) => MonadDatabase m where
  getConnection :: m (Maybe ConnWrapper)
  newConnection :: m ConnWrapper
  withConnection :: Maybe ConnWrapper -> m a -> m a
