{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Database
  ( runTransaction
  , insertM
  , bulkInsertM
  , insertQueryM
  , queryM
  , queryMaybeM
  , queryJustM
  , updateM
  , keyUpdateM
  , deleteM
  , execM
  , module Control.Monad.Database.Type
  ) where

import           Control.Monad.Database.Type
import Control.Monad.Catch
import           Control.Monad.IO.Class      (liftIO)
import           Data.Maybe                  (listToMaybe)
import           Database.HDBC
import           Database.HDBC.Record
import           Database.Record
import           Database.Relational

withConnection' :: (MonadDatabase m) => (ConnWrapper -> IO b) -> m b
withConnection' op = do
  c <- getConnection
  case c of
    Nothing -> do
      c' <- newConnection
      r <- liftIO $ op c'
      liftIO $ commit c'
      destroyConnection c'
      pure r
    Just c' -> liftIO $ op c'

runTransaction :: (MonadDatabase m) => m r -> m r
runTransaction op = do
  c <- getConnection
  case c of
    Nothing -> do
      c' <- newConnection
      r <- doTransaction op c'
      destroyConnection c'
      pure r
    Just c' -> doTransaction op c'
  where
    doTransaction op' conn = do
      r <- onException (withConnection (Just conn) op') (liftIO $ rollback conn)
      liftIO $ commit conn
      pure r

insertM ::(MonadDatabase m, ToSql SqlValue p) => Insert p -> p -> m Integer
insertM s p = withConnection' $ \c -> do
  runInsert c s p

bulkInsertM :: (MonadDatabase m, ToSql SqlValue p) => Insert p -> [p] -> m ()
bulkInsertM s p = withConnection' $ \c -> bulkInsert c s p

insertQueryM :: (MonadDatabase m, ToSql SqlValue p) => InsertQuery p -> p -> m Integer
insertQueryM s p = withConnection' $ \c -> runInsertQuery c s p

queryM :: (MonadDatabase m, ToSql SqlValue p, FromSql SqlValue r) => Query p r -> p -> m [r]
queryM q p = withConnection' $ \c -> runQuery c q p

queryMaybeM :: (MonadDatabase m, ToSql SqlValue p, FromSql SqlValue r) => Query p r -> p -> m (Maybe r)
queryMaybeM q p = listToMaybe <$> queryM q p

queryJustM :: (MonadDatabase m, ToSql SqlValue p, FromSql SqlValue r) => m r -> Query p r -> p -> m r
queryJustM e q p = do
  r <- queryMaybeM q p
  case r of
    Nothing -> e
    Just r  -> pure r

updateM :: (MonadDatabase m, ToSql SqlValue p) => Update p -> p -> m Integer
updateM s p = withConnection' $ \c -> runUpdate c s p

keyUpdateM :: (MonadDatabase m, ToSql SqlValue p) => KeyUpdate r p -> p -> m Integer
keyUpdateM s p = withConnection' $ \c -> runKeyUpdate c s p

deleteM :: (MonadDatabase m, ToSql SqlValue p) => Delete p -> p -> m Integer
deleteM s p = withConnection' $ \c -> runDelete c s p

execM :: (MonadDatabase m) => String -> [SqlValue] -> m Integer
execM s p = withConnection' $ \c -> run c s p
