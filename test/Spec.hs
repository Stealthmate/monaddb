{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where


import TH
import Control.Monad.Database
import Control.Monad.Reader
import Database.HDBC
import Table

type Context = Maybe ConnWrapper

newtype MyMonad a = MyMonad { runMyMonad :: ReaderT Context IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Context )

instance MonadDatabase MyMonad where
  getConnection = fmap ConnWrapper <$> ask
  newConnection = ConnWrapper <$> liftIO connect
  withConnection mc = local (const mc)
  destroyConnection = liftIO . disconnect

main :: IO ()
main = flip runReaderT Nothing . runMyMonad $ do
  insertM insertAcc $ Acc { accname = "asd", currency = "asd" }
  pure ()
