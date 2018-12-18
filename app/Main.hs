{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where


import           Control.Monad.Database
import           Control.Monad.Database.Type
import           Control.Monad.Reader
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL

type Context = Maybe ConnWrapper

newtype MyMonad a = MyMonad { runMyMonad :: ReaderT Context IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Context )

instance MonadDatabase MyMonad where
  getConnection = fmap ConnWrapper <$> ask
  newConnection = ConnWrapper . fromJust <$> ask
  withConnection mc = local (const mc)

main :: IO ()
main = flip runReaderT Nothing . runMyMonad $ do
  pure ()
