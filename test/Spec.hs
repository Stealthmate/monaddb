{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where


import TH
import Control.Monad.Catch
import Control.Monad.Database
import Control.Monad.Reader
import Database.HDBC
import Table
import Test.Hspec
import Debug.Trace

type Context = Maybe ConnWrapper

newtype MyMonad a = MyMonad { runMyMonad :: ReaderT Context IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Context
           , MonadThrow
           , MonadCatch )

instance MonadDatabase MyMonad where
  getConnection = fmap ConnWrapper <$> ask
  newConnection = ConnWrapper <$> liftIO connect
  withConnection mc = local (const mc)
  destroyConnection = liftIO . disconnect

runMyMonad' = flip runReaderT Nothing . runMyMonad

main :: IO ()
main = hspec $ do
  describe "transaction" $ do
    it "commits single" . runMyMonad' $ do
      execM "truncate table acc cascade;" []
      runTransaction $ do
        insertM insertAcc $ Acc { accname = "asd", currency = "asd" }
        pure ()
    it "rolls back on error" $ do
      runMyMonad' $ execM "truncate table acc cascade;" []
      let r = runMyMonad' . runTransaction $ do
            insertM insertAcc $ Acc { accname = "no", currency = "asd" }
            insertM insertAcc $ Acc { accname = "no", currency = "asd" }
            pure ()
      r `shouldThrow` (\(SomeException e) -> trace (show e) True)
