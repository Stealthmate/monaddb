{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where


import           Control.Monad.Catch
import           Control.Monad.Database
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Either
import           Database.HDBC
import           Debug.Trace
import           Table
import           Table.Test1
import           Test.Hspec

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

runMyMonad' :: MyMonad a -> IO (Either String a)
runMyMonad' = handle (\(SomeException e) -> pure . Left $ show e) . fmap Right . flip runReaderT Nothing . runMyMonad

trunc = execM "truncate table test_1;" []

main :: IO ()
main = hspec $ before_ (void $ runMyMonad' trunc) $ do
  describe "transaction" $ do
    it "commits single" $ do
      r <- runMyMonad' $ do
        runTransaction $ do
          insertM insertTest1 $ Test1 { tid = 0, v1 = 0 }
          pure ()
      r `shouldBe` Right ()
    it "rolls back on error" $ do
      r <- runMyMonad' . runTransaction $ do
        insertM insertTest1 $ Test1 { tid = 0, v1 = 0 }
        insertM insertTest1 $ Test1 { tid = 0, v1 = 0 }
        pure ()
      r `shouldSatisfy` isLeft
      rs <- runMyMonad' $ queryM selectTest1 0
      rs `shouldBe` Right []
    it "transaction inside transaction does not commit" $ do
      r <- runMyMonad' $ do
        runTransaction $ do
          insertM insertTest1 $ Test1 { tid = 0, v1 = 0 }
          runTransaction $ do
            insertM insertTest1 $ Test1 { tid = 0, v1 = 0 }
        rs <- queryM selectTest1 0
        liftIO $ rs `shouldBe` []
      r `shouldSatisfy` isLeft
