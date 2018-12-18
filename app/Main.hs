{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main where


import           Control.Monad.Database
import           Control.Monad.Database.Type
import           Control.Monad.Reader
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Database.HDBC.Query.TH          (defineTableFromDB)
import           Database.HDBC.Schema.Driver     (typeMap)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Language.Haskell.TH

main :: IO ()
main = pure ()
