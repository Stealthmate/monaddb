{-# LANGUAGE TemplateHaskell #-}
module TH where

import           Database.HDBC.PostgreSQL
import           Database.HDBC.Query.TH          (defineTableFromDB)
import           Database.HDBC.Schema.Driver     (typeMap)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Language.Haskell.TH

connectOptionsDefault :: String
connectOptionsDefault = concat
  [ "dbname='", "accountant", "'"
    , "host='", "127.0.0.1", "'"
    , "port='", "5555", "'"
  ]

connect :: IO Connection
connect =
  connectPostgreSQL $ concat [
  "user='", "accountant", "'"
  , "password='", "accountant", "'"
  ] <> connectOptionsDefault

convTypes :: [(String, TypeQ)]
convTypes = []

defineTable :: String -> [Name] -> Q [Dec]
defineTable =
  defineTableFromDB
    connect
    (driverPostgreSQL { typeMap = convTypes })
    "public"
