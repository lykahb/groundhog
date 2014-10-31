{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, FlexibleInstances #-}
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.TH.Settings
import Database.Groundhog.Sqlite

data Table = Create {select :: String, update :: Int, fubar :: String} deriving (Eq, Show)

mkPersist defaultCodegenConfig $ PersistDefinitions [
  PSEntityDef' $ PSEntityDef "Table" Nothing Nothing Nothing Nothing $ Just [
    PSConstructorDef "Create" Nothing Nothing Nothing (Just [
        PSFieldDef "select" (Just "SELECT") Nothing Nothing Nothing Nothing Nothing
      , PSFieldDef "fubar" (Just "BEGIN COMMIT") Nothing Nothing Nothing Nothing Nothing
      ])
      Nothing
    ]
  ]

main = withSqliteConn ":memory:" $ runDbConn $ do
  let table = Create "DROP" maxBound "DELETE"
  runMigration $ migrate table
  k <- insert table
  get k >>= liftIO . print
