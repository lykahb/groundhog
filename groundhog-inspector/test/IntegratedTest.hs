{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs, FlexibleInstances, StandaloneDeriving #-}
import Database.Groundhog.Inspector
import Database.Groundhog.Sqlite
import Database.Groundhog.TH
import Database.Groundhog.TH.CodeGen
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Language.Haskell.TH (runIO)

do
  (decs, mappings) <- runIO $ withSqliteConn ":memory:" $ runDbConn $ do
    mapM_ (\s -> executeRaw False s []) [
      "CREATE TABLE customer (id INTEGER PRIMARY KEY NOT NULL, name VARCHAR NOT NULL, phone VARCHAR NOT NULL)",
      "CREATE TABLE booking (id INTEGER PRIMARY KEY NOT NULL, details VARCHAR, customer INTEGER NOT NULL, FOREIGN KEY(customer) REFERENCES customer(id))"
      ]
    ts <- collectTables (const True) Nothing
    let decs = generateData defaultDataCodegenConfig {mkType = sqliteMkType, generateUniqueKeysPhantoms = False} defaultReverseNamingStyle ts
    mappings <- generateMapping defaultReverseNamingStyle ts
    return (decs, mappings)
  -- Function 'mkPersist' cannot be used here because it tries to reify datatypes which don't exist yet. So we manually combine declaration with mapping
  let entities = Map.elems $ Map.intersectionWith (\d m -> applyEntitySettings suffixNamingStyle m $ mkTHEntityDef suffixNamingStyle $ fst d) decs mappings
  mapped <- mapM defaultMkEntityDecs entities
  migrateFunction <- mkMigrateFunction "migrateAll" entities
  return $ migrateFunction ++ concat ((map (uncurry (:)) $ Map.elems decs) ++ mapped)

deriving instance Show Customer
deriving instance Show Booking

main :: IO ()
main = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration migrateAll
  let customer = Customer "John Smith" "+1 123 456 1234"
  customerKey <- insert customer
  orderKey <- insert $ Booking (Just "first booking") customerKey
  get orderKey >>= liftIO . print