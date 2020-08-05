{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Database.Groundhog.Inspector
import Database.Groundhog.Sqlite
import Database.Groundhog.TH
import Database.Groundhog.TH.CodeGen
import Language.Haskell.TH (runIO)

do
  (decs, mappingSettings) <- runIO $
    withSqliteConn ":memory:" $
      runDbConn $ do
        mapM_
          (\s -> executeRaw False s [])
          [ "CREATE TABLE customer (id INTEGER PRIMARY KEY NOT NULL, name VARCHAR NOT NULL, phone VARCHAR NOT NULL)",
            "CREATE TABLE booking (id INTEGER PRIMARY KEY NOT NULL, details VARCHAR, customer INTEGER NOT NULL, FOREIGN KEY(customer) REFERENCES customer(id))"
          ]
        tables <- collectTables (const True) Nothing
        -- Generate datatype declarations
        let dataConfig = defaultDataCodegenConfig {mkType = sqliteMkType, generateUniqueKeysPhantoms = False}
            decs = generateData dataConfig defaultReverseNamingStyle tables
        -- Generate mapping configuration that has tables and column names and other database information
        mappingSettings <- generateMapping defaultReverseNamingStyle tables
        return (decs, mappingSettings)
  -- Function 'mkPersist' cannot be used here because it tries to reify datatypes which don't exist yet.
  -- So we manually combine declaration with mapping settings to generate mapping declarations
  let mkThEntity dec mapping = applyEntitySettings suffixNamingStyle mapping $ mkTHEntityDef suffixNamingStyle $ fst dec
      thEntities = Map.elems $ Map.intersectionWith mkThEntity decs mappingSettings
  -- Use mapping configuration to generate mapping declarations
  mappingDecs <- defaultMkEntityDecs thEntities
  migrateFunction <- mkMigrateFunction "migrateAll" thEntities
  return $ (concatMap (uncurry (:)) $ Map.elems decs) ++ mappingDecs ++ migrateFunction

deriving instance Show Customer

deriving instance Show Booking

main :: IO ()
main = withSqliteConn ":memory:" $
  runDbConn $ do
    runMigration migrateAll
    let customer = Customer "John Smith" "+1 123 456 1234"
    customerKey <- insert customer
    orderKey <- insert $ Booking (Just "first booking") customerKey
    get orderKey >>= liftIO . print
