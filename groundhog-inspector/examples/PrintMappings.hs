import Database.Groundhog.Inspector
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Data.List (isSuffixOf)

import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = withSqliteConn ":memory:" $ runDbConn $ do
  -- Initialize schema
  mapM_ (\s -> executeRaw False s []) [
    "CREATE TABLE customer (id INTEGER PRIMARY KEY NOT NULL, name VARCHAR NOT NULL, phone VARCHAR NOT NULL)",
    "CREATE TABLE booking (id INTEGER PRIMARY KEY NOT NULL, details VARCHAR, customer INTEGER NOT NULL, FOREIGN KEY(customer) REFERENCES customer(id))"]
  -- Collect tables for further analysis
  tables <- collectTables (\(_, name) -> not $ "_not_mapped" `isSuffixOf` name) Nothing
  -- Analyze tables
  let decs = generateData defaultDataCodegenConfig defaultReverseNamingStyle tables
  mappings <- generateMapping defaultReverseNamingStyle tables
  -- Print datatype declarations
  liftIO $ mapM_ (putStrLn . showData) $ concat $ map (uncurry (:)) $ Map.elems $ decs
  -- Remove parts of mapping that are defaults for the chosen naming style
  let mappings' = Map.intersectionWith (minimizeMapping suffixNamingStyle . fst) decs mappings
  -- Print mappings
  liftIO $ B.putStrLn $ showMappings $ Map.elems mappings'