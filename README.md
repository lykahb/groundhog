## Interact with relational databases in Haskell

Migrate and access PostgreSQL, MySQL, and SQLite with type safety and
detailed control.

## Usage

* Check out full list of [examples](examples).
* Read [docs](http://hackage.haskell.org/package/groundhog) for
groundhog on Hackage.
* Read [docs](http://hackage.haskell.org/package/groundhog-th) for the
`mkPersist` DSL on Hackage.

### Creating and migrating tables

Here is a simple example of a database of Machines and Parts where
Machines have many Parts. It creates the tables and links them together.

```haskell
data Machine = Machine { modelName :: String, cost :: Double } deriving Show
data Part = Part { partName :: String, weight :: Maybe Int, machine :: DefaultKey Machine }
deriving instance Show Part

mkPersist defaultCodegenConfig [groundhog|
- entity: Machine
- entity: Part
|]

main = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration defaultMigrationLogger $ do
    migrate (undefined :: Machine)
    migrate (undefined :: Part)
```

### Inserting values

```haskell
megatron <- insert $ Machine "Megatron 5000" 2500.00
insert $ Part "Megamaker" 50 megatron
insert $ Part "Tiny Bolt" 1 megatron

microtron <- insert $ Machine "Microtron 12" 19.99
insert $ Part "Insignificonium" 2 microtron
```

### Querying results

```haskell
partsForMegatron <- select $ (MachineField ==. megatron) `orderBy` [Asc PartNameField]
liftIO $ putStrLn $ "Parts for the Megatron: " ++ show partsForMegatron
```
