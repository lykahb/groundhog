## Interact with relational databases in Haskell [![Build Status](https://travis-ci.org/lykahb/groundhog.png?branch=master)](https://travis-ci.org/lykahb/groundhog)

PostgreSQL, MySQL, and SQLite with ultimate type safety.
Advanced migration capabilities allow you to precisely specify the schema description, whether for an existing database, or
creating a migration script for a new one. Groundhog is not opinionated about schema and can bind your datatypes to a almost any relational model. Schemas can include composite keys, references across multiple schemas, indexes, and more.
It is used in hobby projects and commercial applications.

## Docs

* [Tutorial](https://www.schoolofhaskell.com/user/lykahb/groundhog).
* Full list of [examples](examples).
* Read [docs](http://hackage.haskell.org/package/groundhog) for
groundhog on Hackage.
* Read [docs](http://hackage.haskell.org/package/groundhog-th/docs/Database-Groundhog-TH.html) for the
`mkPersist` mapping description on Hackage.

### Creating and migrating tables

Here is a simple example of a database of Machines and Parts where
Machines have many Parts. It creates the tables and links them together.

```haskell
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Machine = Machine { modelName :: String, cost :: Double } deriving Show
data Part = Part { partName :: String, weight :: Int, machine :: DefaultKey Machine }
deriving instance Show Part

mkPersist defaultCodegenConfig [groundhog|
- entity: Machine
- entity: Part
|]

main = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration $ do
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
megatronFromDB <- get megatron
liftIO $ putStrLn $ "Megatron from DB: " ++ show megatronFromDB
  
parts <- select $ (MachineField ==. megatron &&. lower PartNameField `notLike` "%tiny%") `orderBy` [Asc PartNameField]
liftIO $ putStrLn $ "Big parts for the Megatron: " ++ show parts
```

### User experience

> I was impressed with groundhog's approach from the very beginning. Now I've been using groundhog for the past 8 or 9 months and am very
pleased.  Groundhog's approach for mapping data types to database tables is powerful and composable while at the same time being quite
easy to use.  Its migration facilities allow me to iterate very quickly when building applications.  Boris's support has been
fantastic whenever I have encountered issues. Groundhog is definitely my first choice when I need a Haskell "ORM".

Doug Beardsley, Soostone Inc.

> Groundhog is an excellent library for relational databases in Haskell.  It uses quasi quotation to help you understand the details of your model, not to hide them from you.   
The converters feature allows us to make our "base" types light of type-class baggage while still getting the benefits of automated schema control.  
Thank you very much for your hard work on this Boris!

Scott Murphy, founder of Plow Technologies
