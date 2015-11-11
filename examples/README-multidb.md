# Groundhog multidb example

This code is a simple, commented example of the
use of the
`[groundhog](https://github.com/lykahb/groundhog)`
Record-Relational Model (RRM) database interface for
Haskell.  The example shows the use of any one of a number
of database backends.

By default, data is "persisted" in an in-memory SQLite3
database that disappears on program exit. You may choose one
of several other options as an argument when running the
program:

* `sqlite`: Creates the database as "example.sqlite3" in the
  current directory.

* `mysql`: Connects to a MySQL server on the default port as
  user `test` and uses the database `groundhog_example`.
  This database must have been pre-created and permissions
  given to the test user.
  
* `postgresql`: Connects to a PostgresQL server on the
  default port as user `test` and uses the database
  `groundhog_example`.  This database must have been
  pre-created and permissions given to the test user.

The `mysql` and `postgresql` options are configurable by
environment variables:

* `GROUNDHOG\_EXAMPLE\_USERNAME`: Selects test user name.
* `GROUNDHOG\_EXAMPLE\_DATABASE`: Selects test database name.
* `GROUNDHOG\_EXAMPLE\_PASSWORD`: Supplies a password for the test user.

For the databases that actually persist, all rows of the
tables are deleted at the start of each run. This allows
post-run inspection of the database, but prevents
accumulation of users and products.
