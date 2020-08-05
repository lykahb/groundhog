{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

data Company = Company {name :: String, producedSkynetAndTerminator :: (Bool, Bool), headquarter :: Address, dataCentre :: Address, salesOffice :: Address} deriving (Eq, Show)

data Address = Address {city :: String, zipCode :: String, street :: String} deriving (Eq, Show)

mkPersist
  defaultCodegenConfig
  [groundhog|
definitions:
  - entity: Company
    constructors:
      - name: Company
        fields:
          - name: producedSkynetAndTerminator
            embeddedType:
              - name: val0
                dbName: producedSkynet
              - name: val1
                dbName: producedTerminator
          - name: headquarter
            embeddedType:               # If a field has an embedded type you can access its subfields. If you do it, the database columns will match with the embedded dbNames (no prefixing).
              - name: city              # Just a regular list of fields. However, note that you should use default dbNames of embedded
                dbName: hq_city
              - name: zip_code          # Here we use embedded dbName (zip_code) which differs from the name used in Address definition (zipCode) for accessing the field.
                dbName: hq_zipcode
              - name: street
                dbName: hq_street
          - name: dataCentre
            embeddedType:               # Similar declaration, but using another syntax for YAML objects
              - {name: city, dbName: dc_city}
              - {name: zip_code, dbName: dc_zipcode}
              - {name: street, dbName: dc_street}
                                        # Property embeddedType of salesOffice field is not mentioned, so the corresponding table columns will have names prefixed with salesOffice (salesOffice#city, salesOffice#zip_code, salesOffice#street)
  - embedded: Address
    dbName: Address                     # This name is used only to set polymorphic part of name of its container. E.g, persistName (a :: SomeData Address) = "SomeData#Address"
    fields:                             # The syntax is the same as for constructor fields. Nested embedded types are allowed.
      - name: city                      # This line does nothing and can be omitted. Default settings for city are not changed.
      - name: zipCode
        dbName: zip_code                # Change column name.
        exprName: ZipCodeSelector       # Set the default name explicitly
                                        # Street is not mentioned so it will have default settings.
 |]

main = withSqliteConn ":memory:" $
  runDbConn $ do
    let address = Address "Sunnyvale" "18144" "El Camino Real"
    let company = Company "Cyberdyne Systems" (False, False) address address address
    runMigration $ migrate company
    k <- insert company
    -- compare embedded data fields as a whole and compare their subfields individually
    select (DataCentreField ==. HeadquarterField &&. DataCentreField ~> ZipCodeSelector ==. HeadquarterField ~> ZipCodeSelector) >>= liftIO . print
    -- after the Cyberdyne headquarter was destroyed by John Connor and T-800, the Skynet development was continued by Cyber Research Systems affiliated with Pentagon
    let newAddress = Address "Washington" "20301" "1400 Defense Pentagon"
    -- compare fields with an embedded value as a whole and update embedded field with a value
    update [NameField =. "Cyber Research Systems", HeadquarterField =. newAddress] (NameField ==. "Cyberdyne Systems" &&. HeadquarterField ==. address)
    -- update embedded field with another field as a whole. Separate subfields can be accessed individually for update via ~> as in the select above
    update [DataCentreField =. HeadquarterField, SalesOfficeField =. HeadquarterField] (NameField ==. "Cyber Research Systems" &&. HeadquarterField ==. newAddress)
    -- eventually the skynet was developed. To access the elements of tuple we use predefined selectors. In Tuple2_0Selector 2 is arity of the tuple, 0 is number of element in it
    update [ProducedSkynetAndTerminatorField ~> Tuple2_0Selector =. True] (AutoKeyField ==. k)
    select (HeadquarterField ~> ZipCodeSelector ==. "20301") >>= liftIO . print
