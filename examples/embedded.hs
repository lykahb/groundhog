{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes #-}
import Control.Monad.IO.Class(liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Company = Company {name :: String, headquarter :: Address, dataCentre :: Address, salesOffice :: Address} deriving (Eq, Show)
data Address = Address {city :: String, zipCode :: String, street :: String} deriving (Eq, Show)

mkPersist suffixNamingStyle [groundhog|
definitions:
  - entity: Company
    constructors:
      - name: Company
        fields:
                                        # Property embeddedType of headquarter field is not mentioned, so the corresponding table columns will have names prefixed with headquarter (headquarter$city, headquarter$zip_code, headquarter$street)
          - name: dataCentre
            embeddedType:               # If a field has an embedded type you can access its subfields. If you do it, the database columns will match with the embedded dbNames (no prefixing).
              - name: city              # Just a regular list of fields. However, note that you should use default dbNames of embedded
                dbName: dc_city
              - name: zip_code          # Here we use embedded dbName (zip_code) which differs from the name used in Address definition (zipCode) for accessing the field.
                dbName: dc_zipcode
              - name: street
                dbName: dc_street
          - name: salesOffice
            embeddedType:               # Similar declaration, but using another syntax for YAML objects
              - {name: city, dbName: sales_city}
              - {name: zip_code, dbName: sales_zipcode}
              - {name: street, dbName: sales_street}
  - embedded: Address                        
    fields:                             # The syntax is the same as for constructor fields. Nested embedded types are allowed.
      - name: city                      # This line does nothing and can be omitted. Default settings for city are not changed.
      - name: zipCode
        dbName: zip_code                # Change column name.
                                        # Street is not mentioned so it will have default settings.
 |]

main = do
  withSqliteConn ":memory:" $ runSqliteConn $ do
  let commonAddress = Address "Sunnyvale" "18144" "El Camino Real"
  let company = Company "Cyberdyne Systems" commonAddress commonAddress commonAddress
  runMigration defaultMigrationLogger $ migrate company
  insert company
  -- compare embedded data fields as a whole and compare their subfields individually
  select (DataCentreField ==. HeadquarterField &&. DataCentreField ~> ZipCodeSelector ==. HeadquarterField ~> ZipCodeSelector) [] 0 0 >>= liftIO . print
  -- After the Cyberdyne headquarter was destroyed by John Connor and T-800, the Skynet development was continued by Cyber Research Systems affiliated with Pentagon
  let newAddress = Address "Washington" "20301" "1400 Defense Pentagon"
  -- compare fields with an embedded value as a whole and update embedded field with a value
  update [NameField =. "Cyber Research Systems", HeadquarterField =. newAddress] (NameField ==. "Cyberdyne Systems" &&. HeadquarterField ==. commonAddress)
  -- update embedded field with another field as a whole. Separate subfields can be accessed individually for update via ~> as in the select above
  update [DataCentreField =. HeadquarterField, SalesOfficeField =. HeadquarterField] (NameField ==. "Cyber Research Systems" &&. HeadquarterField ==. newAddress)
  select (HeadquarterField ~> ZipCodeSelector ==. "20301") [] 0 0 >>= liftIO . print