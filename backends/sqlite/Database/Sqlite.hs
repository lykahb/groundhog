{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
module Database.Sqlite (
                         Database,
                         Statement,
                         Error,
                         StepResult(Row,
                                    Done),
                         SQLData(SQLInteger,
                                 SQLFloat,
                                 SQLText,
                                 SQLBlob,
                                 SQLNull),
                         ColumnType(IntegerColumn,
                                    FloatColumn,
                                    TextColumn,
                                    BlobColumn,
                                    NullColumn),
                         open,
                         close,
                         prepare,
                         step,
                         reset,
                         finalize,
                         bindBlob,
                         bindDouble,
                         bindInt,
                         bindInt64,
                         bindNull,
                         bindText,
                         bind,
                         column,
                         columns,
                         unsafeColumns
                        )
    where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.UTF8 as UTF8
import Data.Typeable
import Foreign
import Foreign.C


newtype Database = Database (Ptr ())
data Statement = Statement (Ptr ()) Database

type Error = Int

errorOK :: Error
errorOK = 0
errorRow :: Error
errorRow = 100
errorDone :: Error
errorDone = 101

data StepResult = Row | Done deriving (Eq, Show)

data ColumnType = IntegerColumn
                | FloatColumn
                | TextColumn
                | BlobColumn
                | NullColumn
                  deriving (Eq, Show)

data SQLData = SQLInteger Int64
             | SQLFloat Double
             | SQLText String
             | SQLBlob BS.ByteString
             | SQLNull
               deriving (Eq, Show, Typeable)

showError :: Error -> String
showError 0 = "ErrorOK"
showError 1 = "ErrorError"
showError 2 = "ErrorInternal"
showError 3 = "ErrorPermission"
showError 4 = "ErrorAbort"
showError 5 = "ErrorBusy"
showError 6 = "ErrorLocked"
showError 7 = "ErrorNoMemory"
showError 8 = "ErrorReadOnly"
showError 9 = "ErrorInterrupt"
showError 10 = "ErrorIO"
showError 11 = "ErrorNotFound"
showError 12 = "ErrorCorrupt"
showError 13 = "ErrorFull"
showError 14 = "ErrorCan'tOpen"
showError 15 = "ErrorProtocol"
showError 16 = "ErrorEmpty"
showError 17 = "ErrorSchema"
showError 18 = "ErrorTooBig"
showError 19 = "ErrorConstraint"
showError 20 = "ErrorMismatch"
showError 21 = "ErrorMisuse"
showError 22 = "ErrorNoLargeFileSupport"
showError 23 = "ErrorAuthorization"
showError 24 = "ErrorFormat"
showError 25 = "ErrorRange"
showError 26 = "ErrorNotADatabase"
showError 100 = "ErrorRow"
showError 101 = "ErrorDone"
showError x = "Unknown error: " ++ show x


decodeColumnType :: Int -> ColumnType
decodeColumnType 1 = IntegerColumn
decodeColumnType 2 = FloatColumn
decodeColumnType 3 = TextColumn
decodeColumnType 4 = BlobColumn
decodeColumnType 5 = NullColumn
decodeColumnType x = error $ "Unknown column type: " ++ show x

foreign import ccall "sqlite3_errmsg"
  errmsgC :: Ptr () -> IO CString
errmsg :: Database -> IO String
errmsg (Database database) = do
  message <- errmsgC database
  byteString <- BS.packCString message
  return $ UTF8.toString byteString

sqlError :: Maybe Database -> String -> Error -> IO a
sqlError maybeDatabase functionName err = do
  details <- case maybeDatabase of
               Just database -> do
                 details <- errmsg database
                 return $ ": " ++ details
               Nothing -> return "."
  fail $ "SQLite3 returned " ++ (showError err)
         ++ " while attempting to perform " ++ functionName
         ++ details

foreign import ccall "sqlite3_open"
  openC :: CString -> Ptr (Ptr ()) -> IO Int
openError :: String -> IO (Either Database Error)
openError path = do
  BS.useAsCString (UTF8.fromString path)
                  (\pathC -> do
                     alloca (\databaseC -> do
                               err <- openC pathC databaseC
                               if err == errorOK
                                 then do
                                   database <- peek databaseC
                                   return $ Left $ Database database
                                 else return $ Right err))
open :: String -> IO Database
open path = do
  databaseOrError <- openError path
  case databaseOrError of
    Left database -> return database
    Right err -> sqlError Nothing ("open " ++ show path) err

foreign import ccall "sqlite3_close"
  closeC :: Ptr () -> IO Int
closeError :: Database -> IO Error
closeError (Database database) = do
  err <- closeC database
  return err
close :: Database -> IO ()
close database = do
  err <- closeError database
  if err == errorOK
    then return ()
    else sqlError (Just database) "close" err

foreign import ccall "sqlite3_prepare_v2"
  prepareC :: Ptr () -> CString -> Int -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO Int
prepareError :: Database -> String -> IO (Either Statement Error)
prepareError db@(Database database) text = do
  BS.useAsCString (UTF8.fromString text)
                  (\textC -> do
                     alloca (\statementC -> do
                               err <- prepareC database textC (-1) statementC nullPtr
                               if err == errorOK
                                 then do
                                   statement <- peek statementC
                                   return $ Left $ Statement statement db
                                 else return $ Right err))
prepare :: Database -> String -> IO Statement
prepare database text = do
  statementOrError <- prepareError database text
  case statementOrError of
    Left statement -> return statement
    Right err -> sqlError (Just database) ("prepare " ++ (show text)) err

foreign import ccall "sqlite3_step"
  stepC :: Ptr () -> IO Int

step :: Statement -> IO StepResult
step (Statement statement database) = do
  err <- stepC statement
  if err == errorRow
    then return Row
    else if err == errorDone
      then return Done
      else sqlError (Just database) "step" err

foreign import ccall "sqlite3_reset"
  resetC :: Ptr () -> IO Int
reset :: Statement -> IO ()
reset (Statement statement database) = do
  err <- resetC statement
  if err == errorOK
    then return ()
    else sqlError (Just database) "reset" err

foreign import ccall "sqlite3_finalize"
  finalizeC :: Ptr () -> IO Int
finalize :: Statement -> IO ()
finalize (Statement statement database) = do
  err <- finalizeC statement
  if err == errorOK
    then return ()
    else sqlError (Just database) "finalize" err

foreign import ccall "sqlite3_bind_blob"
  bindBlobC :: Ptr () -> Int -> Ptr () -> Int -> Ptr () -> IO Int
bindBlob :: Statement -> Int -> BS.ByteString -> IO ()
bindBlob (Statement statement database) parameterIndex byteString = do
  size <- return $ BS.length byteString
  err <- BS.useAsCString byteString
                  (\dataC -> do
                     err <- bindBlobC statement parameterIndex (castPtr dataC) size
                                        (intPtrToPtr (-1))
                     return err)
  if err == errorOK
    then return ()
    else sqlError (Just database) "bind blob" err

foreign import ccall "sqlite3_bind_double"
  bindDoubleC :: Ptr () -> Int -> Double -> IO Int
bindDouble :: Statement -> Int -> Double -> IO ()
bindDouble (Statement statement database) parameterIndex datum = do
  err <- bindDoubleC statement parameterIndex datum
  if err == errorOK
    then return ()
    else sqlError (Just database) "bind double" err

foreign import ccall "sqlite3_bind_int"
  bindIntC :: Ptr () -> Int -> Int -> IO Int
bindInt :: Statement -> Int -> Int -> IO ()
bindInt (Statement statement database) parameterIndex datum = do
  err <- bindIntC statement parameterIndex datum
  if err == errorOK
    then return ()
    else sqlError (Just database) "bind int" err

foreign import ccall "sqlite3_bind_int64"
  bindInt64C :: Ptr () -> Int -> Int64 -> IO Int
bindInt64 :: Statement -> Int -> Int64 -> IO ()
bindInt64 (Statement statement database) parameterIndex datum = do
  err <- bindInt64C statement parameterIndex datum
  if err == errorOK
    then return ()
    else sqlError (Just database) "bind int64" err

foreign import ccall "sqlite3_bind_null"
  bindNullC :: Ptr () -> Int -> IO Int
bindNull :: Statement -> Int -> IO ()
bindNull (Statement statement database) parameterIndex = do
  err <- bindNullC statement parameterIndex
  if err == errorOK
    then return ()
    else sqlError (Just database) "bind null" err

foreign import ccall "sqlite3_bind_text"
  bindTextC :: Ptr () -> Int -> CString -> Int -> Ptr () -> IO Int
bindText :: Statement -> Int -> String -> IO ()
bindText (Statement statement database) parameterIndex text = do
  byteString <- return $ UTF8.fromString text
  size <- return $ BS.length byteString
  err <- BS.useAsCString byteString
                  (\dataC -> do
                     err <- bindTextC statement parameterIndex dataC size
                                        (intPtrToPtr (-1))
                     return err)
  if err == errorOK
    then return ()
    else sqlError (Just database) "bind text" err

bind :: Statement -> [SQLData] -> IO ()
bind statement sqlData = do
  mapM_ (\(parameterIndex, datum) -> do
          case datum of
            SQLInteger int64 -> bindInt64 statement parameterIndex int64
            SQLFloat double -> bindDouble statement parameterIndex double
            SQLText text -> bindText statement parameterIndex text
            SQLBlob blob -> bindBlob statement parameterIndex blob
            SQLNull -> bindNull statement parameterIndex)
       $ zip [1..] sqlData
  return ()

foreign import ccall "sqlite3_column_type"
  columnTypeC :: Ptr () -> Int -> IO Int
columnType :: Statement -> Int -> IO ColumnType
columnType (Statement statement _) columnIndex = do
  result <- columnTypeC statement columnIndex
  return $ decodeColumnType result

foreign import ccall "sqlite3_column_bytes"
  columnBytesC :: Ptr () -> Int -> IO Int

foreign import ccall "sqlite3_column_blob"
  columnBlobC :: Ptr () -> Int -> IO (Ptr ())
columnBlob :: Statement -> Int -> IO BS.ByteString
columnBlob (Statement statement _) columnIndex = do
  size <- columnBytesC statement columnIndex
  BSI.create size (\resultPtr -> do
                     dataPtr <- columnBlobC statement columnIndex
                     if dataPtr /= nullPtr
                        then BSI.memcpy resultPtr (castPtr dataPtr) (fromIntegral size)
                        else return ())

foreign import ccall "sqlite3_column_int64"
  columnInt64C :: Ptr () -> Int -> IO Int64
columnInt64 :: Statement -> Int -> IO Int64
columnInt64 (Statement statement _) columnIndex = do
  columnInt64C statement columnIndex

foreign import ccall "sqlite3_column_double"
  columnDoubleC :: Ptr () -> Int -> IO Double
columnDouble :: Statement -> Int -> IO Double
columnDouble (Statement statement _) columnIndex = do
  columnDoubleC statement columnIndex

foreign import ccall "sqlite3_column_text"
  columnTextC :: Ptr () -> Int -> IO CString
columnText :: Statement -> Int -> IO String
columnText (Statement statement _) columnIndex = do
  text <- columnTextC statement columnIndex
  byteString <- BS.packCString text
  return $ UTF8.toString byteString

foreign import ccall "sqlite3_column_count"
  columnCountC :: Ptr () -> IO Int
columnCount :: Statement -> IO Int
columnCount (Statement statement _) = do
  columnCountC statement

column :: Statement -> Int -> IO SQLData
column statement columnIndex = do
  theType <- columnType statement columnIndex
  typedColumn theType statement columnIndex

typedColumn :: ColumnType -> Statement -> Int -> IO SQLData
typedColumn theType statement columnIndex = do
  case theType of
    IntegerColumn -> do
                 int64 <- columnInt64 statement columnIndex
                 return $ SQLInteger int64
    FloatColumn -> do
                 double <- columnDouble statement columnIndex
                 return $ SQLFloat double
    TextColumn -> do
                 text <- columnText statement columnIndex
                 return $ SQLText text
    BlobColumn -> do
                 byteString <- columnBlob statement columnIndex
                 return $ SQLBlob byteString
    NullColumn -> return SQLNull

columns :: Statement -> IO [SQLData]
columns statement = do
  count <- columnCount statement
  mapM (\i -> column statement i) [0..count-1]

unsafeColumns :: Statement -> [Maybe ColumnType] -> IO [SQLData]
unsafeColumns statement types = go 0 types where
  go :: Int -> [Maybe ColumnType] -> IO [SQLData]
  go _ [] = return []
  go n (t:ts) = do
    c <- (maybe column typedColumn t) statement n
    cs <- go (n + 1) ts
    return (c:cs)
