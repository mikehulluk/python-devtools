

{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RecordWildCards #-}
module HdtFilePatchStack where






import Database.SQLite.Simple




import Control.Applicative

import Data.Time.Clock.POSIX

import qualified Data.ByteString.Char8 as B

import HdtTypes
import HdtProject


-- TODO: replace the string concatentaion with "</>"
getDBFilename :: Project -> IO String
getDBFilename proj = do
    configPath <- getHDTConfigPath
    let path = configPath ++ "/" ++ (projectName proj ++ ".sqlite")
    return path



ensureFileInDB :: Connection -> File -> IO()
ensureFileInDB conn file = do
    let fname = filename file
    --putStrLn $ "Adding file:" ++ fname
    execute conn "INSERT OR IGNORE INTO Files (filename) VALUES (?);" (Only (fname :: String)) 
    return ()


getProjectDBHandle :: Project -> IO Connection
getProjectDBHandle proj = do
    dbFilename <- getDBFilename proj
    --putStrLn $ "Database file:" ++ dbFilename

    -- Build the database tables, if they don't exist:
    conn <-open dbFilename
    execute_ conn "CREATE TABLE IF NOT EXISTS Files(id INTEGER PRIMARY KEY, filename TEXT, UNIQUE(filename) );"
    execute_ conn "CREATE TABLE IF NOT EXISTS FilePatches(id INTEGER PRIMARY KEY, file_id INTEGER, insertionIdx INTEGER, timestamp INTEGER, description TEXT, blob TEXT, UNIQUE(file_id,insertionIdx));"

    -- Add entries for files, if they don't exist:
    files <- srcFiles proj
    mapM_ (ensureFileInDB conn) files

    -- Todo: trim out old entries:

    return conn


data DbFileEntry = DbFileEntry Int String deriving (Show)
data DbFilePatchEntry = DbFilePatchEntry {
      primaryKey :: Int
    , fileId :: Int
    , insertionIdx :: Int
    , timestamp :: Int
    , description :: String
    , blob :: B.ByteString
    } deriving (Show)




instance FromRow DbFileEntry where
    fromRow = DbFileEntry <$> field <*> field

instance ToRow DbFileEntry where
    toRow (DbFileEntry id_ fname) = toRow (id_, fname)

instance FromRow DbFilePatchEntry where
    fromRow = DbFilePatchEntry <$> field <*> field <*> field  <*> field  <*> field  <*> field

instance ToRow DbFilePatchEntry where
    toRow (DbFilePatchEntry primaryKey' fileId' insertionIdx' timestamp' description' blob') = toRow (primaryKey', fileId', insertionIdx', timestamp', description', blob')


getFilePatchs :: Connection -> File -> IO [DbFilePatchEntry] 
getFilePatchs conn file = do
    id_ <- getFileId conn file 
    r <- query conn "SELECT * FROM FilePatches where(file_id=?);" (Only (id_ :: Int))  :: IO [DbFilePatchEntry]
    return r



fileHasOutstandingPatchs :: Connection -> File -> IO Bool
fileHasOutstandingPatchs conn file = do
    patchs <- getFilePatchs conn file
    return $ null patchs --(length patchs == 0) 


getFileId :: Connection -> File -> IO Int
getFileId dbConn file = do
    let fname = filename file
    r <- query dbConn "SELECT * FROM Files where(filename=?);" (Only (fname :: String))  :: IO [DbFileEntry]
    case length r of
        1 -> return id_
            where DbFileEntry id_ _ = head r
        _ -> error ""


addFileOutstandingPatchs :: File -> String -> B.ByteString -> IO()
addFileOutstandingPatchs file description' newBlob = do
    -- putStrLn $ "Saving new file patchs for: " ++ filename file
    -- putStrLn  "New File:"
    -- putStrLn  newBlob

    let proj = project file
    --let fname = filename file
    dbConn <- getProjectDBHandle proj


    -- Get the file-id:
    id' <- getFileId dbConn file
    -- putStrLn $ "Found id: " ++ (show id_) ++ " for filename: " ++ fname

    -- Find existing patchs:
    patchs <- getFilePatchs dbConn file
    --putStrLn $ "Found existing patchs: "
    --putStrLn $ unlines $ map show patchs

    let insertionIdx' = (length patchs +1) * 10
    --let description = "DUMMY REPLACEMENT"
    timestamp' <- round `fmap` getPOSIXTime

    -- Create the new patch entry:
    --putStrLn $ "Running query"
    --let newBlob' = newBlob

    execute dbConn "INSERT OR IGNORE INTO FilePatches (file_id,insertionIdx,timestamp, description, blob) VALUES (?,?,?,?,?);"  (id' :: Int, insertionIdx' :: Int, timestamp' :: Int, description' :: String, newBlob :: B.ByteString ) 
    --putStrLn $ "Done Running query"

    return ()



dropOutstandingPatchs :: File -> IO()
dropOutstandingPatchs file = do
    let proj = project file
    let fname = filename file
    putStrLn $ "Dropping patchs for: " ++ fname
    dbConn <- getProjectDBHandle proj
    id_ <- getFileId dbConn file 
    query dbConn "DELETE FROM FilePatches where(file_id=?);" (Only (id_ :: Int))  :: IO [DbFilePatchEntry]
    return ()



