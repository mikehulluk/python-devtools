

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module HdtFileChangeStack where

-- import qualified  Prelude as P
-- import Prelude (String, Bool, IO, Bool(True,False), return, (++), map )
-- import Prelude (putStrLn, ($), (==), Show)

import System.FilePath.Glob
import System.Directory
import Filesystem.Path hiding (filename)
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.List

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Time.Clock.POSIX
--import Data.Text as T
--import qualified Data.ByteString.Lazy as LB
--import qualified Data.Text.Encoding as TE


import HdtTypes
import HdtProject


-- TODO: replace the string concatentaion with "</>"
getDBFilename :: Project -> IO(String)
getDBFilename project = do
    configPath <- getHDTConfigPath
    let path = configPath ++"/" ++ (projectName project ++ ".sqlite")
    return path



ensureFileInDB :: Connection -> File -> IO()
ensureFileInDB conn file = do
    let fname = filename file
    --putStrLn $ "Adding file:" ++ fname
    execute conn "INSERT OR IGNORE INTO Files (filename) VALUES (?);" (Only (fname :: String)) 
    return ()


getProjectDBHandle :: Project -> IO(Connection)
getProjectDBHandle project = do
    dbFilename <- getDBFilename project
    --putStrLn $ "Database file:" ++ dbFilename

    -- Build the database tables, if they don't exist:
    conn <-open dbFilename
    execute_ conn "CREATE TABLE IF NOT EXISTS Files(id INTEGER PRIMARY KEY, filename TEXT, UNIQUE(filename) );"
    execute_ conn "CREATE TABLE IF NOT EXISTS FilePatches(id INTEGER PRIMARY KEY, file_id INTEGER, insertionIdx INTEGER, timestamp INTEGER, description TEXT, blob TEXT, UNIQUE(file_id,insertionIdx));"

    -- Add entries for files, if they don't exist:
    files <- srcFiles project
    mapM (ensureFileInDB conn) files

    -- Todo: trim out old entries:

    return (conn )


data DbFileEntry = DbFileEntry Int String deriving (Show)
data DbFilePatchEntry = DbFilePatchEntry {
      primaryKey :: Int
    , fileId :: Int
    , insertionIdx :: Int
    , timestamp :: Int
    , description :: String
    , blob :: String
    } deriving (Show)


instance FromRow DbFileEntry where
    fromRow = DbFileEntry <$> field <*> field

instance ToRow DbFileEntry where
    toRow (DbFileEntry id_ filename) = toRow (id_, filename)

instance FromRow DbFilePatchEntry where
    fromRow = DbFilePatchEntry <$> field <*> field <*> field  <*> field  <*> field  <*> field

instance ToRow DbFilePatchEntry where
    toRow (DbFilePatchEntry primaryKey fileId insertionIdx timestamp description blob) = toRow (primaryKey, fileId, insertionIdx, timestamp, description, blob)


getFileChanges :: Connection -> File -> IO( [DbFilePatchEntry] )
getFileChanges conn file = do
    id_ <- getFileId conn file 
    r <- query conn "SELECT * FROM FilePatches where(file_id=?);" (Only (id_ :: Int))  :: IO [DbFilePatchEntry]
    return r



fileHasOutstandingChanges :: Connection -> File -> IO(Bool)
fileHasOutstandingChanges conn file = do
    changes <- getFileChanges conn file
    let isChanged = (length changes == 0)
    return isChanged


getFileId :: Connection -> File -> IO(Int)
getFileId dbConn file = do
    let fname = filename file
    r <- query dbConn "SELECT * FROM Files where(filename=?);" (Only (fname :: String))  :: IO [DbFileEntry]
    case length r of
        1 -> return $ id_
            where DbFileEntry id_ _ = head r
        otherwise -> error ""


addFileOutstandingChanges :: File -> String -> String -> IO()
addFileOutstandingChanges file description newBlob = do
    putStrLn $ "Saving new file changes for: " ++ filename file
    putStrLn  "New File:"
    putStrLn  newBlob

    let proj = project file
    let fname = filename file
    dbConn <- getProjectDBHandle proj


    -- Get the file-id:
    id_ <- getFileId dbConn file
    putStrLn $ "Found id: " ++ (show id_) ++ " for filename: " ++ fname

    -- Find existing changes:
    changes <- getFileChanges dbConn file
    putStrLn $ "Found existing changes: "
    putStrLn $ unlines $ map show changes

    let insertionIdx = ((length changes) +1) * 10
    --let description = "DUMMY REPLACEMENT"
    timestamp <- round `fmap` getPOSIXTime




    -- Create the new change entry:
    putStrLn $ "Running query"
    execute dbConn "INSERT OR IGNORE INTO FilePatches (file_id,insertionIdx,timestamp, description, blob) VALUES (?,?,?,?,?);"  (id_ :: Int, insertionIdx :: Int, timestamp :: Int, description :: String, newBlob :: String ) 
    putStrLn $ "Done Running query"

    return ()






