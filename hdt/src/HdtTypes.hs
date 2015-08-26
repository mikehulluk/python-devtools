
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module HdtTypes where

import qualified  Prelude as P
import Prelude (String, Bool, IO, Bool(True,False), return, (++), map )
import Prelude (putStrLn, ($), (==), Show)

import System.FilePath.Glob
import System.Directory
import Filesystem.Path hiding (filename)
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.List

data File = File {
      filename :: String 
    , isClean  :: Bool 
    , tags :: [String]
    } deriving (Show)

data Project = Project {
      projectName :: String
    , rootDir :: String
    , isActive :: Bool
    , _rawSrcFiles :: [String] -- deprecated
}


srcFiles :: Project -> [File]
srcFiles  project = map _buildFile ( sort $ _rawSrcFiles project)
    where _buildFile s = File {filename=s, isClean=True, tags=[]}



getAllProjectConfigs :: IO [Project] 
getAllProjectConfigs = do
    files1 <- globDir1 (compile "*.hs") "/home/mike/dev/python-devtools/hdt/src/"
    files2 <- globDir1 (compile "*.hs") "/home/michael/hw/python-devtools/hdt/src/"
    return [
        Project { projectName ="Project1",isActive=True, rootDir="dir1/",  _rawSrcFiles= (files1++files2) },
        Project { projectName ="Project2",isActive=False,rootDir="dir2/",  _rawSrcFiles=["File3","File4"] },
        Project { projectName ="Project3",isActive=False, rootDir="dir3/", _rawSrcFiles=["File4","File5"] }
        ]



getHDTConfigPath :: IO(String)
getHDTConfigPath  = do
    homeDir <- getHomeDirectory
    let thepath = homeDir ++ "/" ++ (".hdt/")
    createDirectoryIfMissing True thepath
    return thepath


-- TODO: replace the string concatentaion with "</>"
getDBFilename :: Project -> IO(String)
getDBFilename project = do
    configPath <- getHDTConfigPath
    let path = configPath ++"/" ++ (projectName project ++ ".sqlite")
    return path


getProjectDBHandle :: Project -> IO(Connection)
getProjectDBHandle project = do
    dbFilename <- getDBFilename project
    putStrLn $ "Database file:" ++ dbFilename

    -- Build the database tables, if they don't exist:
    conn <-open dbFilename
    execute_ conn "CREATE TABLE IF NOT EXISTS Files(id INTEGER PRIMARY KEY, filename TEXT);"
    execute_ conn "CREATE TABLE IF NOT EXISTS FilePatches(id INTEGER PRIMARY KEY, file INTEGER, timestamp INTEGER, description TEXT, blob TEXT);"

    -- Add entries for files, if they don't exist:
    --
    return (conn )


getFileChanges :: File -> IO( [String] )
getFileChanges file = do
    return []


fileHasOutstandingChanges :: File -> IO(Bool)
fileHasOutstandingChanges file = do
    changes <- getFileChanges file
    let isChanged = (changes == [])
    return isChanged




addFileOutstandingChanges :: File -> String -> IO()
addFileOutstandingChanges file newContents = 
    putStrLn $ "Saving new file changes for: " ++ filename file

