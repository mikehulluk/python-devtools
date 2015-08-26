
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module HdtTypes where

import qualified  Prelude as P
import Prelude (String, Bool, IO, Bool(True,False), return, (++), map )
import Prelude (putStrLn, ($))

import System.FilePath.Glob
import System.Directory
import Filesystem.Path
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data File = File {
      filename :: String
    , isClean  :: Bool
    }

data Project = Project {
      projectName :: String
    , rootDir :: String
    , isActive :: Bool
    , _rawSrcFiles :: [String] -- deprecated
    , fileSelectors :: [FileSelector]
}

data FileSelector = FileSelector {
      globString :: String
    , addTags :: [String]
}


findFiles :: String -> FileSelector -> IO([File])
findFiles rootDir fileSelector = do
    files <- globDir1 (compile $ globString fileSelector) rootDir
    return $ map _buildFile files
    where _buildFile s = File {filename=s, isClean=True}

srcFiles :: Project -> IO( [File] )
srcFiles  project = return $ map _buildFile (_rawSrcFiles project)
    where _buildFile s = File {filename=s, isClean=True}




getAllProjectConfigs :: IO [Project]
getAllProjectConfigs = do
    --files1 <- globDir1 (compile "*.hs") "/home/mike/dev/python-devtools/hdt/src/"
    let files1 = []
    files2 <- globDir1 (compile "src/**/*.hs") "/home/michael/hw/python-devtools/hdt/"
    let fs = FileSelector{ globString="src/**/*.hs", addTags=[]}
    return [
        Project { projectName ="Project1",
                  isActive=True,
                  rootDir="/home/michael/hw/python-devtools/hdt/",
                  _rawSrcFiles= (files1++files2), fileSelectors=[fs] },
        Project { projectName ="Project2",isActive=False,rootDir="dir2/",  _rawSrcFiles=["File3","File4"], fileSelectors=[] },
        Project { projectName ="Project3",isActive=False, rootDir="dir3/", _rawSrcFiles=["File4","File5"], fileSelectors=[] }
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

    -- Build the database tables, if they don't exist:
    conn <-open dbFilename
    execute_ conn "CREATE TABLE IF NOT EXISTS Files(id INTEGER PRIMARY KEY, filename TEXT);"
    execute_ conn "CREATE TABLE IF NOT EXISTS FilePatches(id INTEGER PRIMARY KEY, file INTEGER, timestamp INTEGER, description TEXT, blob TEXT);"
    return (conn )


