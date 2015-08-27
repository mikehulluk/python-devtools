
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

-- import HdtConfigFile
-- 
-- import qualified HdtTypes
-- import  HdtTypes as H

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE


data File = File {
      filename :: String 
    , isClean  :: Bool 
    , tags :: [String]
    } deriving (Show)

data Project = Project {
      projectName :: String
    , rootDir :: String
    , isActive :: Bool
    , fileSelectors :: [FileSelector]
} deriving (P.Show)

data FileSelector = FileSelector {
      globString :: String
    , addTags :: [String]
} deriving (P.Show)


findFiles :: String -> FileSelector -> IO( [File] )
findFiles rootDir fileSelector = do
    files <- globDir1 (compile $ globString fileSelector) rootDir
    return $ P.map _buildFile files
    where _buildFile s = File {filename=s, isClean=True}



srcFiles :: Project -> IO( [File] )
srcFiles project =  do
    files <- P.mapM (findFiles (rootDir project)) ( fileSelectors project )
    return $ P.concat files




getAllProjectConfigs :: IO [Project]
getAllProjectConfigs = do
    conf <- getConfigFileSetup
    return $ projects conf
    -- let fs = FileSelector{ globString="src/**/*.hs", addTags=[]}
    -- return [
    --     Project { projectName ="Project1",
    --               isActive=True,
    --               rootDir="/home/michael/hw/python-devtools/hdt/",
    --               fileSelectors=[fs] },
    --     Project { projectName ="Project2",isActive=False,rootDir="dir2/",  fileSelectors=[] },
    --     Project { projectName ="Project3",isActive=False, rootDir="dir3/", fileSelectors=[] }
    --     ]



getHDTConfigPath :: IO(String)
getHDTConfigPath  = do
    homeDir <- getHomeDirectory
    let thepath = homeDir ++ "/" ++ (".hdt/")
    createDirectoryIfMissing True thepath
    return thepath






--data DBFile = Int String seriving (Show)
--instance 


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












sampleConfigFileContents :: IO( LB.ByteString )
sampleConfigFileContents = do
    contents <- LB.readFile "/home/michael/hw/python-devtools/hdt/src/configfile.json.sample"
    return $ contents



data ConfigFileSetup = MHNothing | ConfigFileSetup {
    projects :: [Project]

} deriving (P.Show)





instance FromJSON HdtTypes.FileSelector where
    parseJSON (Object o) = do
        globString <- o .: "glob"
        return $ HdtTypes.FileSelector{HdtTypes.globString=globString,HdtTypes.addTags=[]}


instance FromJSON ConfigFileSetup where
    parseJSON (Object o) = do
        projects <- parseJSON =<< (o.: "projects")
        return $ ConfigFileSetup{projects=projects}
    parseJSON _ = mzero

instance FromJSON Project where
    parseJSON (Object v) = do
        name <- v .: "name"
        isActive <- v .:? "active" .!= False
        rootDir <- v .: "rootdir"
        fileSelectors <- parseJSON =<< (v.: "files")
        return Project{projectName=name, isActive=isActive, rootDir=rootDir, fileSelectors=fileSelectors}

    parseJSON _ = mzero



getConfigFileSetup :: IO(ConfigFileSetup)
getConfigFileSetup = do
    contents <- sampleConfigFileContents
    let mjson = eitherDecode contents
    case mjson of
        P.Left err -> do
            -- putStrLn
            putStrLn ("Unable to read Configfile: " ++ err)
            return MHNothing
        P.Right result ->  do
            P.putStrLn $ P.show result
            return result


