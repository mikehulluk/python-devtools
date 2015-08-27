
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

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE



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

data File = File {
      filename :: String 
    , tags :: [String]
    , project :: Project
    } deriving (Show)




getHDTConfigPath :: IO(String)
getHDTConfigPath  = do
    homeDir <- getHomeDirectory
    let thepath = homeDir ++ "/" ++ (".hdt/")
    createDirectoryIfMissing True thepath
    return thepath






--data DBFile = Int String seriving (Show)
--instance 









sampleConfigFileContents :: IO( LB.ByteString )
sampleConfigFileContents = do
    contents <- LB.readFile "/home/michael/hw/python-devtools/hdt/src/configfile.json.sample"
    --contents <- LB.readFile "/home/mike/dev/python-devtools/hdt/src/configfile.json.sample"
    return $ contents



data ConfigFileSetup = MHNothing | ConfigFileSetup {
    projects :: [Project]

} deriving (P.Show)





instance FromJSON HdtTypes.FileSelector where
    parseJSON (Object o) = do
        globString <- o .: "glob"
        addTags' <- o .: "tags"
        return $ HdtTypes.FileSelector{HdtTypes.globString=globString,HdtTypes.addTags=addTags'}


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


