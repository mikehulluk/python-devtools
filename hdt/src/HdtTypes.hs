
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HdtTypes where

import System.Directory
--import Filesystem.Path hiding (filename)
--import Control.Applicative
import Data.List

import Data.Aeson
import Control.Monad
import qualified Data.ByteString.Lazy as LB



data Project = Project {
      projectName :: String
    , rootDir :: String
    , isActive :: Bool
    , fileSelectors :: [FileSelector]
} deriving (Show)

data FileSelector = FileSelector {
      globString :: String
    , addTags :: [String]
} deriving (Show)

data File = File {
      filename :: String 
    , tags :: [String]
    , project :: Project
    } deriving (Show)

data ConfigFileSetup = MHNothing | ConfigFileSetup {
    projects :: [Project]

} deriving (Show)









relativeFilename :: File -> String
relativeFilename File{..} = if rootDir project `isPrefixOf` filename then drop (length $ rootDir project) filename else filename

getHDTConfigPath :: IO String
getHDTConfigPath  = do
    homeDir <- getHomeDirectory
    let hdtPath = homeDir ++ "/.hdt/"
    createDirectoryIfMissing True hdtPath
    return hdtPath




sampleConfigFileContents :: IO LB.ByteString 
sampleConfigFileContents = do
    contents <- LB.readFile "/home/michael/hw/python-devtools/hdt/src/configfile.json.sample"
    --contents <- LB.readFile "/home/mike/dev/python-devtools/hdt/src/configfile.json.sample"
    return contents








instance FromJSON HdtTypes.FileSelector where
    parseJSON (Object o) = do
        globString <- o .: "glob"
        addTags <- o .: "tags"
        return HdtTypes.FileSelector{..}


instance FromJSON ConfigFileSetup where
    parseJSON (Object o) = do
        projects <- parseJSON =<< (o.: "projects")
        return ConfigFileSetup{..}
    parseJSON _ = mzero

instance FromJSON Project where
    parseJSON (Object v) = do
        projectName <- v .: "name"
        isActive <- v .:? "active" .!= False
        rootDir <- v .: "rootdir"
        fileSelectors <- parseJSON =<< (v.: "files")
        return Project{..} 

    parseJSON _ = mzero



getConfigFileSetup :: IO ConfigFileSetup
getConfigFileSetup = do
    contents <- sampleConfigFileContents
    let mjson = eitherDecode contents
    case mjson of
        Left err -> do
            -- putStrLn
            putStrLn ("Unable to read Configfile: " ++ err)
            return MHNothing
        Right result ->  do
            putStrLn $ show result
            return result


