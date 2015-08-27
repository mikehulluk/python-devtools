
{-# LANGUAGE OverloadedStrings #-}
module HdtTypes where

import System.Directory
import Filesystem.Path hiding (filename)
import Control.Applicative
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


relativeFilename :: File -> String
relativeFilename file= if rootdir `isPrefixOf` fname then drop (length rootdir) fname else fname
    where fname = filename file
          rootdir = rootDir $ project file


getHDTConfigPath :: IO String
getHDTConfigPath  = do
    homeDir <- getHomeDirectory
    let thepath = homeDir ++ "/.hdt/"
    createDirectoryIfMissing True thepath
    return thepath




sampleConfigFileContents :: IO LB.ByteString 
sampleConfigFileContents = do
    contents <- LB.readFile "/home/michael/hw/python-devtools/hdt/src/configfile.json.sample"
    --contents <- LB.readFile "/home/mike/dev/python-devtools/hdt/src/configfile.json.sample"
    return contents



data ConfigFileSetup = MHNothing | ConfigFileSetup {
    projects :: [Project]

} deriving (Show)





instance FromJSON HdtTypes.FileSelector where
    parseJSON (Object o) = do
        globString <- o .: "glob"
        addTags' <- o .: "tags"
        return HdtTypes.FileSelector{HdtTypes.globString=globString,HdtTypes.addTags=addTags'}


instance FromJSON ConfigFileSetup where
    parseJSON (Object o) = do
        projects <- parseJSON =<< (o.: "projects")
        return ConfigFileSetup{projects=projects}
    parseJSON _ = mzero

instance FromJSON Project where
    parseJSON (Object v) = do
        name <- v .: "name"
        isActive <- v .:? "active" .!= False
        rootDir <- v .: "rootdir"
        fileSelectors <- parseJSON =<< (v.: "files")
        return Project{projectName=name, isActive=isActive, rootDir=rootDir, fileSelectors=fileSelectors}

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


