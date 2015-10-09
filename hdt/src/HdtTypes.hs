
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HdtTypes where

import System.Directory


import Data.List

import Data.Aeson as A
import Data.Yaml as Y
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

import MHUtil (expandUser)



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




sampleConfigFileContents :: IO B.ByteString 
sampleConfigFileContents = do
    --fname <- expandUser "~/hw/python-devtools/hdt/src/configfile.yaml.sample"
    fname <- expandUser "~/.hdtrc"
    contents <- B.readFile fname
    return contents




data ActiveProjectConfig = ActiveProjectConfig {
    primaryProject :: String,
    activeProjects :: [String]
}



instance FromJSON HdtTypes.FileSelector where
    parseJSON (Object o) = do
        globString <- o .: "glob"
        addTags <- o .: "tags"
        return HdtTypes.FileSelector{..}

instance FromJSON Project where
    parseJSON (Object v) = do
        projectName <- v .: "name"
        isActive <- v .:? "active" .!= False
        rootDir <- v .: "rootdir"
        fileSelectors <- parseJSON =<< (v.: "files")
        return Project{..} 

    parseJSON _ = mzero

instance FromJSON ActiveProjectConfig where
    parseJSON (Object v) = do
        primaryProject <- v .: "primary"
        activeProjects <- v .: "active"
        
        return ActiveProjectConfig{..}

instance FromJSON ConfigFileSetup where
    parseJSON (Object o) = do
        apc :: ActiveProjectConfig    <- parseJSON =<< (o.: "general")
        projects <- parseJSON =<< (o.: "projects")
        -- Set the active/primary flags in each project
        let projects' = map (updateIsActiveFields apc) projects 
        return ConfigFileSetup{projects=projects'}
    parseJSON _ = mzero

updateIsActiveFields :: ActiveProjectConfig -> Project -> Project
updateIsActiveFields apc proj = 
    case (projectName proj) `elem` (activeProjects apc) of
        True -> proj{ isActive = True}
        False -> proj





getConfigFileSetup :: IO ConfigFileSetup
getConfigFileSetup = do
    contents <- sampleConfigFileContents
    let mjson = Y.decodeEither contents
    case mjson of
        Left err -> do
            
            putStrLn ("Unable to read Configfile: " ++ err)
            return MHNothing
        Right result ->  do
            putStrLn $ show result
            return result


