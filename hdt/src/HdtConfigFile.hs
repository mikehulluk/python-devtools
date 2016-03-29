
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module HdtConfigFile where


import System.Directory


import Data.List

import Data.Aeson as A
import Data.Yaml as Y
import Control.Monad
import qualified Data.ByteString as B

import MHUtil (expandUser)
import HdtTypes

getHDTConfigPath :: IO String
getHDTConfigPath  = do
    homeDir <- getHomeDirectory
    let hdtPath = homeDir ++ "/.hdt/"
    createDirectoryIfMissing True hdtPath
    return hdtPath






data ActiveProjectConfig = ActiveProjectConfig {
    primaryProject :: String,
    activeProjects :: [String]
}



instance FromJSON HdtTypes.FileSelector where
    parseJSON (Object o) = do
        globString <- o .: "glob"
        addTags <- o .: "tags"
        return HdtTypes.FileSelector{..}
    parseJSON _ = mzero
    --parseJSON _ = error "Unexpected token"

instance FromJSON Project where
    parseJSON (Object v) = do
        projectName <- v .: "name"
        isActive <- v .:? "active" .!= False
        rootDir <- v .: "rootdir"
        fileSelectors <- parseJSON =<< (v.: "files")
        let isPrimary = False 
        return Project{..} 
    parseJSON _ = mzero

instance FromJSON ActiveProjectConfig where
    parseJSON (Object v) = do
        primaryProject <- v .: "primary"
        activeProjects' <- v .: "active"
        let activeProjects = nub $ [primaryProject] ++ activeProjects'
        return ActiveProjectConfig{..}
    parseJSON _ = mzero

instance FromJSON ConfigFileSetup where
    parseJSON (Object o) = do
        apc :: ActiveProjectConfig    <- parseJSON =<< (o.: "general")
        projects <- parseJSON =<< (o.: "projects")
        -- Set the active/primary flags in each project
        let updatefunc = (updateIsActiveFields apc) . (updateIsPrimaryFields apc)
        let projects' = map (updatefunc) projects 
        return ConfigFileSetup{projects=projects'}
    parseJSON _ = mzero


updateIsActiveFields :: ActiveProjectConfig -> Project -> Project
updateIsActiveFields apc proj = 
    case (projectName proj) `elem` (activeProjects apc) of
        True -> proj{ isActive = True}
        False -> proj

updateIsPrimaryFields :: ActiveProjectConfig -> Project -> Project
updateIsPrimaryFields apc proj = proj{ isPrimary = isPrimary}
    where isPrimary = (projectName proj) == (primaryProject apc)




configFileContents :: IO B.ByteString 
configFileContents = expandUser "~/.hdtrc" >>= B.readFile



getConfigFileSetup :: IO ConfigFileSetup
getConfigFileSetup = do
    contents <- configFileContents
    let mjson = Y.decodeEither contents
    case mjson of
        Left err -> do
          error ("Unable to read Configfile: " ++ err)
        Right result ->  do
          return result










