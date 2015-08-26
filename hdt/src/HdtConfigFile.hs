
{-# LANGUAGE OverloadedStrings #-}
module HdtConfigFile where

import qualified HdtTypes

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE

sampleConfigFileContents :: IO( LB.ByteString )
sampleConfigFileContents = do
    contents <- LB.readFile "/home/michael/hw/python-devtools/hdt/src/configfile.json.sample"
    return $ contents



data ConfigFileSetup = MHNothing | ConfigFileSetup {
    projects :: [ConfigFileProject]

} deriving (Show)


data ConfigFileProject = ConfigFileProject {
      projectName :: String 
    , isActive :: Bool
    , rootDir :: String
    , fileSelectors :: [HdtTypes.FileSelector]
} deriving (Show)



instance FromJSON HdtTypes.FileSelector where
    parseJSON (Object o) = do
        globString <- o .: "glob"
        return $ HdtTypes.FileSelector{HdtTypes.globString=globString,HdtTypes.addTags=[]}
        

instance FromJSON ConfigFileSetup where
    parseJSON (Object o) = do
        projects <- parseJSON =<< (o.: "projects")
        return $ ConfigFileSetup{projects=projects}
    parseJSON _ = mzero

instance FromJSON ConfigFileProject where
    parseJSON (Object v) = do
        name <- v .: "name"
        isActive <- v .:? "active" .!= False
        rootDir <- v .: "rootdir"
        fileSelectors <- parseJSON =<< (v.: "files")
        return ConfigFileProject{projectName=name, isActive=isActive, rootDir=rootDir, fileSelectors=fileSelectors}

    parseJSON _ = mzero



getConfigFileSetup :: IO(ConfigFileSetup)
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



