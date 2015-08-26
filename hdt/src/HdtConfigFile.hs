
{-# LANGUAGE OverloadedStrings #-}
module HdtConfigFile where


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
    name :: Text
} deriving (Show)

instance FromJSON ConfigFileSetup where
    parseJSON (Object o) = do
        projects <- parseJSON =<< (o.: "projects")
        return $ ConfigFileSetup{projects=projects}
    parseJSON _ = mzero

instance FromJSON ConfigFileProject where
    parseJSON (Object v) = ConfigFileProject <$>
                            v.: "name"
    parseJSON _ = mzero
        


getConfigFileSetup :: IO(ConfigFileSetup)
getConfigFileSetup = do
    contents <- sampleConfigFileContents
    let mjson = decode contents
    case mjson of
        Nothing -> do
            putStrLn "Unable to read Configfile"
            return MHNothing 
        Just result ->  do
            putStrLn $ show result
            return result



