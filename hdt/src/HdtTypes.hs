
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HdtTypes where


import Data.List



-- Basic types definitions:
-- ------------------------
data Project = Project {
      projectName :: String
    , rootDir :: String
    , isActive :: Bool
    , isPrimary :: Bool
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

data ConfigFileSetup = ConfigFileSetup {
    projects :: [Project]
} deriving (Show)





relativeFilename :: File -> String
relativeFilename File{..} = if rootDir project `isPrefixOf` filename then drop (length $ rootDir project) filename else filename




