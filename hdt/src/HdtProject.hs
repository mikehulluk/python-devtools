
{-# LANGUAGE OverloadedStrings #-}
module HdtProject where

import HdtTypes
import System.FilePath.Glob

import Prelude hiding (findFiles)


findFiles :: Project -> FileSelector -> IO [File]
findFiles project fileSelector = do
    files <- globDir1 (compile $ globString fileSelector) (rootDir project)
    return $ map _buildFile files
    where _buildFile s = File {filename=s, tags=addTags fileSelector, project=project}

srcFiles :: Project -> IO [File]
srcFiles project =  do
    files <- mapM (findFiles project) (fileSelectors project)
    return $ concat files

getAllProjectConfigs :: IO [Project]
getAllProjectConfigs = do
    conf <- getConfigFileSetup
    return $ projects conf
