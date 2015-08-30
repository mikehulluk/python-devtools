
{-# LANGUAGE OverloadedStrings #-}
module HdtProject where

import HdtTypes
import System.FilePath.Glob



findFiles :: Project -> FileSelector -> IO [File]
findFiles proj fileSelector = do
    files <- globDir1 (compile $ globString fileSelector) (rootDir proj)
    return $ map _buildFile files
    where _buildFile s = File {filename=s, tags=addTags fileSelector, project=proj}

srcFiles :: Project -> IO [File]
srcFiles proj =  do
    files <- mapM (findFiles proj) (fileSelectors proj)
    return $ concat files

getAllProjectConfigs :: IO [Project]
getAllProjectConfigs = do
    conf <- getConfigFileSetup
    return $ projects conf
