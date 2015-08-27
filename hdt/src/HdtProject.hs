
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module HdtProject where

import HdtTypes
import System.FilePath.Glob

import Prelude hiding (findFiles)


findFiles :: String -> FileSelector -> IO( [File] )
findFiles rootDir fileSelector = do
    files <- globDir1 (compile $ globString fileSelector) rootDir
    return $ map _buildFile files
    where _buildFile s = File {filename=s, isClean=True, tags=addTags fileSelector}



srcFiles :: Project -> IO( [File] )
srcFiles project =  do
    files <- mapM (findFiles $  rootDir project) ( fileSelectors project )
    return $ concat files




getAllProjectConfigs :: IO [Project]
getAllProjectConfigs = do
    conf <- getConfigFileSetup
    return $ projects conf
