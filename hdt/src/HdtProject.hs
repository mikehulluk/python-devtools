
{-# LANGUAGE OverloadedStrings #-}
module HdtProject where

import Data.List


import System.FilePath.Glob

import HdtTypes
import HdtConfigFile


-- Selecting Projects:
-- --------------------
getAllProjectConfigs :: IO [Project]
getAllProjectConfigs = do
    conf <- getConfigFileSetup
    return $ projects conf

getPrimaryProject :: IO (Maybe Project)
getPrimaryProject = do
    primaryProjects <-  fmap (filter isPrimary) getAllProjectConfigs
    case length(primaryProjects) of
        0 -> return Nothing
        1 -> return $ Just (head primaryProjects)
        _ -> error $ "Unexpected number of primary projects. Found: " ++ (intercalate "," (map projectName primaryProjects))

getActiveProjects :: IO [Project]
getActiveProjects = fmap (filter isActive) getAllProjectConfigs



-- Selecting files for a project:
-- ------------------------------
findFiles :: Project -> FileSelector -> IO [File]
findFiles proj fileSelector = do
    files <- globDir1 (compile $ globString fileSelector) (rootDir proj)
    return $ map _buildFile files
    where _buildFile s = File {filename=s, tags=addTags fileSelector, project=proj}

srcFiles :: Project -> IO [File]
srcFiles proj =  do
    files <- mapM (findFiles proj) (fileSelectors proj)
    return $ concat files

allActiveSrcFiles :: IO [File]
allActiveSrcFiles = do
    projs <- getActiveProjects
    srcfiles <- mapM srcFiles projs
    return $ concat srcfiles
