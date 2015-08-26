
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionReplace where

import HdtTypes
import CmdLineOpts

execReplace :: MyOptions -> IO ()
execReplace opts@ModeReplace{..} = do
    putStrLn $ "Replacing " ++ show searchString ++ " with " ++ show replaceString ++ ""
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    let activeFile = head $ srcFiles $ head activeProjects
    putStrLn $ show activeFile

    addFileOutstandingChanges activeFile "New contents"


    putStrLn "OK"




