
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionReplace where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFileChangeStack

execReplace :: MyOptions -> IO ()
execReplace opts@ModeReplace{..} = do
    putStrLn $ "Replacing " ++ searchString ++ " with " ++ replaceString ++ ""
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    files <- srcFiles $ head activeProjects
    let activeFile = last $ files
    putStrLn $ show activeFile

    let description = "(s/" ++ searchString ++ "/" ++ replaceString ++ ")"
    addFileOutstandingChanges activeFile description ">>>>++++ New contents ++++<<<< "


    putStrLn "OK"




