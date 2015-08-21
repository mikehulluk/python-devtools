
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
    putStrLn "OK"
