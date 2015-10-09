{-# LANGUAGE RecordWildCards #-}

module ActionGetRootDir where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

import Data.List
import System.Cmd
import System.Data



execGetRootDir :: MyOptions -> IO ()
execGetRootDir opts@GetRootDir{..} = do
    let primaryProject = getPrimaryProject;
    case primaryProject of
        Nothing     ->  do
            putStrLn "No primary project selected"
            exitWith (ExitFailure 10)
        (Just proj) -> do
            putStrLn $ rootDir proj

    --putStrLn "/home/mike/dev/python-devtools/hdt"
    -- -- Get the active projects
    -- projects <- getAllProjectConfigs
    -- let activeProjects = filter isActive projects
    -- srcfiles <- mapM srcFiles activeProjects

    -- let all_srcfiles = map filename ( concat( srcfiles) )
    -- createTagsFile all_srcfiles 
    -- return ()
    -- -- mapM_ dropOutstandingPatchs $ concat srcfiles

