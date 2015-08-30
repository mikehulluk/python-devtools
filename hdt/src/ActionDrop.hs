{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionDrop(execDrop) where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack




execDrop :: MyOptions -> IO ()
execDrop opts@ModeDrop{..} = do
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    srcfiles <- mapM srcFiles activeProjects
    let s = concat srcfiles

    mapM_ dropOutstandingPatchs s



