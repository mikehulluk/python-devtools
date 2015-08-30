{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionDrop(execDrop) where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

execDrop :: MyOptions -> IO ()
execDrop opts@Drop{..} = do
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    srcfiles <- mapM srcFiles activeProjects
    mapM_ dropOutstandingPatchs $ concat srcfiles
