{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionDrop(execDrop) where

--import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

execDrop :: MyOptions -> IO ()
execDrop _opts@Drop{..} = do

    -- Get the active projects
    srcfiles <- allActiveSrcFiles
    mapM_ dropOutstandingPatchs  srcfiles

    
execDrop _ = error "execDrop() called with wrong option type"
