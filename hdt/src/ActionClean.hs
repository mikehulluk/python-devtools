{-# LANGUAGE RecordWildCards #-}
module ActionClean(execClean) where

import System.Directory
import CmdLineOpts
import HdtConstants


execClean :: MyOptions -> IO ()
execClean _opts@Clean{..} = do
    hdtPath <- getHDTConfigPath
    putStrLn $ "\nRemoving directory: " ++ hdtPath
    removeDirectoryRecursive hdtPath

execClean _ = error "execClean() called with wrong option type"
