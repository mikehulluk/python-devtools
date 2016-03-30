{-# LANGUAGE RecordWildCards #-}
module ActionClean(execClean) where

import System.Directory
import CmdLineOpts
import HdtConstants
import Text.Printf

execClean :: MyOptions -> IO ()
execClean _opts@Clean{..} = do
    hdtPath <- getHDTConfigPath
    putStrLn $ printf "Removing directory: %s" hdtPath
    removeDirectoryRecursive hdtPath

execClean _ = error "execClean() called with wrong option type"
