{-# LANGUAGE RecordWildCards #-}

module ActionGetRootDir where

import HdtTypes
import CmdLineOpts
import HdtProject


import System.Exit



execGetRootDir :: MyOptions -> IO ()
execGetRootDir _opts@GetRootDir{..} = do
    primaryProject <- getPrimaryProject;
    case primaryProject of
        Nothing     ->  do
            putStrLn "No primary project selected"
            exitWith (ExitFailure 10)
        (Just proj) -> do
            putStrLn $ rootDir proj
execGetRootDir _ = error "execGetRootDir() called with wrong option type"
