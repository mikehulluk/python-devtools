{-# LANGUAGE RecordWildCards #-}

module ActionGetRootDir where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

import Data.List
import System.Cmd
import System.Exit



execGetRootDir :: MyOptions -> IO ()
execGetRootDir opts@GetRootDir{..} = do
    primaryProject <- getPrimaryProject;
    case primaryProject of
        Nothing     ->  do
            putStrLn "No primary project selected"
            exitWith (ExitFailure 10)
        (Just proj) -> do
            putStrLn $ rootDir proj
