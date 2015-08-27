
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionConfig where

import HdtTypes
import CmdLineOpts
import MHUtil as U
import HdtProject
import HdtFileChangeStack


import System.Console.ANSI
import Control.Monad
import Data.List


-- Printing to the screen
-- ^^^^^^^^^^^^^^^^^^^^^^^^

summariseFileLine :: Int -> File -> IO()
summariseFileLine filenamePadding file = do
    putStrLn $ "\t" ++ paddedFname ++ "<<<--" ++ fileChangesOutstanding ++ tagString
    where paddedFname = pad ' ' filenamePadding (filename file)
          fileChangesOutstanding = "??"
          ts = (tags file) :: [String]
          tagString' = intercalate "," ts
          tagString = "[" ++ tagString' ++ "]"


summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do
        setSGR [SetColor Foreground Vivid textcolor]
    
        putStrLn $ projectName project ++ ": " ++ (if (isActive project) then "<active>" else "<inactive>")
        putStrLn $ "  Root:" ++  (rootDir project)

        putStrLn "  Files:"
        let nFnamePadding = 75
        srcfiles <-(srcFiles project)
        mapM (summariseFileLine nFnamePadding)  srcfiles
        setSGR []
        putStrLn ""
        return ()
    where textcolor = if isActive project then Green else Red

execConfig :: MyOptions -> IO ()
execConfig opts@ModeConfig{..} = do
    projects <- getAllProjectConfigs
    forM projects summariseProjectConsole
    forM projects getProjectDBHandle

    x <- getConfigFileSetup
    return ()

