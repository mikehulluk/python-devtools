
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionConfig where

import HdtTypes
import CmdLineOpts


import System.Console.ANSI
import Control.Monad
import Data.List


-- Printing to the screen
-- ^^^^^^^^^^^^^^^^^^^^^^^^

summariseFileLine :: File -> IO()
summariseFileLine file = do
    putStrLn $ "\t" ++ (filename file)


summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do
        setSGR [SetColor Foreground Vivid textcolor]
        -- putStrLn $ summariseProject project
    
        putStrLn $ projectName project ++ ": " ++ (if (isActive project) then "<active>" else "<inactive>")
        putStrLn $ "  Root:" ++  (rootDir project)

        putStrLn "  Files:"
        mapM summariseFileLine (srcFiles project)
        setSGR []
        putStrLn ""
        return ()
    where textcolor = if isActive project then Green else Red

execConfig :: MyOptions -> IO ()
execConfig opts@ModeConfig{..} = do
    projects <- getAllProjectConfigs
    forM projects summariseProjectConsole
    forM projects getProjectDBHandle
    return ()

