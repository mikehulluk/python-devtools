
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionConfig where

import HdtTypes
import CmdLineOpts


import System.Console.ANSI
import Control.Monad


-- Printing to the screen
-- ^^^^^^^^^^^^^^^^^^^^^^^^
summariseProject :: Project -> String
summariseProject project = unlines [
     projectName project ++ ": " ++ (if (isActive project) then "<active>" else "<inactive>")
    ,"  Root:" ++  (rootDir project)
    ,"  Files:" ++ (unwords $ (map filename (srcFiles project)) )
    ]

summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do
        setSGR [SetColor Foreground Vivid textcolor]
        putStrLn $ summariseProject project
        setSGR []
        return ()
    where textcolor = if isActive project then Green else Red

execConfig :: MyOptions -> IO ()
execConfig opts@ModeConfig{..} = do
    projects <- getAllProjectConfigs
    forM projects summariseProjectConsole
    forM projects getProjectDBHandle
    return ()

