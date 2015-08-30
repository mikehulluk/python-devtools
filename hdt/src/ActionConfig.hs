{-# LANGUAGE RecordWildCards #-}

module ActionConfig(execConfig) where

import HdtTypes
import CmdLineOpts
import MHUtil as U
import HdtProject
import HdtFilePatchStack


import System.Console.ANSI
import Control.Monad
import Data.List
import Database.SQLite.Simple
--import Data.Traversable (for)


-- Printing to the screen
-- ^^^^^^^^^^^^^^^^^^^^^^^^

summariseFileLine :: Connection -> Int -> File -> IO()
summariseFileLine conn filenamePadding file = do

    patchs <-getFilePatchs conn file
    let nPatchs = show $ length  patchs
    let outstandingPatchs = "Patches: " ++ nPatchs ++ "  "
    putStrLn $ "\t" ++ paddedFname ++ "[" ++ outstandingPatchs ++ tagString ++ "]"
    where paddedFname = pad ' ' filenamePadding (relativeFilename file)
          tagString = "tags:[" ++ intercalate "," (tags file)  ++ "]"


summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do

        conn <- getProjectDBHandle project
        setSGR [SetColor Foreground Vivid textcolor]

        putStrLn $ projectName project ++ ": " ++ if isActive project then "<active>" else "<inactive>"
        let rootdir = rootDir project
        putStrLn $ "  Root:" ++  rootdir

        putStrLn "  Files:"
        srcfiles' <-(srcFiles project)

        let nFnamePadding = (maximum $ map (length.relativeFilename) srcfiles' ) + 3
        mapM_ (summariseFileLine conn nFnamePadding)  srcfiles'
        setSGR []
        putStrLn ""
        return ()
    where textcolor = if isActive project then Green else Red

execConfig :: MyOptions -> IO ()
execConfig opts@ModeConfig{..} = do
    projects <- getAllProjectConfigs
    forM_ projects summariseProjectConsole
    forM_ projects getProjectDBHandle
    return ()

