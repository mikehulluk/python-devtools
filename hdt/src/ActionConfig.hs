
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
import Database.SQLite.Simple
--import Data.Traversable (for)


-- Printing to the screen
-- ^^^^^^^^^^^^^^^^^^^^^^^^

summariseFileLine :: Connection -> Int -> File -> IO()
summariseFileLine conn filenamePadding file = do

    changes <-getFileChanges conn file
    let nChanges = show $ length  changes
    let outstandingChanges = "Patches: " ++ nChanges ++ "  "
    putStrLn $ "\t" ++ paddedFname ++ "[" ++ outstandingChanges ++ tagString ++ "]"
    where paddedFname = pad ' ' filenamePadding (relativeFilename file)
          tagString = "tags:[" ++ (intercalate "," (tags file) ) ++ "]"


summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do

        conn <- getProjectDBHandle project
        setSGR [SetColor Foreground Vivid textcolor]

        putStrLn $ projectName project ++ ": " ++ (if (isActive project) then "<active>" else "<inactive>")
        let rootdir = rootDir project
        putStrLn $ "  Root:" ++  rootdir

        putStrLn "  Files:"
        srcfiles' <-(srcFiles project)

        --let srcfiles = map  (\x -> if rootdir `isPrefixOf` x then drop (length rootdir) x else x) srcfiles'


        let nFnamePadding = (maximum $ map (length.relativeFilename) srcfiles') + 3
        --let nFnamePadding = maxFilenameLength + 3
        mapM (summariseFileLine conn nFnamePadding)  srcfiles'
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

