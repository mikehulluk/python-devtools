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



summariseFileLine :: Connection -> Int -> File -> IO()
summariseFileLine conn filenamePadding file = do

    patchs <-getFilePatchs conn file
    let nPatchs = show $ length  patchs
    let outstandingPatchs = "Patches: " ++ nPatchs ++ "  "
    putStrLn $ "\t" ++ paddedFname ++ "[" ++ outstandingPatchs ++ tagString ++ "]"
    where paddedFname = pad ' ' filenamePadding (relativeFilename file)
          tagString = "tags:[" ++ intercalate "," (tags file)  ++ "]"



projectTextColor :: Project -> Color
projectTextColor proj = case (isPrimary proj, isActive proj) of
    (True, _) -> Magenta
    (False, True) -> Green
    otherwise -> Red

summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do
        let textcolor = projectTextColor project
        conn <- getProjectDBHandle project
        setSGR [SetColor Foreground Vivid textcolor]

        let isActiveLabel = if isActive project then "<active>" else "<inactive>"
        let isPrimaryLabel = if isPrimary project then "<primary>" else ""
        putStrLn $ projectName project ++ ": " ++ isPrimaryLabel ++ " " ++ isActiveLabel
        let rootdir = rootDir project
        putStrLn $ "  Root:" ++  rootdir

        srcfiles' <-(srcFiles project)
        putStrLn $"  Files: " ++ (show $ length srcfiles')

        -- let nFnamePadding = (maximum $ map (length.relativeFilename) srcfiles' ) + 3
        -- mapM_ (summariseFileLine conn nFnamePadding)  srcfiles'
        


        setSGR []
        putStrLn ""
        return ()
    --where textcolor = if isActive project then Green else Red

execConfig :: MyOptions -> IO ()
execConfig opts@Config{..} = do
    projects <- getAllProjectConfigs
    forM_ projects summariseProjectConsole
    forM_ projects getProjectDBHandle
    return ()

