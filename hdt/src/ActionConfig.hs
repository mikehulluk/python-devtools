{-# LANGUAGE RecordWildCards #-}

module ActionConfig(execConfig) where

import Text.Printf
import System.Console.ANSI
import Control.Monad

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack


--summariseFileLine :: Connection -> Int -> File -> IO()
--summariseFileLine conn filenamePadding file = do
--    patchs <- getFilePatchs conn file
--    let nPatchs = show $ length  patchs
--    let outstandingPatchs = "Patches: " ++ nPatchs ++ "  "
--    putStrLn $ "\t" ++ paddedFname ++ "[" ++ outstandingPatchs ++ tagString ++ "]"
--    where paddedFname = pad ' ' filenamePadding (relativeFilename file)
--          tagString = "tags:[" ++ intercalate "," (tags file)  ++ "]"



projectTextColor :: Project -> Color
projectTextColor proj = case (isPrimary proj, isActive proj) of
    (True, _)     -> Magenta
    (False, True) -> Green
    (_,_)         -> Red

summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do
        
        srcfiles' <- (srcFiles project)
        let isActiveLabel =  if isActive project then "<active>" else "<inactive>"
        let isPrimaryLabel = if isPrimary project then "<primary>" else ""
        
        setSGR [SetColor Foreground Vivid (projectTextColor project)]
        putStrLn $ printf "%s: %s %s"  (projectName project) isPrimaryLabel isActiveLabel
        putStrLn $ printf "  Root: %s" (rootDir project)        
        putStrLn $ printf "  Files: %d" $ (length srcfiles')
        putStrLn ""
        setSGR []
        
        --conn <- getProjectDBHandle project
        -- let nFnamePadding = (maximum $ map (length.relativeFilename) srcfiles' ) + 3
        -- mapM_ (summariseFileLine conn nFnamePadding)  srcfiles'
        
        return ()
    

execConfig :: MyOptions -> IO ()
execConfig _opts@Config{..} = do
    projects <- getAllProjectConfigs
    forM_ projects summariseProjectConsole
    forM_ projects getProjectDBHandle
    return ()
execConfig _ = error "execConfig() called with wrong option type"

