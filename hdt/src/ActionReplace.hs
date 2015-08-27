
{-# LANGUAGE RecordWildCards #-}

module ActionReplace where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

import Data.String.Utils



execReplace :: MyOptions -> IO ()
execReplace opts@ModeReplace{..} = do


    putStrLn $ "Replacing " ++ searchString ++ " with " ++ replaceString ++ ""
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects

    -- Find all the files:
    allfiles' <- mapM srcFiles activeProjects
    let files = concat allfiles'

    -- Check no files belong to more than one project:
    let allFullFilename = map filename files
    putStrLn $ unlines allFullFilename

    -- Apply the patchs to each file:
    mapM_ (actionReplace searchString replaceString) files
    --return ()


actionReplace :: String -> String -> File -> IO()
actionReplace findStr replStr file = do
    x <- tryReplace findStr replStr file
    case x of
        Nothing -> return ()
        Just newContents -> do
            let description = "(s/" ++ findStr ++ "/" ++ replStr ++ ")"
            addFileOutstandingPatchs file description newContents



tryReplace :: String -> String -> File -> IO( Maybe String)
tryReplace findStr replStr file = do
    oldContents <- readFile $ filename file
    let newContents  = replace findStr replStr oldContents
    if newContents==oldContents then return Nothing else return $ Just newContents





