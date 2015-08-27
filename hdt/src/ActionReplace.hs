
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionReplace where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFileChangeStack

import Data.String.Utils


-- mapWhile f (a:b) = case f a of
--     Just x -> x : mapWhile f b
--     Nothing -> []
-- mapWhile f [] = []

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

    -- Apply the changes to each file:
    mapM_ (actionReplace searchString replaceString) files
    --return ()


actionReplace :: String -> String -> File -> IO()
actionReplace findStr replStr file = do
    x <- tryReplace findStr replStr file
    case x of
        Nothing -> do
                    return ()
        Just newContents -> do
            let description = "(s/" ++ findStr ++ "/" ++ replStr ++ ")"
            addFileOutstandingChanges file description newContents



tryReplace :: String -> String -> File -> IO( Maybe String)
tryReplace findStr replStr file = do
    oldContents <- readFile $ filename file
    let newContents  = replace findStr replStr oldContents
    case newContents==oldContents of
        True -> return Nothing
        False -> return $ Just newContents





