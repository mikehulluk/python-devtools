

{-# LANGUAGE RecordWildCards #-}

module ActionApply where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

import System.IO.Temp
--import GHC.IO.Handle
import System.IO

import System.Process
import System.Exit

execApply :: MyOptions -> IO ()
execApply opts@ModeApply{..} = do


    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    let proj = head activeProjects
    files <- srcFiles proj
    mapM_ applyFile files
    --return()




applyFile :: File -> IO()
applyFile file = do
    let fname = filename file
    putStrLn $ "Applying patches to: " ++ fname
    dbConn <- getProjectDBHandle $ project file
    patches <- getFilePatchs dbConn file

    case length patches of
        0 -> putStrLn "No patches"
        _ -> do

            putStrLn $ unlines $ map show patches
            -- 1. Calculate the final output file, from the diffs:
            let finalBlob = mergePatches file patches 

            -- 2. Write it to a tempfile, and run the mergetool
            exitCode <- withTempFile "/home/michael/.hdt/" "tmp.mergefile" (runMergeTool file finalBlob)
            putStrLn $ " --- Finished with exit code: " ++ show exitCode

            -- 3. If it completed ok, then remove the diffs from the database.
            dropOutstandingPatchs file

            return ()

    putStrLn ""




mergePatches :: File -> [DbFilePatchEntry]  -> String 
mergePatches file patches = blob $ last patches










runMergeTool :: File -> String -> FilePath -> Handle -> IO Bool
runMergeTool file newBlob tmpFilePath hFile = do
    putStrLn $ "Writing into temp file:" ++ tmpFilePath

    -- Write the newBlob into the temp-file:
    hPutStr hFile newBlob
    hClose hFile

    (_, _, _, hProcess) <- createProcess (proc "meld" [filename file, tmpFilePath ])
    exitCode <- waitForProcess hProcess

    putStrLn $ "Finished with exit code: " ++ show exitCode
    case exitCode of
        ExitSuccess -> return True
        ExitFailure _ ->  do
            error "Failed to merge - terminating"
            return False


