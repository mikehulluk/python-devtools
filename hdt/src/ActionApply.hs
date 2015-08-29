

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
import System.Directory

--import Data.Algorithm.Diff
--import Data.ByteString.Delta
--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B

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
            finalBlob <- mergePatches file patches

            -- 2. Write it to a tempfile, and run the mergetool
            exitCode <- withTempFile "/home/michael/.hdt/" "tmp.mergefile" (runMergeTool file finalBlob)
            putStrLn $ " --- Finished with exit code: " ++ show exitCode

            -- 3. If it completed ok, then remove the diffs from the database.
            dropOutstandingPatchs file

            return ()

    putStrLn ""







extDiff :: B.ByteString -> B.ByteString -> IO B.ByteString
extDiff originalBlob newBlob = do
    (p0, h0) <- openTempFile "/home/michael/.hdt/" "diff"
    (p1, h1) <- openTempFile "/home/michael/.hdt/" "diff"
    hPutStr h0 (B.unpack originalBlob)
    hPutStr h1 (B.unpack newBlob)
    hClose h0
    hClose h1

    (_, Just hOut, _, hProcess) <- createProcess (proc "diff" ["-u", p0, p1] ){std_out=CreatePipe}
    exitCode <- waitForProcess hProcess
    contents <- hGetContents hOut
    putStrLn contents

    putStrLn $ "Finished with exit code: " ++ show exitCode
    -- 'diff' returns 
    --   0 - No difference
    --   1 - Differences
    --   2 - Error
    case exitCode of
        ExitSuccess -> do
            removeFile p0
            removeFile p1
            return $ B.pack contents
        ExitFailure 1 ->  do
            removeFile p0
            removeFile p1
            return $ B.pack contents
        ExitFailure _ ->  do
            error "Failed to diff - terminating"
            return $ B.pack ""



    --


extPatch :: B.ByteString -> B.ByteString -> IO ( Maybe B.ByteString)
extPatch originalBlob patch = do
    (p0, h0) <- openTempFile "/home/michael/.hdt/" "merge"
    hPutStr h0 (B.unpack originalBlob)
    hClose h0
    (Just hIn, Just hOut, _, hProcess) <- createProcess (proc "patch" ["-u", p0] ){std_out=CreatePipe, std_in=CreatePipe}
    hPutStr hIn (B.unpack patch)
    hClose hIn
    exitCode <- waitForProcess hProcess
    stdOut <- hGetContents hOut

    putStrLn $ "Output of patch:" ++ stdOut





    case exitCode of
        ExitSuccess -> do
            contents <- readFile p0
            return $ Just ( B.pack contents)
        ExitFailure _ ->  do
            error "Failed to patch - terminating"
            return $ Nothing






mergePatches' :: B.ByteString -> [B.ByteString] -> IO (Maybe B.ByteString)
mergePatches' originalBlob [] = return $ Just originalBlob
mergePatches' originalBlob [p] = do
    pch <- (extPatch originalBlob p)
    case pch of
        Nothing  -> return $ Nothing
        Just p -> return $ (Just p)

mergePatches' originalBlob (p:ps) = do
    pch <- extPatch originalBlob p
    case pch of
        Nothing   -> return $ Nothing
        Just res  ->  do
            rs <- (mergePatches' res ps)
            return $ rs







mergePatches :: File -> [DbFilePatchEntry]  -> IO String
mergePatches file patches = do

    -- 1. Diff each revision against the original:
    originalBlob <- readFile $ filename file
    let originalBlob' = B.pack originalBlob
    let blobs = map blob patches
    let blobs' = map B.pack blobs
    diffs <- mapM (extDiff originalBlob') blobs'

    putStrLn $ "\nDiffs: "  ++ (show $ length diffs)
    putStrLn $ unlines $ map (show . B.unpack) diffs

    -- 2. Now sequentially apply the patches:
    -- If something fails to merge, this probably suggests
    -- that we are trying to do too much in a single place.
    result <- mergePatches' originalBlob' diffs
    case result of
        Nothing -> error "Unable to merge patches"
        Just output -> return $ B.unpack output









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


