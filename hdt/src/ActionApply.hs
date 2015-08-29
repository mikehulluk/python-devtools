

{-# LANGUAGE RecordWildCards #-}

module ActionApply where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

import System.IO.Temp
import System.IO

import System.Process
import System.Exit
import System.Directory

import qualified Data.ByteString.Char8 as B

import ExtTools

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












mergePatches' :: B.ByteString -> [B.ByteString] -> IO (Maybe B.ByteString)
mergePatches' originalBlob [] = return $ Just originalBlob
mergePatches' originalBlob [p] = do
    pch <- (runExtPatch originalBlob p)
    case pch of
        Nothing  -> return $ Nothing
        Just p -> return $ (Just p)

mergePatches' originalBlob (p:ps) = do
    pch <- runExtPatch originalBlob p
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










