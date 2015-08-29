

{-# LANGUAGE RecordWildCards #-}

module ActionApply where

import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack
import ExtTools

import System.IO.Temp
import System.IO

import System.Process
import System.Exit
import System.Directory

import qualified Data.ByteString.Char8 as B
import Database.SQLite.Simple
import Data.Maybe






execApply :: MyOptions -> IO ()
execApply opts@ModeApply{..} = do
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    let proj = head activeProjects
    --files <- srcFiles proj
    --mapM_ applyFile files


    merges <- getProjectFileMergeInfos proj
    applyProjectFileMergeInfos merges


data FileMergeInfo = FileMergeInfo {
      file    :: File
    , newBlob :: String
} deriving (Show)


uiMergeFile :: String -> String -> IO ()
uiMergeFile fname newBlob  = do
    exitCode <- withTempFile "/home/michael/.hdt/" "tmp.mergefile" (runMergeTool fname newBlob)
    putStrLn $ " --- Finished with exit code: " ++ show exitCode


getFileMergeInfo :: Connection -> File -> IO ( Maybe FileMergeInfo)
getFileMergeInfo dbConn file = do
    patches <- getFilePatchs dbConn file
    case length patches of 
        0 -> return Nothing
        _ -> do
            newBlob <- mergePatches file patches
            return $ Just FileMergeInfo{file=file,newBlob=newBlob}


getProjectFileMergeInfos :: Project -> IO [ Maybe FileMergeInfo] 
getProjectFileMergeInfos proj = do
    dbConn <- getProjectDBHandle proj
    files <- srcFiles proj
    infos <- mapM (getFileMergeInfo dbConn) files
    return infos

applyFileMergeInfo :: FileMergeInfo -> IO()
applyFileMergeInfo mergeInfo = uiMergeFile (filename $ file mergeInfo) (newBlob mergeInfo)


applyProjectFileMergeInfos :: [Maybe FileMergeInfo] -> IO()
applyProjectFileMergeInfos  infos = do
    mapM_ applyFileMergeInfo (catMaybes infos)




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
            newBlob <- mergePatches file patches

            -- 2. Write it to a tempfile, and run the mergetool
            uiMergeFile fname newBlob

            -- 3. If it completed ok, then remove the diffs from the database.
            dropOutstandingPatchs file

            return ()

    putStrLn ""



















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
    result <- mergePatchList originalBlob' diffs
    case result of
        Nothing -> error "Unable to merge patches"
        Just output -> return $ B.unpack output








mergePatchList :: B.ByteString -> [B.ByteString] -> IO (Maybe B.ByteString)
mergePatchList originalBlob []  = return $ Just originalBlob
mergePatchList originalBlob [p] = return =<< (runExtPatch originalBlob p)
mergePatchList originalBlob (p:ps) = do
    pch <- runExtPatch originalBlob p
    case pch of
        Nothing  -> return $ Nothing
        Just res -> return =<< (mergePatchList res ps)


