{-# LANGUAGE RecordWildCards #-}

module ActionApply(execApply) where


import qualified Data.ByteString.Char8 as B
import Database.SQLite.Simple
import Data.Maybe
import Text.Printf


import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack
import ExtTools


execApply :: MyOptions -> IO ()
execApply _opts@Apply{..} = do
    activeProjects <- getActiveProjects
    mapM_ execApplyProject activeProjects 
    
execApply _ = error "execApply() called with wrong option type"

execApplyProject :: Project -> IO ()
execApplyProject proj = (getProjectFileMergeInfos proj) >>= applyProjectFileMergeInfos



data FileMergeInfo = FileMergeInfo {
      file    :: File
    , newBlob :: B.ByteString
} deriving (Show)




getFileMergeInfo :: Connection -> File -> IO ( Maybe FileMergeInfo)
getFileMergeInfo dbConn file = do
    printf "Generating merge info for: %s" $ filename file
    -- Generate all the patches:
    patches <- getFilePatchs dbConn file
    
    -- Merge them into a single output:
    case (length patches) of 
        0 -> return Nothing
        _ -> do
            printf "\n%d patchs for %s" (length patches) (filename file)
            newBlob <- mergePatches file patches
            return $ Just FileMergeInfo{file=file,newBlob=newBlob}


getProjectFileMergeInfos :: Project -> IO [ Maybe FileMergeInfo] 
getProjectFileMergeInfos proj = do
    printf "Collecting merge information for: %s" (projectName proj)
    dbConn <- getProjectDBHandle proj
    files <- srcFiles proj
    infos <- mapM (getFileMergeInfo dbConn) files
    return infos

applyFileMergeInfo :: FileMergeInfo -> IO()
applyFileMergeInfo mergeInfo = uiMergeFile (filename $ file mergeInfo) (newBlob mergeInfo)


applyProjectFileMergeInfos :: [Maybe FileMergeInfo] -> IO()
applyProjectFileMergeInfos  infos = do
    mapM_ applyFileMergeInfo (catMaybes infos)























mergePatches :: File -> [DbFilePatchEntry]  -> IO B.ByteString
mergePatches file patches = do

    -- 1. Diff each revision against the original:
    originalBlob <- B.readFile $ filename file
    let blobs = map blob patches
    diffs <- mapM ( (extDiff originalBlob) ) blobs

    -- 2. Now sequentially apply the patches:
    -- If something fails to merge, this probably suggests
    -- that we are trying to do too much in a single place.
    result <- mergePatchList originalBlob diffs
    case result of
        Nothing -> error "Unable to merge patches"
        Just output -> return $ output








mergePatchList :: B.ByteString -> [B.ByteString] -> IO (Maybe B.ByteString)
mergePatchList originalBlob []  = return $ Just originalBlob
mergePatchList originalBlob [p] = return =<< (runExtPatch originalBlob p)
mergePatchList originalBlob (p:ps) = do
    pch <- runExtPatch originalBlob p
    case pch of
        Nothing  -> return $ Nothing
        Just res -> return =<< (mergePatchList res ps)


