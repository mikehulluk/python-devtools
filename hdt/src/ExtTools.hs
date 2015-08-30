{-# LANGUAGE OverloadedStrings #-}
module ExtTools where


--import HdtTypes
--import CmdLineOpts
--import HdtProject
--import HdtFilePatchStack

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


extDiff :: B.ByteString -> B.ByteString -> IO B.ByteString
extDiff originalBlob newBlob = do
    (p0, h0) <- openTempFile "/home/michael/.hdt/" "diff"
    (p1, h1) <- openTempFile "/home/michael/.hdt/" "diff"
    B.hPutStr h0 originalBlob
    B.hPutStr h1 newBlob
    mapM_ hClose [h0,h1]

    (_, Just hOut, _, hProcess) <- createProcess (proc "diff" ["-u", p0, p1] ){std_out=CreatePipe}
    exitCode <- waitForProcess hProcess
    contents <- B.hGetContents hOut

    -- 'diff' returns
    --   0 - No difference
    --   1 - Differences
    --   2 - Error
    case exitCode of
        ExitSuccess -> do
            mapM_ removeFile [p0,p1]
            return $ contents
        ExitFailure 1 ->  do
            mapM_ removeFile [p0,p1]
            return $ contents
        ExitFailure _ ->  do
            error "Failed to diff - terminating"
            B.putStrLn $ contents
            return $ B.pack ""



    --


runExtPatch :: B.ByteString -> B.ByteString -> IO ( Maybe B.ByteString)
runExtPatch originalBlob patch = do
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




runMergeTool :: String -> B.ByteString -> FilePath -> Handle -> IO Bool
runMergeTool fname newBlob tmpFilePath hFile = do
    putStrLn $ "Writing into temp file:" ++ tmpFilePath

    -- Write the newBlob into the temp-file:
    B.hPutStr hFile newBlob
    hClose hFile

    (_, _, _, hProcess) <- createProcess (proc "meld" [fname, tmpFilePath ])
    exitCode <- waitForProcess hProcess

    putStrLn $ "Finished with exit code: " ++ show exitCode
    case exitCode of
        ExitSuccess -> return True
        ExitFailure _ ->  do
            error "Failed to merge - terminating"
            return False

uiMergeFile :: String -> B.ByteString -> IO ()
uiMergeFile fname newBlob  = do
    exitCode <- withTempFile "/home/michael/.hdt/" "tmp.mergefile" (runMergeTool fname newBlob)
    putStrLn $ " --- Finished with exit code: " ++ show exitCode
