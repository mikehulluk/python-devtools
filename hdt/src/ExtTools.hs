{-# LANGUAGE OverloadedStrings #-}
module ExtTools where


import System.IO.Temp
import System.IO

import System.Process
import System.Exit
import System.Directory
import Text.Printf

import qualified Data.ByteString.Char8 as B

import HdtConstants
import MHUtil (trim)




extDiff :: B.ByteString -> B.ByteString -> IO B.ByteString
extDiff originalBlob newBlob = do
    hdtDir <- getHDTConfigPath
    (p0, h0) <- openTempFile hdtDir "diff"
    (p1, h1) <- openTempFile hdtDir "diff"
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
            error "\nFailed to diff - terminating\n"
            B.putStrLn $ contents
            return $ B.pack ""



    


runExtPatch :: B.ByteString -> B.ByteString -> IO ( Maybe B.ByteString)
runExtPatch originalBlob patch = do
    putStrLn $ printf "  Applying patch" 
    hdtDir <- getHDTConfigPath
    (p0, h0) <- openTempFile hdtDir "merge"
    hPutStr h0 (B.unpack originalBlob)
    hClose h0
    (Just hIn, Just hOut, hErr, hProcess) <- createProcess (proc "patch" ["-u", p0] ){std_out=CreatePipe, std_in=CreatePipe}
    hPutStr hIn (B.unpack patch)
    hClose hIn
    exitCode <- waitForProcess hProcess
    stdOut <- hGetContents hOut
    
    stdErr <- case hErr of
            Nothing -> (return "")
            Just h -> (hGetContents h)

    putStrLn $ printf  "    -- Stdout from 'patch': '%s'" (trim $ stdOut) 
    putStrLn $ printf  "    -- Stderr from 'patch': '%s'" (trim $ stdErr) 

    case exitCode of
        ExitSuccess -> do
            contents <- readFile p0
            return $ Just ( B.pack contents)
        ExitFailure _ ->  do
            error "\nFailed to patch - terminating"
            return $ Nothing




runMergeTool :: String -> B.ByteString -> FilePath -> Handle -> IO Bool
runMergeTool fname newBlob tmpFilePath hFile = do
    putStrLn $ "runMergeTool: writing into temp file:" ++ tmpFilePath

    -- Write the newBlob into the temp-file:
    B.hPutStr hFile newBlob
    hClose hFile

    (_, _, _, hProcess) <- createProcess (proc "meld" [fname, tmpFilePath ])
    exitCode <- waitForProcess hProcess

    putStrLn $ "Finished with exit code: " ++ show exitCode
    case exitCode of
        ExitSuccess -> return True
        ExitFailure _ ->  do
            error "\nFailed to merge - terminating"
            return False

uiMergeFile :: String -> B.ByteString -> IO ()
uiMergeFile fname newBlob  = do
    hdtDir <- getHDTConfigPath
    exitCode <- withTempFile hdtDir "tmp.mergefile" (runMergeTool fname newBlob)
    putStrLn $ " --- Finished with exit code: " ++ show exitCode
