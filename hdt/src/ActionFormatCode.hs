{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionFormatCode(execFormatCode) where

import System.FilePath
import qualified Data.ByteString.Char8 as B
import Text.Printf
import System.IO
import System.Process
import System.Exit


import CmdLineOpts
import HdtTypes
import HdtProject
import HdtFilePatchStack



pipeFileThroughTool :: (String,[String]) -> String -> IO( Maybe B.ByteString )
pipeFileThroughTool (toolName, args) fName = do
    putStrLn $ printf "  Running %s through %s " fName toolName
    original <- B.readFile fName
    (Just hIn, Just hOut, _, hProcess) <- createProcess (proc toolName args ){std_in=CreatePipe, std_out=CreatePipe}
    B.hPutStr hIn original
    hClose hIn
    exitCode <- waitForProcess hProcess
    newContents <- B.hGetContents hOut
    
    case exitCode of
        ExitSuccess -> do
            printf "[OK]"
        ExitFailure _ ->  do
            printf "[** FAILED **]"

    case (original == newContents) of
        True  -> return (Nothing)
        False -> return (Just newContents)
    

formatFile :: File -> IO()
formatFile file = do
    case takeExtension $ fname of
        ".py" -> do
            newContents <- (pipeFileThroughTool ("autopep8", ["-a", "-"]) fname)
            maybe (return ()) (addFileOutstandingPatchs file "(autopep8)") newContents
        ".hs" -> do
            newContents <- (pipeFileThroughTool ("haskell-formatter", []) fname)
            maybe (return ()) (addFileOutstandingPatchs file "(haskell-formatter)") newContents
        _     -> error "Not implemented"
    where fname = filename file
    


execFormatCode :: MyOptions -> IO ()
execFormatCode _opts@FormatCode{..} = do
    putStrLn $ "Formatting code"
    files <- allActiveSrcFiles
    mapM_ formatFile files


execFormatCode _ = error "execClean() called with wrong option type"

