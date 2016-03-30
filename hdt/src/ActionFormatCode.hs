{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionFormatCode(execFormatCode) where

--import System.Directory
import CmdLineOpts
--import HdtConstants


import System.FilePath
import HdtTypes
--import CmdLineOpts
import HdtProject
import HdtFilePatchStack
--import ExtTools

import qualified Data.ByteString.Char8 as B
--import Database.SQLite.Simple
--import Data.Maybe
import Text.Printf
--import System.IO.Temp
import System.IO

import System.Process
--import System.Exit
--import System.Directory
--import Text.Printf

--import Data.Maybe


pipeFileThroughTool :: (String,[String]) -> String -> IO( Maybe B.ByteString )
pipeFileThroughTool (toolName, args) fName = do
    printf "\nRunning %s through %s:" fName toolName
    original <- B.readFile fName
    (Just hIn, Just hOut, _, hProcess) <- createProcess (proc toolName args ){std_in=CreatePipe, std_out=CreatePipe}
    B.hPutStr hIn original
    hClose hIn
    exitCode <- waitForProcess hProcess
    newContents <- B.hGetContents hOut
    
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

    putStrLn "Formatting code"
    files <- allActiveSrcFiles
    
    
    mapM_ formatFile files


execFormatCode _ = error "execClean() called with wrong option type"



--addFileOutstandingPatchs file description newContents


--import System.IO.Temp
--import System.IO

--import System.Process
--import System.Exit
--import System.Directory
--import Text.Printf

--import qualified Data.ByteString.Char8 as B

--import HdtConstants
--import MHUtil (trim)




--extDiff :: B.ByteString -> B.ByteString -> IO B.ByteString
--extDiff originalBlob newBlob = do
    --hdtDir <- getHDTConfigPath
    --(p0, h0) <- openTempFile hdtDir "diff"
    --(p1, h1) <- openTempFile hdtDir "diff"
    --B.hPutStr h0 originalBlob
    --B.hPutStr h1 newBlob
    --mapM_ hClose [h0,h1]

    --(_, Just hOut, _, hProcess) <- createProcess (proc "diff" ["-u", p0, p1] ){std_out=CreatePipe}
    --exitCode <- waitForProcess hProcess
    --contents <- B.hGetContents hOut
