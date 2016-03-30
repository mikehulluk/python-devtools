{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import Text.Printf

import CmdLineOpts
import ActionGrep
import ActionReplace
import ActionConfig
import ActionApply
import ActionDrop
import ActionTags
import ActionGetRootDir
import ActionClean
import ActionFormatCode

import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter 


import System.FilePath ((</>))
import System.IO
import HdtConstants

main :: IO ()
main = do

    let consoleLogLevel = INFO

    -- Setup logging:
    hdtPath <- getHDTConfigPath
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    h <- fileHandler (hdtPath </> "debug.log") DEBUG >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    g <- streamHandler (stdout) consoleLogLevel >>= \lh -> return $ setFormatter lh (simpleLogFormatter "[$loggername : $prio] $msg")
    updateGlobalLogger rootLoggerName (addHandler h)
    updateGlobalLogger rootLoggerName (addHandler g)
    updateGlobalLogger rootLoggerName (setHandlers [g,h])
    debugM "main" $ printf "Starting."
    
    
    -- Parse commandline:
    args' <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args' then withArgs ["--help"] else id) $ cmdArgsRun myModes
    optionHandler opts    
    
    
    -- Did we finish cleanly?
    debugM "main" $ printf "LOG:Finished OK."
    
    
optionHandler :: MyOptions -> IO ()
optionHandler opts@Config{..}  = do
--    when (null first_name) $ putStrLn "warning: --first-name is blank"
--    when (null last_name) $ putStrLn "warning: --last-name is blank"
    execConfig opts

optionHandler opts@Grep{..}  = do
    infoM "main"  $ printf "Hdt:Grepping: '%s'" grepString
    execGrep opts

optionHandler opts@Repl{..}  = do
    infoM "main"  $ printf "Hdt:Replacing: '%s' -> '%s'" searchString replaceString
    execReplace opts

optionHandler opts@Drop{..}  = do
    infoM "main"  $ printf "Hdt:Drop"
    execDrop opts

optionHandler opts@Apply{..}  = do
    infoM "main"  $ printf "Hdt:Apply"
    execApply opts

optionHandler opts@Tags{..}  = do
    infoM "main"  $ printf "Hdt:Tag"
    execTags opts

optionHandler opts@GetRootDir{..}  = do
    execGetRootDir opts

optionHandler opts@Clean{..}  = do
    infoM "main"  $ printf "Hdt:Clean"
    execClean opts
    
optionHandler opts@FormatCode{..}  = do
    infoM "main"  $ printf "Hdt:FormatCode"
    execFormatCode opts
