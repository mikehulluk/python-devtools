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


main :: IO ()
main = do
    args' <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args' then withArgs ["--help"] else id) $ cmdArgsRun myModes
    optionHandler opts
    printf "\nFinished OK.\n"

optionHandler :: MyOptions -> IO ()
optionHandler opts@Config{..}  = do
--    when (null first_name) $ putStrLn "warning: --first-name is blank"
--    when (null last_name) $ putStrLn "warning: --last-name is blank"
    execConfig opts

optionHandler opts@Grep{..}  = do
    putStrLn  $ "Grepping for: " ++ "'" ++ grepString ++ "'"
    execGrep opts

optionHandler opts@Repl{..}  = do
    putStrLn  $ "Replacing for: " ++ "'" ++ searchString ++ "'"
    execReplace opts

optionHandler opts@Drop{..}  = do
    execDrop opts

optionHandler opts@Apply{..}  = do
    execApply opts

optionHandler opts@Tags{..}  = do
    execTags opts

optionHandler opts@GetRootDir{..}  = do
    execGetRootDir opts

