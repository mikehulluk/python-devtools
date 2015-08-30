{-# LANGUAGE RecordWildCards #-}


import CmdLineOpts
import ActionGrep
import ActionReplace
import ActionConfig
import ActionApply
import ActionDrop

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)


import Control.Monad


main :: IO ()
main = do
    args' <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args' then withArgs ["--help"] else id) $ cmdArgsRun myModes
    optionHandler opts

optionHandler :: MyOptions -> IO ()
optionHandler opts@ModeConfig{..}  = do
    when (null first_name) $ putStrLn "warning: --first-name is blank"
    when (null last_name) $ putStrLn "warning: --last-name is blank"
    execConfig opts

optionHandler opts@ModeGrep{..}  = do
    putStrLn  $ "Grepping for: " ++ "'" ++ grepString ++ "'"
    execGrep opts

optionHandler opts@ModeReplace{..}  = do
    putStrLn  $ "Replacing for: " ++ "'" ++ searchString ++ "'"
    execReplace opts

optionHandler opts@ModeDrop{..}  = do
    execDrop opts


optionHandler opts@ModeApply{..}  = do
    execApply opts


