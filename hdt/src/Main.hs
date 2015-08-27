--{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}


import HdtTypes
import CmdLineOpts
import ActionGrep
import ActionReplace
import ActionConfig
import ActionApply

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Data.List
import Data.Text.Format

import System.Console.ANSI
import Control.Monad
import Control.Exception

import Text.Regex.Posix   
import Text.Regex.Posix.String











main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
    optionHandler opts

optionHandler :: MyOptions -> IO ()
optionHandler opts@ModeConfig{..}  = do
    when (null first_name) $ putStrLn "warning: --first-name is blank"
    when (null last_name) $ putStrLn "warning: --last-name is blank"
    execConfig opts

optionHandler opts@ModeGrep{..}  = do
    putStrLn  $ "Grepping for: " ++ "'" ++ grepString ++ "'"
    -- when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    -- when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    execGrep opts

optionHandler opts@ModeReplace{..}  = do
    putStrLn  $ "Replacing for: " ++ "'" ++ searchString ++ "'"
    -- when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    -- when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    execReplace opts

optionHandler opts@Mode2{..}  = do
    when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    exec opts

optionHandler opts@ModeApply{..}  = do
    execApply opts


-- Executors for the subcommands:
exec :: MyOptions -> IO ()
exec opts@Mode2{..} = putStrLn $ "You are " ++ show height ++ "cm tall, and weigh " ++ show weight ++ "kg!"


--execReplace :: MyOptions -> IO ()
--execReplace opts@ModeReplace{..} = do
--    putStrLn $ "Replacing " ++ show searchString ++ " with " ++ show replaceString ++ ""








