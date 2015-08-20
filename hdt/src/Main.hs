{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}


import HdtTypes
import CmdLineOpts
import ActionGrep

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Data.List
import Data.Text.Format

import System.Console.ANSI
import Control.Monad
import Control.Exception

import Text.Regex.Posix   -- for regular expressions
import Text.Regex.Posix.String

--colorStrLn :: ColorIntensity -> Color -> String -> IO ()
--colorStrLn fgi fg str = do
--  setSGR [SetColor Foreground fgi fg]
--  putStr str
--  setSGR []
--  putStrLn ""

--colorStrLn :: ColorIntensity -> Color -> String -> IO ()
--colorStrLn fgi fg bgi bg str = do
--  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
--  putStr str
--  setSGR []
--  putStrLn ""

--main = do
--  colorStrLn Vivid White Vivid Red "This is red on white."
--  colorStrLn Vivid White Dull Blue "This is white on blue."
--  colorStrLn Vivid Green Dull Black "This is green on black."
--  colorStrLn Vivid Yellow Dull Black "This is yellow on black."
--  colorStrLn Dull Black Vivid Blue "This is black on light blue."












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



-- Executors for the subcommands:
exec :: MyOptions -> IO ()
exec opts@Mode2{..} = putStrLn $ "You are " ++ show height ++ "cm tall, and weigh " ++ show weight ++ "kg!"


execReplace :: MyOptions -> IO ()
execReplace opts@ModeReplace{..} = putStrLn $ "Replacing " ++ show searchString ++ " with " ++ show replaceString ++ ""


-- Printing to the screen:
-- ^^^^^^^^^^^^^^^^^^^^^^^^
summariseProject :: Project -> String
summariseProject project = unlines [
     projectName project ++ ": " ++ (if (isActive project) then "<active>" else "<inactive>")
    ,"  Root:" ++  (rootDir project)
    ,"  Files:" ++ (unwords $ srcFiles project)
    ]

summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do
        setSGR [SetColor Foreground Vivid textcolor]
        putStrLn $ summariseProject project
        setSGR []
        return ()
    where textcolor = if isActive project then Green else Red

execConfig :: MyOptions -> IO ()
execConfig opts@ModeConfig{..} = do
    projects <- getAllProjectConfigs
    forM projects summariseProjectConsole
    return ()






