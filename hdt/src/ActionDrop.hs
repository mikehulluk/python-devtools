
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionDrop(execDrop) where

import HdtTypes
import CmdLineOpts
import HdtProject
import MHUtil
import System.Console.CmdArgs
--import System.Environment (getArgs, withArgs)
--import System.Exit
import Control.Monad
import Data.List
--import Data.Text.Format



import System.Console.ANSI
import Control.Exception

import Text.Regex.Posix   -- for regular expressions
import Text.Regex.Posix.String
import HdtFilePatchStack



execDrop :: MyOptions -> IO ()
execDrop opts@ModeDrop{..} = do
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    srcfiles <- mapM srcFiles activeProjects
    let s = concat srcfiles

    mapM_ dropOutstandingPatchs s



