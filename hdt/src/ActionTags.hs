{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}


module ActionTags where


import HdtTypes
import CmdLineOpts
import HdtProject
import HdtFilePatchStack

import Data.List
import System.Cmd



execTags :: MyOptions -> IO ()
execTags opts@Tags{..} = do
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects
    srcfiles <- mapM srcFiles activeProjects

    let all_srcfiles = map filename ( concat( srcfiles) )
    createTagsFile all_srcfiles 
    return ()
    -- mapM_ dropOutstandingPatchs $ concat srcfiles

outputTagFile :: String
outputTagFile = "/home/michael/.tags"

createTagsFile :: [String] -> IO()
createTagsFile fnames = do
    let file_str = intercalate " " fnames
    -- putStrLn $ show $ file_str
    let cmd_str = "ctags -f " ++ outputTagFile ++ " " ++ file_str
    putStrLn $ show $ cmd_str
    system cmd_str
    return ()
