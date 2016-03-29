{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}


module ActionTags where


import HdtTypes
import CmdLineOpts
import HdtProject
import Data.List
import System.Process



execTags :: MyOptions -> IO ()
execTags _opts@Tags{..} = do
    -- Get the active projects
    activeProjects <- getActiveProjects
    
    srcfiles <- mapM srcFiles activeProjects

    let all_srcfiles = map filename ( concat( srcfiles) )
    createTagsFile all_srcfiles 
    return ()
execTags _ = error "execTags() called with wrong option type"

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
