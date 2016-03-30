{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}


module ActionTags where


import HdtTypes
import CmdLineOpts
import HdtProject
import Data.List
import System.Process
import Text.Printf
--import Control.Monad
import Control.Applicative

import HdtConstants


execTags :: MyOptions -> IO ()
execTags _opts@Tags{..} = do
    srcfiles <- allActiveSrcFiles
    createTagsFile $ filename <$> srcfiles
    return ()
    
execTags _ = error "execTags() called with wrong option type"



createTagsFile :: [String] -> IO()
createTagsFile fnames = do
    outputTagFile' <- outputTagFile
    let file_str = intercalate " " fnames
    let cmd_str = printf "ctags -f %s %s" outputTagFile' file_str
    putStrLn $ cmd_str
    system cmd_str
    return ()
