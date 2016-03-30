
module HdtConstants where

import System.FilePath ((</>), (<.>))
import System.Directory

import MHUtil (expandUser)


filenameConfig :: IO String
filenameConfig = expandUser "~/.hdtrc"

getHDTConfigPath :: IO String
getHDTConfigPath  = do
    homeDir <- getHomeDirectory
    let hdtPath = homeDir </> ".hdt/"
    createDirectoryIfMissing True hdtPath
    return hdtPath


outputTagFile :: IO String
outputTagFile = expandUser "~/.hdt-tags" 
