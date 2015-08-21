
module HdtTypes where

import System.FilePath.Glob

data File = File {
      filename :: String 
    , isClean  :: Bool 
    , tags :: [String]
    }

data Project = Project {
      projectName :: String
    , rootDir :: String
    , isActive :: Bool
    , _rawSrcFiles :: [String] -- deprecated
}


srcFiles :: Project -> [File]
srcFiles  project = map _buildFile (_rawSrcFiles project)
    where _buildFile s = File {filename=s, isClean=True, tags=[]}



getAllProjectConfigs :: IO [Project] 
getAllProjectConfigs = do
    files1 <- globDir1 (compile "*.hs") "/home/mike/dev/python-devtools/hdt/src/"
    files2 <- globDir1 (compile "*.hs") "/home/michael/hw/python-devtools/hdt/src/"
    return [
        Project { projectName ="Project1",isActive=True, rootDir="dir1/",  _rawSrcFiles= (files1++files2) },
        Project { projectName ="Project2",isActive=False,rootDir="dir2/",  _rawSrcFiles=["File3","File4"] },
        Project { projectName ="Project3",isActive=False, rootDir="dir3/", _rawSrcFiles=["File4","File5"] }
        ]


--isFileClean
