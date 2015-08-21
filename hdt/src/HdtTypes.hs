
module HdtTypes where

import System.FilePath.Glob


data Project = Project {
     projectName :: String
    ,rootDir :: String
    ,srcFiles :: [String]
    , isActive :: Bool
}



getAllProjectConfigs :: IO [Project] 
getAllProjectConfigs = do
    files1 <- globDir1 (compile "*.hs") "/home/mike/dev/python-devtools/hdt/src/"
    files2 <- globDir1 (compile "*.hs") "/home/michael/hw/python-devtools/hdt/src/"
    return [
        Project { projectName ="Project1",isActive=True, rootDir="dir1/", srcFiles=files1++files2 },
        Project { projectName ="Project2",isActive=False,rootDir="dir2/", srcFiles=["File3","File4"] },
        Project { projectName ="Project3",isActive=False, rootDir="dir3/", srcFiles=["File4","File5"] }
        ]

data File = File {
      filename :: String 
    , isClean  :: Bool 
    , tags :: [String]
    }

--isFileClean
