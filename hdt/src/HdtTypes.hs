
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
    files1 <- globDir1 (compile "*.hs") "/home/michael/hw/python-devtools/hdt/src/"
    --files1 <- []
    return [
        Project { projectName ="Project1",isActive=True, rootDir="dir1/", srcFiles=files1 },
        Project { projectName ="Project2",isActive=False,rootDir="dir2/", srcFiles=["File3","File4"] },
        Project { projectName ="Project3",isActive=True, rootDir="dir3/", srcFiles=["File4","File5"] }
        ]
    --where files1 = globDir1 (compile "*.hs") "/home/michael/hw/python-devtools/hdt/src/*.hs"
