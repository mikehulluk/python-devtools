
module HdtTypes where

data Project = Project {
     projectName :: String
    ,rootDir :: String
    ,srcFiles :: [String]
    , isActive :: Bool
}



getAllProjectConfigs :: [Project]
getAllProjectConfigs = [
    Project { projectName ="Project1",isActive=True, rootDir="dir1/", srcFiles=["File1","File2"] },
    Project { projectName ="Project2",isActive=False,rootDir="dir2/", srcFiles=["File3","File4"] },
    Project { projectName ="Project3",isActive=True, rootDir="dir3/", srcFiles=["File4","File5"] }
    ]
