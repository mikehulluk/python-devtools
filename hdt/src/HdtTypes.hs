
module HdtTypes where

data Project = Project {
     projectName :: String
    ,rootDir :: String
    ,src_files :: [String]
}
