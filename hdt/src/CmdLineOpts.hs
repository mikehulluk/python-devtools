{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module CmdLineOpts(
    MyOptions(Config,Grep,Repl,Apply,Drop, Tags, GetRootDir), 
    modeConfig,
        --first_name, last_name,
    modeGrep,
        grepString, ignoreCase, word, count, lineNumbers, doEdit, nContextLines,
    modeReplace,
        searchString, replaceString, doWordRegex, noApply,
    modeApply,
        acceptAll,
    modeDrop,
    modeTags,
    modeGetRootDir,
    myModes
    )
where

import System.Console.CmdArgs




data MyOptions =
    Config   {  
         detailed     :: Bool
        --,last_name    :: String
        }
    | Grep {      
         grepString    :: String
        ,ignoreCase    :: Bool
        ,word          :: Bool
        ,count         :: Bool
        ,lineNumbers   :: Bool
        ,doEdit        :: Bool
        ,nContextLines :: Int
        }
    | Repl {
         searchString   :: String
        ,replaceString  :: String
        ,ignoreCase     :: Bool
        ,doWordRegex    :: Bool
        ,noApply        :: Bool
        }
    | Apply {
        acceptAll       :: Bool
        }
    | Drop { }
    | Tags { }
    | GetRootDir {}
    deriving (Data, Typeable, Show, Eq)



modeConfig :: MyOptions
modeConfig = Config
    { 
        detailed = False &= help "Display detailed information"
    }
    &= details  [ "Examples:",
                  "  hdt config\n", 
                  "  hdt config --detailed\n"
                ]


modeGrep :: MyOptions
modeGrep = Grep
    {
      grepString    = def   &= argPos 0 &= typ "GREPSTRING"
    , ignoreCase    = True  &= help "ignoreCase"
    , word          = False &= name "jkl" &= help "doWordRegex"
    , count         = False &= help "doCount"
    , lineNumbers   = False &= help "doLineNumber"
    , doEdit        = False &= help "doEdit"
    , nContextLines = 0     &= name "contextlines" &= help "nContextLines"
    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeReplace :: MyOptions
modeReplace = Repl
    {
          searchString  = def &= argPos 1 &= typ "Search-String"
        , replaceString = def &= argPos 2 &= typ "Replace-String"
        , ignoreCase    = def &= help "Case insensitive search"
        , doWordRegex   = def &= help "Search for word"
        , noApply       = False &= help "Don't apply the replace. This allows a series of operations to be batched up"

    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeApply :: MyOptions
modeApply = Apply
    { 
        acceptAll = False &= help "No Gui, just accept all patches"
    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeDrop :: MyOptions
modeDrop = Drop
    { }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]


modeTags :: MyOptions
modeTags = Tags
    { }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeGetRootDir :: MyOptions
modeGetRootDir = GetRootDir
    { }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

myModes :: Mode (CmdArgs MyOptions)
myModes = cmdArgsMode $ modes [modeConfig,  modeGrep, modeReplace, modeApply, modeDrop, modeTags, modeGetRootDir]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME


_PROGRAM_NAME    :: String
_PROGRAM_VERSION :: String
_PROGRAM_INFO    :: String
_PROGRAM_ABOUT   :: String
_COPYRIGHT       :: String


_PROGRAM_NAME = "hdt"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Haskell-DevTools: a collection of tools for dealing with text files from the commandline"
_COPYRIGHT = "(C) Mike Hull 2016. (mikehulluk@gmail.com)"
