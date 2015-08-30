{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module CmdLineOpts(
    MyOptions(ModeConfig,ModeGrep,Repl,ModeApply,ModeDrop), 
    modeConfig,
        first_name, last_name,
    modeGrep,
        grepString, ignoreCase, word, count, lineNumbers, doEdit, nContextLines,
    modeReplace,
        searchString, replaceString, doWordRegex, noApply,
    modeApply,
    modeDrop,
    myModes
    )
where

import System.Console.CmdArgs




data MyOptions =
    ModeConfig   { first_name :: String
                 , last_name :: String
                 }
    | ModeGrep {  grepString    :: String
                , ignoreCase    :: Bool
                , word          :: Bool
                , count         :: Bool
                , lineNumbers   :: Bool
                , doEdit        :: Bool
                , nContextLines :: Int
            }
    | Repl {
          searchString   :: String
        , replaceString  :: String
        , ignoreCase     :: Bool
        , doWordRegex    :: Bool
        , noApply        :: Bool
        }
    | ModeApply { }
    | ModeDrop { }
    deriving (Data, Typeable, Show, Eq)



modeConfig :: MyOptions
modeConfig = ModeConfig
    { first_name = "FIRSTNAME" &= help "your first name"
    , last_name = "LASTNAME" &= help "your last name"
    }
    &= details  [ "Examples:"
                , "Blah blah blah."
                ]


modeGrep :: MyOptions
modeGrep = ModeGrep
    {
      grepString    = def   &= argPos 0 &= typ "GREPSTRING"
    , ignoreCase    = True  &= help "ignoreCase"
    , word          = False &= name "jkl" &= help "doWordRegex"
    , count         = False &= help "doCount"
    , lineNumbers   = False  &= help "doLineNumber"
    , doEdit        = False &= help "doEdit"
    , nContextLines = 0      &= name "contextlines" &= help "nContextLines"
    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeReplace :: MyOptions
modeReplace = Repl
    {
          searchString  = def &= argPos 0 &= typ "GREPSTRING"
        , replaceString = def &= argPos 1 &= typ "replaceString"
        , ignoreCase    = def &= help "GREPSTRING"
        , doWordRegex   = def &= help "GREPSTRING"
        , noApply       = False &= help "GREPSTRING"

    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeApply :: MyOptions
modeApply = ModeApply
    { }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeDrop :: MyOptions
modeDrop = ModeDrop
    { }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]



myModes :: Mode (CmdArgs MyOptions)
myModes = cmdArgsMode $ modes [modeConfig,  modeGrep, modeReplace, modeApply, modeDrop]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME


_PROGRAM_NAME :: String
_PROGRAM_VERSION :: String
_PROGRAM_INFO :: String
_PROGRAM_ABOUT :: String
_COPYRIGHT :: String


_PROGRAM_NAME = "myProg"
_PROGRAM_VERSION = "0.1.2.3"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "a sample CmdArgs program for you tinkering pleasure"
_COPYRIGHT = "(C) Your Name Here 2011"
