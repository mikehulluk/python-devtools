{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module CmdLineOpts where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)



data MyOptions =
    ModeConfig   { first_name :: String
                 , last_name :: String
                 }
    |
    Mode2   { height :: Double
            , weight :: Double
            }
    |
    ModeGrep {    grepString    :: String
                , ignoreCase    :: Bool
                , doWordRegex   :: Bool
                , doCount       :: Bool
                , doLineNumber  :: Bool
                , doEdit        :: Bool
                , nContextLines :: Int
            }
    |
    ModeReplace {
          searchString  :: String
        , replaceString  :: String
        , ignoreCase  :: Bool
        , doWordRegex :: Bool

    }
    deriving (Data, Typeable, Show, Eq)



modeConfig :: MyOptions
modeConfig = ModeConfig
    { first_name = "FIRSTNAME" &= help "your first name"
    , last_name = "LASTNAME" &= help "your last name"
    }
    &= details  [ "Examples:"
                , "Blah blah blah."
                ]

mode2 :: MyOptions
mode2 = Mode2
    {
      height = def &= help "your height, in centimeters"
    , weight = def &= help "your weight, in kilograms"
    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeGrep :: MyOptions
modeGrep = ModeGrep
    {
      grepString    = def &= argPos 0 &= typ "GREPSTRING"
    , ignoreCase    = def &= help "ignoreCase"
    , doWordRegex   = def &= help "doWordRegex"
    , doCount       = def &= help "doCount"
    , doLineNumber  = def &= help "doLineNumber"
    , doEdit        = def &= help "doEdit"
    , nContextLines = def &= help "nContextLines"
    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeReplace :: MyOptions
modeReplace = ModeReplace
    {
          searchString  = def &= argPos 0 &= typ "GREPSTRING"
        , replaceString = def &= argPos 1 &= typ "replaceString"
        , ignoreCase    = def &= help "GREPSTRING"
        , doWordRegex   = def &= help "GREPSTRING"

    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

myModes :: Mode (CmdArgs MyOptions)
myModes = cmdArgsMode $ modes [modeConfig, mode2, modeGrep, modeReplace]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "myProg"
_PROGRAM_VERSION = "0.1.2.3"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "a sample CmdArgs program for you tinkering pleasure"
_COPYRIGHT = "(C) Your Name Here 2011"
