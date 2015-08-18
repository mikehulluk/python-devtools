-- multiMode.hs
-- License: PUBLIC DOMAIN
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}


import HdtTypes

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)

import System.Console.ANSI
import Control.Monad

colorStrLn :: ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg str = do
  setSGR [SetColor Foreground fgi fg]
  putStr str
  setSGR []
  putStrLn ""

--colorStrLn :: ColorIntensity -> Color -> String -> IO ()
--colorStrLn fgi fg bgi bg str = do
--  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
--  putStr str
--  setSGR []
--  putStrLn ""

--main = do
--  colorStrLn Vivid White Vivid Red "This is red on white."
--  colorStrLn Vivid White Dull Blue "This is white on blue."
--  colorStrLn Vivid Green Dull Black "This is green on black."
--  colorStrLn Vivid Yellow Dull Black "This is yellow on black."
--  colorStrLn Dull Black Vivid Blue "This is black on light blue."











data MyOptions =
    ModeConfig   { first_name :: String
            , last_name :: String
            }
    |
    Mode2   { height :: Double
            , weight :: Double
            } 
    |
    ModeGrep { height :: Double
            , weight :: Double
            } deriving (Data, Typeable, Show, Eq)

mode1 :: MyOptions
mode1 = ModeConfig
    { first_name = "FIRSTNAME" &= help "your first name"
    , last_name = "LASTNAME" &= help "your last name"
    }
    &= details  [ "Examples:"
                , "Blah blah blah."
                ]

mode2 :: MyOptions
mode2 = Mode2
    { height = def &= help "your height, in centimeters"
    , weight = def &= help "your weight, in kilograms"
    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]

modeGrep :: MyOptions
modeGrep = ModeGrep
    { height = def &= help "your height, in centimeters"
    , weight = def &= help "your weight, in kilograms"
    }
    &= details  [ "Examples:"
                , "Blah blah blah again."
                ]


myModes :: Mode (CmdArgs MyOptions)
myModes = cmdArgsMode $ modes [mode1, mode2, modeGrep]
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

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
    optionHandler opts

optionHandler :: MyOptions -> IO ()
optionHandler opts@ModeConfig{..}  = do
    when (null first_name) $ putStrLn "warning: --first-name is blank"
    when (null last_name) $ putStrLn "warning: --last-name is blank"
    exec opts
optionHandler opts@Mode2{..}  = do
    when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    exec opts

optionHandler opts@ModeGrep{..}  = do
    when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    exec opts




-- Executors for the subcommands:



-- Printing to the screen:
-- ^^^^^^^^^^^^^^^^^^^^^^^^
summariseProject :: Project -> String
summariseProject project = unlines [
     projectName project ++ ": " ++ (if (isActive project) then "<active>" else "<inactive>")
    ,"  Root:" ++  (rootDir project)
    ,"  Files:" ++ (unwords $ srcFiles project)
    ]

summariseProjectConsole :: Project -> IO ()
summariseProjectConsole project = do
        setSGR [SetColor Foreground Vivid textcolor]
        putStrLn $ summariseProject project
        setSGR []
        return ()
    where textcolor = if isActive project then Green else Red

exec :: MyOptions -> IO ()
exec opts@ModeConfig{..} = do
    projects <- getAllProjectConfigs
    forM projects summariseProjectConsole
    return ()




exec opts@ModeGrep{..} = putStrLn $ "Grep-time!"



exec opts@Mode2{..} = putStrLn $ "You are " ++ show height ++ "cm tall, and weigh " ++ show weight ++ "kg!"



