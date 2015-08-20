-- multiMode.hs
-- License: PUBLIC DOMAIN
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}


import HdtTypes

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Data.List
import Data.Text.Format

import System.Console.ANSI
import Control.Monad
import Control.Exception

import Text.Regex.Posix   -- for regular expressions
import Text.Regex.Posix.String

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
    execConfig opts

optionHandler opts@ModeGrep{..}  = do
    putStrLn  $ "Grepping for: " ++ "'" ++ grepString ++ "'"
    -- when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    -- when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    execGrep opts

optionHandler opts@ModeReplace{..}  = do
    putStrLn  $ "Replacing for: " ++ "'" ++ searchString ++ "'"
    -- when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    -- when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    execReplace opts

optionHandler opts@Mode2{..}  = do
    when (height == 0.0) $ putStrLn "warning: --height is 0.0"
    when (weight == 0.0) $ putStrLn "warning: --weight is 0.0"
    exec opts



-- Executors for the subcommands:
exec :: MyOptions -> IO ()
exec opts@Mode2{..} = putStrLn $ "You are " ++ show height ++ "cm tall, and weigh " ++ show weight ++ "kg!"


execReplace :: MyOptions -> IO ()
execReplace opts@ModeReplace{..} = putStrLn $ "Replacing " ++ show searchString ++ " with " ++ show replaceString ++ ""


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

execConfig :: MyOptions -> IO ()
execConfig opts@ModeConfig{..} = do
    projects <- getAllProjectConfigs
    forM projects summariseProjectConsole
    return ()




-- Grepping:
-- ^^^^^^^^^^^^^^^^^^^^^^^^
execGrep :: MyOptions -> IO ()
execGrep opts@ModeGrep{..} = do
    putStrLn $ "Grep-time!"
    -- Get the active projects
    projects <- getAllProjectConfigs
    let activeProjects = filter isActive projects

    -- Compile the regular expression:
    regexCompRes <- compile defaultCompOpt execBlank grepString
    case regexCompRes of
        Left wrapError -> do
            putStrLn "Unable to compile"
            return ()
        Right compiledRegex -> do
            putStrLn "Compiled OK"

            forM activeProjects (grepProject compiledRegex opts)
            return ()

grepProject ::  Regex -> MyOptions -> Project -> IO ()
grepProject compiledRegex opts project = do
    forM (srcFiles project) (execGrepFile compiledRegex opts)
    return ()


execGrepFile :: Regex -> MyOptions -> String -> IO ()
execGrepFile compiledRegex opts filename= do
    contents <- catch (readFile filename)
        (\e -> do
                  let err = show (e :: IOException)
                  putStrLn ("Warning: Couldn't open " ++ filename ++ ": " ++ err)
                  return "")

    let ls = lines contents
    let ils = zip [0..] ls
    --forM ils (grepLineIO compiledRegex ls)

    -- Find all the matching lines:
    grepLinesAll <- mapM (grepLine compiledRegex) ils
    let grepLines = concat(grepLinesAll)
    putStrLn $ "Filename:" ++ filename
    putStrLn $ intercalate "\n" (map show grepLines)

    putStrLn $ "\n\n\n"

    let nContextLines = 3
    -- Add in context lines
    let nLinesFile = (length ls)
    let linesIncludingContext = addContextLinesNew nContextLines nLinesFile grepLines
    putStrLn $ intercalate "\n" (map show linesIncludingContext)

    putStrLn $ "\n\n\n"

    let groupedLinesPrinted = groupLines linesIncludingContext
    

    putStrLn $ "\n\n\n"

    mapM (printGroupLines ls) groupedLinesPrinted
    -- let linesIncludingContextClean = grepLinesWithContextClean nLinesFile linesIncludingContext
    -- putStrLn $ intercalate "\n" (map show linesIncludingContextClean)


    return ()


data GrepLineMatch = GrepLineMatch (String, String, String, [String]) Int deriving (Data, Typeable, Show, Eq)
data GrepLinePrinted = MatchLine GrepLineMatch | ContextLine Int deriving (Data, Typeable, Show, Eq)

instance Ord GrepLinePrinted where
    compare a b = compare (grepLineNum a) (grepLineNum b)




grepLine :: Regex -> (Int, String) -> IO [GrepLineMatch]
grepLine compiledRegex (lineNo, line) = do
    result <- regexec compiledRegex line
    case result of
        Left (returnCode, errorStr) -> return []
        Right match -> case match of
            Nothing -> return []
            Just (pre, matched, post,subexpression) -> return [ GrepLineMatch (pre, matched, post,subexpression) lineNo ]



grepLineNum :: GrepLinePrinted -> Int
grepLineNum (MatchLine m0) = l where (GrepLineMatch _ l) = m0
grepLineNum (ContextLine l0) = l0

addContextLinesNew :: Int -> Int -> [GrepLineMatch] -> [GrepLinePrinted]
addContextLinesNew nContextLines nLinesFile grepLines =
    sort printedLines 
    where linesWithGrep = map (grepLineNum . MatchLine) grepLines
          possibleContextLines = nub  $ concat [ [l-nContextLines..l+nContextLines] | l <- linesWithGrep]
          contextLines = [i | i<- possibleContextLines, not( i `elem` linesWithGrep), i>=0, i<nLinesFile]
          printedLines = (map MatchLine grepLines ) ++ [ ContextLine i | i <- contextLines]


groupLines :: [GrepLinePrinted] -> [ [GrepLinePrinted] ]
groupLines x = [x, x] 


printGrepLineNo :: (Maybe Int) -> Int -> String
printGrepLineNo Nothing _ = ""
printGrepLineNo (Just lineNumberWidth) x = x_out --(show x) ++ ":"
    where suffix = ": "
          x_str = (show x) ++ ": "
          padding_needed = (lineNumberWidth+ (length suffix)) - (length x_str) + 1
          padding = ( concat $ (replicate padding_needed " ") )
          x_out = padding ++ x_str

printGrepLine :: [String] -> (Maybe Int) -> GrepLinePrinted  -> IO ()
printGrepLine allLines lineNumberWidth (MatchLine m) = do
    putStr $ printGrepLineNo lineNumberWidth lineNo
    --putStr $ (show lineNo) ++ " :   "
    setSGR [SetColor Foreground Dull White]
    putStr $ pre
    setSGR [SetColor Foreground Vivid Green]
    putStr $ matched
    setSGR [SetColor Foreground Dull White]
    putStr $ post ++ "\n"
    setSGR []
    where GrepLineMatch (pre, matched, post,subexpression) lineNo = m



printGrepLine allLines lineNumberWidth (ContextLine lineNo)  = do 
    putStr $ printGrepLineNo lineNumberWidth lineNo
    setSGR [SetColor Foreground Dull White]
    putStr $ (allLines!!lineNo)
    putStr $ "\n"
    setSGR []


--grepLinePrintMatch :: (String, String, String, [String]) -> Int -> [String] -> IO()
--grepLinePrintMatch (pre, matched, post,subexpression) lineNo allLines = do
--    putStr $ (show lineNo) ++ " : "
--    setSGR [SetColor Foreground Dull White]
--    putStr $ pre
--    setSGR [SetColor Foreground Vivid Green]
--    putStr $ matched
--    setSGR [SetColor Foreground Dull White]
--    putStr $ post ++ "\n"
--    setSGR []


printGroupLines :: [String] -> [GrepLinePrinted] -> IO ()
printGroupLines allLines lines = do
    putStrLn "Printing.."
    let lineNumberWidth = Just 3
    mapM (printGrepLine allLines lineNumberWidth) lines
    return ()




--Ord GrepLinePrinted












-- -- Functions for adding context lines:
-- buildContextLineList :: Int -> Int -> GrepLineMatch -> [GrepLinePrinted]
-- buildContextLineList linesContext lineNo m =
--     [ContextLine (lineNo-i) | i<- reverse [1..linesContext] ] ++
--     [MatchLine m] ++
--     [ContextLine (lineNo+i)| i<-[1..linesContext] ]
--
-- addContextLines :: Int -> [GrepLineMatch] -> [GrepLinePrinted]
-- addContextLines linesContext ([])         = []
-- addContextLines linesContext (mprev:m:[])       = buildContextLineList linesContext lineNo m
--     where GrepLineMatch _ lineNo = m
--
-- addContextLines linesContext (mprev:m:mnext:matches) = (buildContextLineList linesContext lineNo m) ++ (addContextLines linesContext m:next:matches)
--     where GrepLineMatch _ lineNo = undefined --m
--
--
-- grepLinesWithContextClean :: Int -> [GrepLinePrinted] -> [GrepLinePrinted]
-- grepLinesWithContextClean nLinesMax lines = lines
--grepLinesWithContextClean nLinesMax [] = []
--grepLinesWithContextClean nLinesMax [ContextLine m0] = [ContextLine m0]
--grepLinesWithContextClean nLinesMax [MatchLine m0] = [MatchLine m0]
--
--grepLinesWithContextClean nLinesMax (MatchLine   m0 : MatchLine m1   : ms) = [MatchLine m0] ++ (grepLinesWithContextClean nLinesMax (MatchLine m1:ms))
--
--
--grepLinesWithContextClean nLinesMax (ContextLine m0 : MatchLine m1   : ms) = [ContextLine m0] ++ (grepLinesWithContextClean nLinesMax (MatchLine m1:ms))
--grepLinesWithContextClean nLinesMax (MatchLine   m0 : ContextLine m1 : ms) = [MatchLine m0] ++ (grepLinesWithContextClean nLinesMax (ContextLine m1:ms))
--
--grepLinesWithContextClean nLinesMax (ContextLine l0 : ContextLine l1 : ms)
--    | l0 < l1   = [ContextLine l0] ++ (grepLinesWithContextClean nLinesMax (ContextLine l1:ms))     -- Drop sequential contextlines if the numbering is not sequential
--    | otherwise = grepLinesWithContextClean nLinesMax (ContextLine l0:ms)



-- data GrepLineMatch = GrepLineMatch (String, String, String, [String]) Int deriving (Data, Typeable, Show, Eq)
-- data GrepLinePrinted = MatchLine GrepLineMatch | ContextLine Int deriving (Data, Typeable, Show, Eq)



--grepLineIO :: Regex -> [String] -> (Int, String) -> IO ()
--grepLineIO compiledRegex allLines (lineNo, line) = do
--
--    result <- regexec compiledRegex line
--    case result of
--        Left (returnCode, errorStr) -> putStrLn $ "Unexpected error while matching: " ++ errorStr
--        Right match -> case match of
--            Nothing -> return ()
--            Just (pre, matched, post,subexpression) -> do
--                grepLinePrintMatch (pre, matched, post,subexpression) lineNo allLines



--grepLinePrintMatch :: (String, String, String, [String]) -> Int -> [String] -> IO()
--grepLinePrintMatch (pre, matched, post,subexpression) lineNo allLines = do
--    putStr $ (show lineNo) ++ " : "
--    setSGR [SetColor Foreground Dull White]
--    putStr $ pre
--    setSGR [SetColor Foreground Vivid Green]
--    putStr $ matched
--    setSGR [SetColor Foreground Dull White]
--    putStr $ post ++ "\n"
--    setSGR []
--


